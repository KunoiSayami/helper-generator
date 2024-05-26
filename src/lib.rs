mod enchanted;

use once_cell::sync::Lazy;
use proc_macro2::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Attribute, DataEnum, Fields, Variant};

static WORD_RE: Lazy<fancy_regex::Regex> =
    Lazy::new(|| fancy_regex::Regex::new(r".(?:[^A-Z0-9]+|[A-Z0-9]*)(?![^A-Z0-9])").unwrap());
static WORD_RE_ERROR: Lazy<String> = Lazy::new(|| "ERROR_PLEASE_REPORT".to_string());

type TyType = TokenStream;
type IdentType = Ident;

#[derive(Clone)]
enum FieldsType {
    Named(Vec<(IdentType, TyType)>),
    Unnamed(Vec<TyType>),
    None,
}

impl FieldsType {
    fn to_arg_def(&self, span: Span) -> TokenStream {
        match self {
            FieldsType::Named(v) => {
                let updated = v
                    .iter()
                    .map(|(ident, ty)| {
                        quote!(
                            #ident: #ty
                        )
                    })
                    .collect::<Vec<_>>();
                quote!(#(#updated), *)
            }
            FieldsType::Unnamed(v) => {
                let i = (0..v.len())
                    .map(|i| Ident::new(&format!("input{}", i), span))
                    .collect::<Vec<_>>();
                quote!(#(#i: #v), *)
            }
            FieldsType::None => TokenStream::new(),
        }
    }

    fn to_arg(&self, span: Span) -> TokenStream {
        match self {
            FieldsType::Named(v) => {
                let idents = v
                    .iter()
                    .map(|(ident, _)| quote!(#ident))
                    .collect::<Vec<_>>();
                quote!({#(#idents), *})
            }
            FieldsType::Unnamed(v) => {
                let i = (0..v.len())
                    .map(|i| (Ident::new(&format!("input{}", i), span)))
                    .collect::<Vec<_>>();
                quote!((#(#i), *))
            }
            FieldsType::None => TokenStream::new(),
        }
    }

    fn enum_arg_def(&self, return_type: Option<TokenStream>) -> TokenStream {
        match self {
            FieldsType::Named(v) => {
                let mut idents = v
                    .iter()
                    .map(|(ident, ty)| {
                        quote!(
                            #ident: #ty
                        )
                    })
                    .collect::<Vec<_>>();
                if let Some(ret) = return_type {
                    let ident = Ident::new("__private_sender", ret.span());
                    idents.push(quote! ( #ident: tokio::sync::oneshot::Sender< #ret >));
                }
                quote!({#(#idents), *})
            }
            FieldsType::Unnamed(v) => {
                if let Some(ret) = return_type {
                    let mut v = v.clone();
                    v.push(quote! (tokio::sync::oneshot::Sender< #ret >));
                    quote!((#(#v), *))
                } else {
                    quote!((#(#v), *))
                }
            }
            FieldsType::None => {
                if let Some(ret) = return_type {
                    quote! {(tokio::sync::oneshot::Sender< #ret >)}
                } else {
                    TokenStream::new()
                }
            }
        }
    }

    fn enchant_arg(&self, span: Span) -> TokenStream {
        let private_receiver = Ident::new("__private_sender", span);
        match self {
            FieldsType::Named(v) => {
                let mut idents = v
                    .iter()
                    .map(|(ident, _)| quote!(#ident))
                    .collect::<Vec<_>>();
                idents.push(quote! {#private_receiver});
                quote!({#(#idents), *})
            }
            FieldsType::Unnamed(v) => {
                let mut i = (0..v.len())
                    .map(|i| (Ident::new(&format!("input{}", i), span)))
                    .collect::<Vec<_>>();
                i.push(private_receiver);
                quote!((#(#i), *))
            }
            FieldsType::None => quote! {(#private_receiver)},
        }
    }
}

enum FieldType {
    Named(IdentType, TyType),
    Unnamed(TyType),
}

impl FieldType {
    fn named(ident: IdentType, ty: TyType) -> Self {
        Self::Named(ident, ty)
    }

    fn unnamed(ty: TyType) -> Self {
        Self::Unnamed(ty)
    }

    fn get_named(self) -> Option<(IdentType, TyType)> {
        match self {
            Self::Named(a, b) => Some((a, b)),
            _ => None,
        }
    }

    fn get_unnamed(self) -> Option<TyType> {
        match self {
            FieldType::Unnamed(s) => Some(s),
            _ => None,
        }
    }
}

impl From<Vec<(IdentType, TyType)>> for FieldsType {
    fn from(value: Vec<(IdentType, TyType)>) -> Self {
        Self::Named(value)
    }
}

impl From<Vec<TyType>> for FieldsType {
    fn from(value: Vec<TyType>) -> Self {
        Self::Unnamed(value)
    }
}

impl Default for FieldsType {
    fn default() -> Self {
        Self::None
    }
}

impl TryFrom<Vec<FieldType>> for FieldsType {
    type Error = ();

    fn try_from(value: Vec<FieldType>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Ok(Self::None);
        }

        let is_named = match value.first() {
            Some(value) => match value {
                FieldType::Named(_, _) => true,
                FieldType::Unnamed(_) => false,
                //FieldType::None => unreachable!("If type is None, should never going to this way"),
            },
            None => unreachable!("Has checked vec is empty"),
        };

        if is_named {
            value
                .into_iter()
                .map(|f| f.get_named())
                .collect::<Option<Vec<_>>>()
                .map(|v| v.into())
        } else {
            value
                .into_iter()
                .map(|f| f.get_unnamed())
                .collect::<Option<Vec<_>>>()
                .map(|v| v.into())
        }
        .ok_or(())
    }
}

impl TryFrom<&syn::Fields> for FieldsType {
    type Error = syn::Error;
    fn try_from(fields: &syn::Fields) -> Result<Self, Self::Error> {
        let span = fields.span();
        let fs: Vec<FieldType> = match fields {
            Fields::Named(fields) => {
                //fields.named.iter().map(|field| )
                fields
                    .named
                    .iter()
                    .map(|field| {
                        Ok(FieldType::named(
                            field.ident.clone().ok_or_else(|| {
                                syn::Error::new_spanned(field, "Field should have a name")
                            })?,
                            field.ty.to_token_stream(),
                        ))
                    })
                    .collect::<syn::Result<Vec<FieldType>>>()?
            }
            Fields::Unnamed(fields) => fields
                .unnamed
                .iter()
                .map(|field| FieldType::unnamed(field.ty.to_token_stream()))
                .collect::<Vec<_>>(),
            Fields::Unit => Vec::new(),
        };

        let fs = fs.try_into();
        match fs {
            Ok(f) => Ok(f),
            Err(_) => Err(syn::Error::new(
                span,
                "Type not match, it should never happened",
            )),
        }
    }
}

#[derive(Clone)]
struct EnumDefinition {
    ident: String,
    fields: FieldsType,
}

impl EnumDefinition {
    fn name_into_snake_case(&self) -> String {
        match WORD_RE
            .find_iter(&self.ident)
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(matches) => matches
                .iter()
                .map(|s| s.as_str().to_ascii_lowercase())
                .collect::<Vec<_>>()
                .join("_"),
            Err(_) => WORD_RE_ERROR.clone(),
        }
    }

    fn get_name(&self, span: Span) -> Ident {
        Ident::new(&self.name_into_snake_case(), span)
    }

    fn get_normal_name(&self, span: Span) -> Ident {
        Ident::new(&self.ident, span)
    }

    fn get_name_block(&self, span: Span) -> Ident {
        // TODO: Optimize this
        let block_name = format!("{}_b", self.name_into_snake_case());
        Ident::new(&block_name, span)
    }

    fn fields(&self) -> &FieldsType {
        &self.fields
    }
}

impl TryFrom<&Variant> for EnumDefinition {
    type Error = syn::Error;

    fn try_from(value: &Variant) -> Result<Self, Self::Error> {
        let ident = value.ident.to_string();
        Ok(Self {
            ident,
            fields: FieldsType::try_from(&value.fields)?,
        })
    }
}

/* fn print_fields(data: &DataEnum) {
    let idents: Vec<_> = data.variants.iter().map(|f| &f.ident).collect();
    //let types: Vec<_> = data.variants.iter().map(|f| &f.ty).collect();

   //eprintln!("{:#?}", idents);
    //eprintln!("{:#?}", types);
} */

/* fn check_is_enum(st: &syn::DeriveInput) -> syn::Result<&DataEnum> {
    match st.data {
        syn::Data::Enum(ref data_enum) => Ok(data_enum),
        _ => Err(syn::Error::new_spanned(
            st,
            "Must defined a enum, not struct".to_string(),
        )),
    }
} */

fn generate_function(
    st: &syn::DeriveInput,
    de: &DataEnum,
    block: bool,
    no_async: bool,
) -> syn::Result<TokenStream> {
    let mut ret = TokenStream::new();
    let basic = &st.ident;
    for variant in &de.variants {
        let definition = EnumDefinition::try_from(variant)?;
        let arg_def = definition.fields().to_arg_def(variant.span());
        let arg = definition.fields().to_arg(variant.span());
        let function_name = definition.get_name(variant.span());
        let member = &variant.ident;
        if !no_async {
            let result = quote! {
                pub async fn #function_name (&self, #arg_def) -> std::option::Option<()> {
                    self.sender
                        .send(#basic::#member #arg)
                        .await
                        .ok()
                }
            };
            //eprintln!("{:#?}", result.to_string());
            ret.extend(result);
        }
        if block {
            let function_name = if no_async {
                function_name
            } else {
                definition.get_name_block(variant.span())
            };
            let result = quote! {
                pub fn #function_name (&self, #arg_def) -> std::option::Option<()> {
                    self.sender
                        .blocking_send(#basic::#member #arg)
                        .ok()
                }
            };
            ret.extend(result);
        }
    }
    Ok(ret)
}

pub(crate) fn parse_tokens(token_stream: &TokenStream) -> syn::Result<(bool, bool)> {
    let mut no_async = false;
    let mut block = false;

    for token in token_stream.clone().into_iter() {
        match &token {
            proc_macro2::TokenTree::Ident(ident) => {
                if ident.eq("no_async") {
                    no_async = true;
                } else if ident.eq("block") {
                    block = true;
                } else {
                    return Err(syn::Error::new(ident.span(), "Unrecognized token"));
                }
            }
            _ => continue,
        }
    }
    Ok((block, no_async))
}

pub(crate) fn parse_arguments(attrs: &[Attribute]) -> syn::Result<(bool, bool)> {
    if attrs.is_empty() {
        return Ok((false, false));
    }
    for attr in attrs {
        match &attr.meta {
            syn::Meta::Path(_) => {
                /* return Err(syn::Error::new(
                    attr.span(),
                    "Unimplemented syn::Meta::Path",
                )) */
            }
            syn::Meta::List(list) => {
                if let Some(seg) = list.path.segments.first() {
                    if !seg.ident.eq("helper") {
                        continue;
                    }
                    let (block, no_async) = parse_tokens(&list.tokens)?;
                    if !block && no_async {
                        return Err(syn::Error::new(
                            list.span(),
                            "This code generate `new' function only!",
                        ));
                    }
                    return Ok((block, no_async));
                }
            }
            syn::Meta::NameValue(_) => {
                /* return Err(syn::Error::new(
                    attr.span(),
                    "Unimplemented syn::Meta::NameValue",
                )) */
            }
        }
    }
    Ok((false, false))
}

type GenMemberFn = fn(&syn::DeriveInput, &syn::DataEnum, bool, bool) -> syn::Result<TokenStream>;

pub(crate) fn do_expand(
    st: &syn::DeriveInput,
    replace_function: Option<GenMemberFn>,
) -> syn::Result<TokenStream> {
    /* if !st.data.eq(syn::Data) {
        return Err(syn::Error::new(
            st.span(),
            "Should apply this drive to enum",
        ));
    } */
    let (block, no_async) = parse_arguments(&st.attrs)?;
    //eprintln!("{} {}", block, no_async);
    let data_enum = extract_enum(st)?;
    //print_fields();
    let enum_name = st.ident.to_string();
    let (basic_name, _) = enum_name.rsplit_once("Event").unwrap();

    let helper_receiver_type = format!("{}Receiver", enum_name);
    let helper_receiver_type_indent = syn::Ident::new(&helper_receiver_type, st.ident.span());

    let helper_name = format!("{}Helper", basic_name);
    let helper_name_ident = syn::Ident::new(&helper_name, st.ident.span());

    let enum_ident = &st.ident;

    let member_function = match replace_function {
        Some(func) => func(st, data_enum, block, no_async),
        None => generate_function(st, data_enum, block, no_async),
    }?;

    let ret = quote! {

        #[derive(Clone, Debug)]
        pub struct #helper_name_ident {
            sender: tokio::sync::mpsc::Sender<#enum_ident>
        }

        pub type #helper_receiver_type_indent = tokio::sync::mpsc::Receiver<#enum_ident>;

        impl #helper_name_ident {
            pub fn new(size: usize) -> (Self, #helper_receiver_type_indent) {
                let (a, b) = tokio::sync::mpsc::channel(size);
                (a.into(), b)
            }

            #member_function
        }

        impl From<tokio::sync::mpsc::Sender<#enum_ident>> for #helper_name_ident {
            fn from(value: tokio::sync::mpsc::Sender<#enum_ident>) -> Self {
                Self {
                    sender: value
                }
            }
        }

    };

    Ok(ret)
}

pub(crate) fn early_check(st: &syn::DeriveInput) -> syn::Result<()> {
    match st.data {
        syn::Data::Enum(_) => {
            if !st.ident.to_string().contains("Event") {
                return Err(syn::Error::new(
                    st.ident.span(),
                    "Should contains Event in name",
                ));
            }
            Ok(())
        }
        _ => Err(syn::Error::new_spanned(
            st,
            "Must defined a enum, not struct".to_string(),
        )),
    }
}

pub(crate) fn extract_enum(st: &syn::DeriveInput) -> syn::Result<&DataEnum> {
    match st.data {
        syn::Data::Enum(ref data_enum) => Ok(data_enum),
        _ => unreachable!(),
    }
}

#[proc_macro_derive(Helper, attributes(helper))]
pub fn enum_helper_generator(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    //eprintln!("{:#?}", st.attrs);

    if let Err(e) = crate::early_check(&st) {
        return e.into_compile_error().into();
    }

    match do_expand(&st, None) {
        Ok(token_stream) => {
            //eprintln!("{}", token_stream.to_string());
            token_stream.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
    //eprintln!("{:#?}", item);
}

#[proc_macro]
pub fn oneshot_helper(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let early_st = syn::parse_macro_input!(input as syn::DeriveInput);
    match enchanted::handle_new(early_st) {
        Ok(ret) => ret,
        Err(e) => e.into_compile_error(),
    }
    .into()
}

#[cfg(test)]
mod test {
    use crate::EnumDefinition;

    #[test]
    fn test_snake_case_convert() {
        fn func(input: &str) -> String {
            EnumDefinition {
                ident: input.to_string(),
                fields: super::FieldsType::None,
            }
            .name_into_snake_case()
        }

        assert_eq!(func("GetHTTPResponse"), "get_http_response".to_string());
        assert_eq!(func("CSV"), "csv".to_string());
        assert_eq!(func("IPChecker"), "ip_checker".to_string());
        assert_eq!(func("UserAdd"), "user_add".to_string());
        assert_eq!(
            func("IsHTTPSpecifyASpecicalAdd"),
            "is_http_specify_a_specical_add".to_string()
        );
        assert_eq!(func("IPV4"), "ipv4".to_string())
    }
}
