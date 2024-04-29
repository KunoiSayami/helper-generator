use proc_macro2::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Attribute, DataEnum, Fields, Variant};

type TyType = TokenStream;
type IdentType = Ident;

#[derive(Clone)]
enum FieldsType {
    Named(Vec<(IdentType, TyType)>),
    Unnamed(Vec<TyType>),
    None,
}

impl FieldsType {
    fn into_arg_def(&self, span: Span) -> TokenStream {
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
                    .map(|i| Ident::new(&format!("data{}", i), span))
                    .collect::<Vec<_>>();
                quote!(#(#i: #v), *)
            }
            FieldsType::None => TokenStream::new(),
        }
    }

    fn into_arg(&self, span: Span) -> TokenStream {
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
                    .map(|i| (Ident::new(&format!("data{}", i), span)))
                    .collect::<Vec<_>>();
                quote!((#(#i), *))
            }
            FieldsType::None => TokenStream::new(),
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
        let fs: Vec<FieldType> =
            match fields {
                Fields::Named(fields) => {
                    //fields.named.iter().map(|field| )
                    fields
                        .named
                        .iter()
                        .map(|field| {
                            Ok(FieldType::named(
                                field.ident.as_ref().map(|ident| ident.clone()).ok_or_else(
                                    || syn::Error::new_spanned(field, "Field should have a name"),
                                )?,
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
    fn name_into_underline_type(&self) -> String {
        let mut v = Vec::new();
        for c in self.ident.chars().into_iter() {
            v.push(c.to_lowercase().to_string());
            if v.len() > 1 && (c.is_uppercase() || c.is_digit(10)) {
                v.insert(v.len() - 1, "_".to_string());
            }
        }
        v.into_iter().collect()
    }

    fn get_name(&self, span: Span) -> Ident {
        Ident::new(&self.name_into_underline_type(), span)
    }
    fn get_name_block(&self, span: Span) -> Ident {
        // TODO: Optimize this
        let block_name = format!("{}_b", self.name_into_underline_type());
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

fn check_is_enum(st: &syn::DeriveInput) -> syn::Result<&DataEnum> {
    match st.data {
        syn::Data::Enum(ref data_enum) => Ok(data_enum),
        _ => Err(syn::Error::new_spanned(
            st,
            "Must defined a enum, not struct".to_string(),
        )),
    }
}

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
        let arg_def = definition.fields().into_arg_def(variant.span());
        let arg = definition.fields().into_arg(variant.span());
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

fn parse_tokens(token_stream: &TokenStream) -> syn::Result<(bool, bool)> {
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

fn parse_arguments(attrs: &[Attribute]) -> syn::Result<(bool, bool)> {
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

fn do_expand(st: &syn::DeriveInput) -> syn::Result<TokenStream> {
    /* if !st.data.eq(syn::Data) {
        return Err(syn::Error::new(
            st.span(),
            "Should apply this drive to enum",
        ));
    } */
    let (block, no_async) = parse_arguments(&st.attrs)?;
    //eprintln!("{} {}", block, no_async);
    let data_enum = check_is_enum(st)?;
    //print_fields();
    let enum_name = st.ident.to_string();
    if !enum_name.contains("Event") {
        return Err(syn::Error::new(
            st.ident.span(),
            "Should contains Event in name",
        ));
    }
    let (basic_name, _) = enum_name.rsplit_once("Event").unwrap();

    let helper_receiver_type = format!("{}Receiver", enum_name);
    let helper_receiver_type_indent = syn::Ident::new(&helper_receiver_type, st.ident.span());

    let helper_name = format!("{}Helper", basic_name);
    let helper_name_ident = syn::Ident::new(&helper_name, st.ident.span());

    let enum_ident = &st.ident;

    let member_function = generate_function(st, data_enum, block, no_async)?;

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

    return Ok(ret);
}

#[proc_macro_derive(Helper, attributes(helper))]
pub fn enum_helper_generator(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    //eprintln!("{:#?}", st.attrs);
    match do_expand(&st) {
        Ok(token_stream) => {
            //eprintln!("{}", token_stream.to_string());
            token_stream.into()
        }
        Err(e) => e.to_compile_error().into(),
    }
    //eprintln!("{:#?}", item);
}
