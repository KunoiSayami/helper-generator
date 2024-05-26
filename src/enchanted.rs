use proc_macro2::TokenStream;
use syn::{spanned::Spanned, DataEnum, Variant};

use proc_macro2::Ident;
use quote::{quote, ToTokens};

use crate::{do_expand, extract_enum, EnumDefinition};

/* fn check_is_enum(input: &TokenStream) -> bool {
    for token in input.clone().into_iter() {
        if let proc_macro2::TokenTree::Ident(ident) = token {
            if ident.eq("enum") {
                return true;
            }
        }
    }
    return false;
}
 */
/// Extract visibility and name
/* fn header(input: TokenStream) -> syn::Result<(Vec<TokenTree>, TokenTree, TokenTree)> {
    let mut v = Vec::new();
    let span = input.span();
    let mut iter = input.into_iter();
    while let Some(token) = iter.next() {
        if let proc_macro2::TokenTree::Ident(ident) = &token {
            if ident.eq("pub") {
                v.push(token.clone());
                break;
            }
        }
    }

    while let Some(token) = iter.next() {
        if let proc_macro2::TokenTree::Ident(ident) = &token {
           //eprintln!("ident: {}", token);
            if ident.eq("enum") {
                break;
            }
        }
        v.push(token.clone());
    }
    let name = iter
        .next()
        .ok_or_else(|| syn::Error::new(span, "Should have a name"))?;

    Ok((
        v,
        name,
        iter.next()
            .ok_or_else(|| syn::Error::new(span, "Should have body"))?,
    ))
}

fn parse_group(input: Group) -> syn::Result<TokenStream> {
    for input in input.stream() {
       //eprintln!("{:?}", input);
    }
    Ok(Default::default())
}

fn build_tree(input: TokenStream) -> syn::Result<TokenStream> {
    for token in input.clone().into_iter() {
       //eprintln!("{:?}", token);
    }
    if !check_is_enum(&input) {
        return Err(syn::Error::new_spanned(input, "Not enum"));
    }
    let (visibility, name, group) = header(input.clone())?;
   //eprintln!("{:?} {}", visibility, name);

    if let proc_macro2::TokenTree::Group(group) = group {
       //eprintln!("--------------------------------");
        parse_group(group);
    } else {
        return Err(syn::Error::new_spanned(
            input,
            "Missing group after enum name",
        ));
    }

    Ok(Default::default())
}
 */

fn generate_normal_function(
    basic: &Ident,
    variant: &Variant,
    block: bool,
    no_async: bool,
) -> syn::Result<TokenStream> {
    let mut ret = TokenStream::new();
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

    Ok(ret)
}

fn generate_waitable_function(
    basic: &Ident,
    variant: &Variant,
    return_type: TokenStream,
    block: bool,
    no_async: bool,
) -> syn::Result<TokenStream> {
    let mut ret = TokenStream::new();
    let definition = EnumDefinition::try_from(variant)?;
    let arg_def = definition.fields().to_arg_def(variant.span());
    let arg = definition.fields().enchant_arg(variant.span());
    let function_name = definition.get_name(variant.span());
    let member = &variant.ident;
    if !no_async {
        let q = quote! {
            pub async fn #function_name (&self, #arg_def) -> std::option::Option< #return_type > {
                let (__private_sender, __private_r) = tokio::sync::oneshot::channel();
                self.sender
                    .send(#basic::#member #arg)
                    .await
                    .ok();
                __private_r.await.ok()
            }
        };
        ret.extend(q);
    };

    if block {
        let function_name = if no_async {
            function_name
        } else {
            definition.get_name_block(variant.span())
        };
        let q = quote! {
            pub fn #function_name (&self, #arg_def) -> std::option::Option< #return_type > {
                let (__private_sender, __private_r) = tokio::sync::oneshot::channel();
                self.sender
                    .blocking_send(#basic::#member #arg)
                    .ok();
                __private_r.blocking_recv().ok()
            }
        };
        ret.extend(q);
    }
    Ok(ret)
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
        let return_type = parse_return_type(variant);

        let token = if let Some(ret) = return_type {
            generate_waitable_function(basic, variant, ret, block, no_async)
        } else {
            generate_normal_function(basic, variant, block, no_async)
        }?;

        //eprintln!("{:?}", token);

        ret.extend(token);
    }
    Ok(ret)
}

fn parse_return_type(variant: &Variant) -> Option<TokenStream> {
    for attr in &variant.attrs {
        match &attr.meta {
            syn::Meta::List(list) => {
                if !list.path.segments.first()?.ident.eq("ret") {
                    continue;
                }
                return Some(list.tokens.clone());
            }
            _ => continue,
        }
    }
    None
}

fn generate_member(_st: &syn::DeriveInput, de: &DataEnum) -> syn::Result<TokenStream> {
    let mut ret = TokenStream::new();
    //eprintln!("{:?}", st);
    for variant in &de.variants {
        //eprintln!("{:?}", parse_return_type(variant));
        let return_type = parse_return_type(variant);
        let definition = EnumDefinition::try_from(variant)?;
        let arg_def = definition.fields().enum_arg_def(return_type);
        //eprintln!("{:?}", arg_def);
        let enum_name = definition.get_normal_name(variant.span());
        let result = quote! {
            #enum_name #arg_def ,
        };
        //eprintln!("{:#?}", result.to_string());
        ret.extend(result);
    }
    Ok(ret)
}

fn do_first_expand(st: &syn::DeriveInput) -> syn::Result<TokenStream> {
    //eprintln!("{} {}", block, no_async);
    let data_enum = extract_enum(st)?;
    //print_fields();

    let enum_ident = &st.ident;

    let mut attrs = TokenStream::new();
    st.attrs.iter().for_each(|x| x.to_tokens(&mut attrs));
    //eprintln!("{:?}", attrs);

    let member_function = generate_member(st, data_enum)?;

    let ret = quote! {

        #attrs
        pub enum #enum_ident {
            #member_function
        }

    };

    Ok(ret)
}

pub fn handle_new(input: syn::DeriveInput) -> syn::Result<TokenStream> {
    //eprintln!("{:?}", early_st);

    crate::early_check(&input)?;

    let mut header = do_first_expand(&input)?;

    header.extend(do_expand(&input, Some(generate_function))?);

    Ok(header)
}
