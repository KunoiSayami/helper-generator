use proc_macro2::{Literal, TokenStream};
use quote::quote;
use syn::{Data, DeriveInput, Ident};

use crate::basic::EnumDefinition;

pub(crate) fn parse_bit(input: DeriveInput) -> syn::Result<TokenStream> {
    //eprintln!("{:#?}", input);

    let vis = input.vis.clone();
    let ident = &input.ident;

    let Data::Enum(de) = input.data else {
        return Err(syn::Error::new_spanned(input, "Should use enum as input"));
    };

    let mut v = Vec::with_capacity(de.variants.len());
    let mut body = TokenStream::new();

    for (index, element) in de.variants.iter().enumerate() {
        //eprintln!("{element:?}");
        let ident = &element.ident;
        v.push(&element.ident);

        let index = Literal::usize_unsuffixed(index);

        //let index = index.to_string();

        body.extend(quote! {
            #ident = 1 << #index,
        });
    }

    //let mut checker = Vec::with_capacity(v.len());

    let mut impl_body = TokenStream::new();

    for ident in v {
        let name = format!(
            "is_{}",
            EnumDefinition::string_into_snake_case(&ident.to_string())
        );
        let fn_name = Ident::new(&name, ident.span());
        impl_body.extend(quote! {
            #vis fn #fn_name(&self) -> bool {
                matches!(&self, Self::#ident)
            }
        });
    }

    Ok(quote! {
        #[derive(Clone, Copy, Debug)]
        #vis enum #ident {
            #body
        }

        impl #ident {
            #impl_body
        }
    })
}
