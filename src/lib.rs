mod basic;
mod bit;
mod enchanted;

#[proc_macro_derive(Helper, attributes(helper))]
pub fn enum_helper_generator(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    //eprintln!("{:#?}", st.attrs);

    if let Err(e) = crate::basic::early_check(&st) {
        return e.into_compile_error().into();
    }

    basic::do_expand(&st, None)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
    //eprintln!("{:#?}", item);
}

#[proc_macro]
pub fn bit_helper(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    //eprintln!("{:#?}", st.attrs);

    bit::parse_bit(st)
        .unwrap_or_else(|x| x.into_compile_error())
        .into()
}

#[proc_macro]
pub fn oneshot_helper(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let early_st = syn::parse_macro_input!(input as syn::DeriveInput);
    enchanted::handle_new(early_st)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
