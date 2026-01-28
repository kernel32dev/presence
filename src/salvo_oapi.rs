#![cfg(feature = "salvo")]
#![doc(hidden)]

pub type RenamedOption<T> = std::option::Option<T>;

pub use salvo_oapi::*;

// causing presence fields to be not required is handled at the macro level
// the ToSchema impl is incapable of expressing the fact that the field is not required
#[cfg(feature = "salvo")]
impl<T: salvo_oapi::ToSchema> salvo_oapi::ToSchema for crate::Presence<T> {
    fn to_schema(
        components: &mut salvo_oapi::Components,
    ) -> salvo_oapi::RefOr<salvo_oapi::schema::Schema> {
        <T as salvo_oapi::ToSchema>::to_schema(components)
    }
}
