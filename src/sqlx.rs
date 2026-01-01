//! this is an undocumented feature needed by another crate of mine due to orphan rules
#![cfg(feature = "sqlx")]

pub trait PresenceAwareFromSqlxSequenceItem: Sized {
    /// on types other than Undefined, calls try_get and increments index on success
    ///
    /// if Self is Undefined, then this method ignores the arguments and returns Undefined
    fn from_row_item(row: &sqlx::postgres::PgRow, index: &mut usize) -> Result<Self, sqlx::Error>;
    /// on types other than Undefined, calls try_decode
    ///
    /// if Self is Undefined, then this method ignores the arguments and returns Undefined
    fn from_record_item(
        decoder: &mut sqlx::postgres::types::PgRecordDecoder<'_>,
    ) -> Result<Self, sqlx::error::BoxDynError>;
}
impl<T: for<'a> sqlx::Decode<'a, sqlx::Postgres> + sqlx::Type<sqlx::Postgres>>
    PresenceAwareFromSqlxSequenceItem for T
{
    fn from_row_item(row: &sqlx::postgres::PgRow, index: &mut usize) -> Result<Self, sqlx::Error> {
        let value = sqlx::Row::try_get(row, *index)?;
        *index += 1;
        Ok(value)
    }
    fn from_record_item(
        decoder: &mut sqlx::postgres::types::PgRecordDecoder<'_>,
    ) -> Result<Self, sqlx::error::BoxDynError> {
        decoder.try_decode::<T>()
    }
}
impl PresenceAwareFromSqlxSequenceItem for crate::Undefined {
    fn from_row_item(_: &sqlx::postgres::PgRow, _: &mut usize) -> Result<Self, sqlx::Error> {
        Ok(Self)
    }
    fn from_record_item(
        _: &mut sqlx::postgres::types::PgRecordDecoder<'_>,
    ) -> Result<Self, sqlx::error::BoxDynError> {
        Ok(Self)
    }
}
