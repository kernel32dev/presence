#![cfg(test)]

use std::marker::PhantomData;

macro_rules! test_round_trip_json {
    ($variable:ident) => {{
        let encoded = serde_json::to_vec(&$variable).expect("Failed to serialize");
        let decoded = serde_json::from_slice(&encoded).expect("Failed to deserialize");
        assert_eq!($variable, decoded);
    }};
}

mod structs {
    use crate::{Presence, tests::ByteArray};

    #[crate::presence_aware(crate = crate)]
    #[derive(Debug, PartialEq, serde_derive::Serialize, serde_derive::Deserialize)]
    pub struct UserProfile<'a> {
        id: u64,
        name: Presence<String>,
        #[serde(borrow)]
        email: Presence<&'a str>,
        #[serde(rename = "lastLogin")]
        last_login_at: Presence<u64>,
    }

    #[crate::presence_aware(crate = crate)]
    #[derive(Debug, PartialEq, serde_derive::Serialize, serde_derive::Deserialize)]
    pub struct AppSettings {
        theme: Presence<String>,
        notifications_enabled: bool,
        #[serde(default)]
        dark_mode: bool,
        #[serde(rename = "cacheSize")]
        cache_size_kb: Presence<u32>,
    }

    #[crate::presence_aware(crate = crate)]
    #[derive(Debug, PartialEq, Clone, serde_derive::Serialize, serde_derive::Deserialize)]
    struct GenericEvent<T>
    where
        T: Clone + PartialEq,
    {
        event_type: Presence<String>,
        payload: Presence<T>,
    }

    #[crate::presence_aware(crate = crate)]
    #[derive(Debug, PartialEq, Clone, serde_derive::Serialize, serde_derive::Deserialize)]
    struct ConstSizedArray<const N: usize> {
        #[serde(default = "new_const_sized_array_id")]
        id: u64,
        #[serde(rename = "dataArray")]
        data_array: Presence<ByteArray<N>>,
    }

    fn new_const_sized_array_id() -> u64 {
        u64::MAX
    }

    #[test]
    fn test_user_profile_fully_present() {
        let profile = UserProfile {
            id: 123,
            name: Presence::Present("Alice".to_string()),
            email: Presence::Present("alice@example.com"),
            last_login_at: Presence::Present(1678886400),
        };
        test_round_trip_json!(profile);
    }

    #[test]
    fn test_user_profile_partially_absent() {
        let profile = UserProfile {
            id: 456,
            name: Presence::Absent,
            email: Presence::Present("bob@example.com"),
            last_login_at: Presence::Absent,
        };
        test_round_trip_json!(profile);
    }

    #[test]
    fn test_user_profile_borrowed_data() {
        let email_str = "charlie@example.com";
        let profile = UserProfile {
            id: 789,
            name: Presence::Present("Charlie".to_string()),
            email: Presence::Present(email_str),
            last_login_at: Presence::Present(1678887000),
        };
        test_round_trip_json!(profile);
    }

    #[test]
    fn test_app_settings_fully_present() {
        let settings = AppSettings {
            theme: Presence::Present("dark".to_string()),
            notifications_enabled: true,
            dark_mode: true,
            cache_size_kb: Presence::Present(1024),
        };
        test_round_trip_json!(settings);
    }

    #[test]
    fn test_app_settings_partially_absent() {
        let settings = AppSettings {
            theme: Presence::Absent,
            notifications_enabled: false,
            dark_mode: false,
            cache_size_kb: Presence::Present(2048),
        };
        test_round_trip_json!(settings);
    }

    #[test]
    fn test_app_settings_missing_mandatory_field() {
        // Construct a JSON object that is missing the mandatory `notifications_enabled` field.
        let json_data = serde_json::json!({
            "theme": "light",
            "cacheSize": 512
        });

        let result = serde_json::from_value::<AppSettings>(json_data);
        let error = result.unwrap_err();
        assert_eq!(error.to_string(), "missing field `notifications_enabled`");
    }

    #[test]
    fn test_generic_event_with_string_payload() {
        let event = GenericEvent {
            event_type: Presence::Present("user_update".to_string()),
            payload: Presence::Present("User Alice has logged in".to_string()),
        };
        test_round_trip_json!(event);
    }

    #[test]
    fn test_generic_event_with_u32_payload() {
        let event = GenericEvent {
            event_type: Presence::Present("user_id".to_string()),
            payload: Presence::Present(12345),
        };
        test_round_trip_json!(event);
    }

    #[test]
    fn test_generic_event_absent_payload() {
        let event: GenericEvent<u32> = GenericEvent {
            event_type: Presence::Present("ping".to_string()),
            payload: Presence::Absent,
        };
        test_round_trip_json!(event);
    }

    #[test]
    fn test_const_sized_array_present() {
        let array_struct = ConstSizedArray::<4> {
            id: 1,
            data_array: Presence::Present(ByteArray([1, 2, 3, 4])),
        };
        test_round_trip_json!(array_struct);
    }

    #[test]
    fn test_const_sized_array_absent() {
        let array_struct = ConstSizedArray::<8> {
            id: 2,
            data_array: Presence::Absent,
        };
        test_round_trip_json!(array_struct);
    }
}

mod enums {
    use crate::{Presence, tests::ByteArray};

    #[crate::presence_aware(crate = crate)]
    #[derive(Debug, PartialEq, serde_derive::Serialize, serde_derive::Deserialize)]
    enum Command<'a, T, const N: usize>
    where
        T: Clone + PartialEq,
    {
        Login {
            username: Presence<String>,
            #[serde(borrow)]
            session_id: Presence<&'a str>,
        },
        Update {
            user_id: u64,
            payload: Presence<T>,
        },
        Status(#[serde(default)] u8),
        Data(u32, ByteArray<N>),
        Logout,
    }

    #[crate::presence_aware(crate = crate)]
    #[derive(Debug, PartialEq, serde_derive::Serialize, serde_derive::Deserialize)]
    enum Event {
        Connected,
        Disconnected,
        Message { payload: Presence<String> },
        Error(#[serde(default = "default_error")] u32),
    }

    const fn default_error() -> u32 {
        u32::MAX
    }

    #[test]
    fn test_command_login_fully_present() {
        let session_id = "abc-123";
        let cmd = Command::Login::<u32, 0> {
            username: Presence::Present("Alice".to_string()),
            session_id: Presence::Present(session_id),
        };
        test_round_trip_json!(cmd);
    }

    #[test]
    fn test_command_login_partially_absent() {
        let cmd = Command::Login::<u32, 0> {
            username: Presence::Absent,
            session_id: Presence::Present("def-456"),
        };
        test_round_trip_json!(cmd);
    }

    #[test]
    fn test_command_update_with_generic_payload() {
        let cmd = Command::Update::<String, 0> {
            user_id: 101,
            payload: Presence::Present("new_data".to_string()),
        };
        test_round_trip_json!(cmd);
    }

    #[test]
    fn test_command_update_absent_payload() {
        let cmd = Command::Update::<String, 0> {
            user_id: 102,
            payload: Presence::Absent,
        };
        test_round_trip_json!(cmd);
    }

    #[test]
    fn test_command_status() {
        let cmd = Command::Status::<u32, 0>(200);
        test_round_trip_json!(cmd);
    }

    #[test]
    fn test_command_data_with_const_generic() {
        let cmd = Command::Data::<u32, 3>(100, ByteArray([0xDE, 0xAD, 0xBE]));
        test_round_trip_json!(cmd);
    }

    #[test]
    fn test_command_logout() {
        let cmd = Command::Logout::<u32, 0>;
        test_round_trip_json!(cmd);
    }

    #[test]
    fn test_event_simple_variants() {
        let connected = Event::Connected;
        test_round_trip_json!(connected);

        let disconnected = Event::Disconnected;
        test_round_trip_json!(disconnected);
    }

    #[test]
    fn test_event_message_present() {
        let event = Event::Message {
            payload: Presence::Present("Hello, World!".to_string()),
        };
        test_round_trip_json!(event);
    }

    #[test]
    fn test_event_message_absent() {
        let event = Event::Message {
            payload: Presence::Absent,
        };
        test_round_trip_json!(event);
    }

    #[test]
    fn test_event_error_variant() {
        let event = Event::Error(500);
        test_round_trip_json!(event);
    }
}

/// serde serializable and deserializable byte array
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ByteArray<const N: usize>([u8; N]);

impl<const N: usize> serde::Serialize for ByteArray<N> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_bytes(&self.0)
    }
}
impl<'de, const N: usize> serde::Deserialize<'de> for ByteArray<N> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor<const N: usize> {
            _phantom: PhantomData<[u8; N]>,
        }
        impl<'de, const N: usize> serde::de::Visitor<'de> for Visitor<N> {
            type Value = [u8; N];

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                if N == 1 {
                    formatter.write_str("a byte array with 1 byte")
                } else {
                    formatter.write_fmt(format_args!("a byte array with {N} bytes"))
                }
            }
            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match TryInto::<&[u8; N]>::try_into(v) {
                    Ok(array) => Ok(*array),
                    Err(_) => Err(serde::de::Error::invalid_length(v.len(), &self)),
                }
            }
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                where
                    A: serde::de::SeqAccess<'de>, {
                let mut buffer = [0; N];
                for i in 0..N {
                    match seq.next_element::<u8>()? {
                        Some(byte) => {
                            buffer[i] = byte;
                        },
                        None => return Err(serde::de::Error::invalid_length(i, &self)),
                    }
                }
                let mut excess = 0;
                while let Some(_) = seq.next_element::<u8>()? {
                    excess += 1;
                }
                if excess != 0 {
                    return Err(serde::de::Error::invalid_length(N + excess, &self));
                }
                Ok(buffer)
            }
        }
        deserializer
            .deserialize_bytes(Visitor::<N> {
                _phantom: PhantomData,
            })
            .map(Self)
    }
}
