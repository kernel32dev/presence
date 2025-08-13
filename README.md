# presence

This library provides `Presence<T>`, an enum for distinguishing between an explicitly `null` value and an omitted (`undefined`) value. This is a common requirement in contexts such as API partial updates (e.g., HTTP PATCH) where the absence of a field has a different meaning than the field being present with a `null` value.

-----

## If `None` is `null`, then `Absent` is `undefined`

The `Presence<T>` enum is analogous to the standard `Option<T>` but carries a different semantic meaning.

  * **`Presence::Absent`**: Represents an omitted or undefined value. In a JSON payload, this corresponds to a key not being present.
  * **`Option::None`**: Represents an explicit `null` value. In a JSON payload, this corresponds to a key being present with the value `null`.

The primary utility is realized when `Presence<T>` wraps an `Option<T>`. A field of type `Presence<Option<T>>` can represent three states:

| State | JSON Example (`{"field":...}`) | Rust Representation | Description |
| :--- | :--- | :--- | :--- |
| **Defined** | `{"field": "value"}` | `Present(Some("value"))` | The field is present with a value. |
| **Null** | `{"field": null}` | `Present(None)` | The field is present and explicitly set to `null`. |
| **Undefined** | `{}` | `Absent` | The field was not included in the payload. |

The library also exports `Undefined` which is a zero sized type that represents absence, this is useful for generic structs where `T` may be mandatory (just a plain `T`), ommitable (a `Presence<T>`), or not specifiable at all (an `Undefined`). Avoiding the need to reserve bytes for a `Presence::<T>::Absent` if the value is known to be absent at compile time.

-----

## Deriving with `presence_aware`

Standard derives like `Debug`, `serde::Serialize`, and `serde::Deserialize` cannot be used directly on structs containing `Presence<T>` due to the need for specialized implementations.

To enable these traits, the library provides the `#[presence_aware]` attribute macro. This macro must be applied to any struct or enum definition containing `Presence<T>` fields to ensure correct behavior.

### Example

The following example defines a `UserPatch` struct for a partial update. The `email` field can be updated, set to `null`, or left unmodified.

```rust
use presence::prelude::*;
use serde::{Deserialize, Serialize};

// The presence_aware macro is required for the derives to work correctly.
#[presence_aware]
#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct UserPatch {
    // `serde(default)` is necessary to handle cases where the field is absent.
    #[serde(default)]
    email: Presence<Option<String>>,
}

fn main() {
    // 1. Field is defined with a value
    let payload_defined = r#"{ "email": "new@example.com" }"#;
    let patch1: UserPatch = serde_json::from_str(payload_defined).unwrap();
    assert_eq!(
        patch1.email,
        Present(Some("new@example.com".to_string()))
    );
    println!("Defined: {:?}", patch1);

    // 2. Field is explicitly null
    let payload_null = r#"{ "email": null }"#;
    let patch2: UserPatch = serde_json::from_str(payload_null).unwrap();
    assert_eq!(patch2.email, Present(None));
    println!("Null:    {:?}", patch2);

    // 3. Field is absent (undefined)
    let payload_absent = r#"{}"#;
    let patch3: UserPatch = serde_json::from_str(payload_absent).unwrap();
    assert_eq!(patch3.email, Absent);
    println!("Absent:  {:?}", patch3);
}
```

#### Output

```
Defined: UserPatch { email: Present(Some("new@example.com")) }
Null:    UserPatch { email: Present(None) }
Absent:  UserPatch { email: Absent }
```
