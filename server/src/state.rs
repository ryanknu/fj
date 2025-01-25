use oxhttp::model::{HeaderName, Request};
use std::str::FromStr;

struct FjState {
    // http client
    // meili client
    // lmdb handle
}

pub fn extract_user_id(request: &Request) -> anyhow::Result<&str> {
    Ok(request
        .header(&HeaderName::from_str("x-fj-user")?)
        .unwrap()
        .to_str()?)
}
