use crate::error::FjError;
use heed::types::Str;
use heed::{Database, Env};
use oxhttp::model::{HeaderName, Request, Response, Status};
use serde::{Deserialize, Serialize};
use std::io::Read;

#[derive(Clone, Deserialize)]
pub struct UserRequest {
    image: String,
    user_name: String,
    display_name: String,
    target_calories: u64,
}

#[derive(Deserialize, Serialize)]
pub struct UserDbRecord<'a> {
    #[serde(borrow)]
    image: &'a str,
    #[serde(borrow)]
    display_name: &'a str,
    target_calories: u64,
    target_fat: u64,
    target_protein: u64,
    target_carbohydrate: u64,
}

impl<'a> UserDbRecord<'a> {
    fn into_response_with_username(self, user_name: &'a str) -> UserResponse {
        UserResponse {
            image: self.image,
            user_name,
            display_name: self.display_name,
            target_calories: self.target_calories,
            target_fat: self.target_fat,
            target_protein: self.target_protein,
            target_carbohydrate: self.target_carbohydrate,
        }
    }
}

/// This is a thin wrapper for the API response in case we want to add more functionality to the
/// API later.
#[derive(Serialize)]
pub struct UsersApiResponse<'a> {
    users: Vec<UserResponse<'a>>,
}

#[derive(Serialize)]
pub struct UserResponse<'a> {
    image: &'a str,
    user_name: &'a str,
    display_name: &'a str,
    target_calories: u64,
    target_fat: u64,
    target_protein: u64,
    target_carbohydrate: u64,
}

struct TargetMacros {
    target_fat: u64,
    target_protein: u64,
    target_carbohydrate: u64,
}

pub fn http_post_register(r: &mut Request, db_env: &Env, db: &Database<Str, Str>) -> Response {
    // This function is a little wrapper that converts anyhow errors into FjErrors to allow ? use.
    post_register(r, db_env, db).unwrap_or_else(|err| FjError::from(err).into())
}

fn post_register(
    r: &mut Request,
    db_env: &Env,
    db: &Database<Str, Str>,
) -> anyhow::Result<Response> {
    // TODO: It would be nice if we checked the Content-Type header for some kind of JSON.
    let mut name = Vec::new();
    let mut body = r.body_mut();
    body.read_to_end(&mut name)?;

    let request = serde_json::from_slice::<UserRequest>(&name)?;
    let data = register(request.clone(), db_env, db)?;

    Ok(Response::builder(Status::CREATED)
        .with_header(HeaderName::CONTENT_TYPE, "application/json")?
        .with_body(data)
        .into())
}

fn register(
    request: UserRequest,
    db_env: &Env,
    db: &Database<Str, Str>,
) -> anyhow::Result<Vec<u8>> {
    let macros = target_macros(request.target_calories);

    let record = UserDbRecord {
        image: &request.image,
        display_name: &request.display_name,
        target_calories: request.target_calories,
        target_fat: macros.target_fat,
        target_protein: macros.target_protein,
        target_carbohydrate: macros.target_carbohydrate,
    };
    let record_json = serde_json::to_string(&record).unwrap();

    let mut wtxn = db_env.write_txn()?;
    db.put(
        &mut wtxn,
        &format!("user.{}", request.user_name),
        &record_json,
    )?;
    wtxn.commit()?;

    Ok(serde_json::to_vec(&UserResponse {
        image: &request.image,
        user_name: &request.user_name,
        display_name: &request.display_name,
        target_calories: request.target_calories,
        target_fat: macros.target_fat,
        target_protein: macros.target_protein,
        target_carbohydrate: macros.target_carbohydrate,
    })?)
}

pub fn http_get_users(db_env: &Env, db: &Database<Str, Str>) -> Response {
    get_users_inner(db_env, db).unwrap_or_else(|err| FjError::from(err).into())
}

fn get_users_inner(db_env: &Env, db: &Database<Str, Str>) -> anyhow::Result<Response> {
    let data = get_users(db_env, db)?;

    Ok(Response::builder(Status::OK)
        .with_header(HeaderName::CONTENT_TYPE, "application/json")?
        .with_body(data)
        .into())
}

fn get_users(db_env: &Env, db: &Database<Str, Str>) -> anyhow::Result<Vec<u8>> {
    let mut users = Vec::new();
    let rtxn = db_env.read_txn()?;

    for record in db.prefix_iter(&rtxn, "user.")? {
        let (key, value) = record?;
        let record = serde_json::from_str::<UserDbRecord>(value)?;
        users.push(record.into_response_with_username(&key[5..]));
    }

    let res = serde_json::to_vec(&UsersApiResponse { users })?;

    rtxn.commit()?;
    Ok(res)
}

/// Sets target macros based on the simple 50/30/20 rule.
fn target_macros(target_calories: u64) -> TargetMacros {
    TargetMacros {
        target_fat: target_calories / 8,
        target_protein: target_calories / 12,
        target_carbohydrate: target_calories / 45,
    }
}
