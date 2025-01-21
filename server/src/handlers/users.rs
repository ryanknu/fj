use crate::error::FjError;
use heed::types::Str;
use heed::{Database, Env};
use oxhttp::model::{HeaderName, Request, Response, Status};
use serde::{Deserialize, Serialize};
use std::io::Read;

#[derive(Deserialize)]
pub struct UserRequest {
    image: String,
    user_name: String,
    display_name: String,
    target_calories: u64,
}

#[derive(Deserialize, Serialize)]
pub struct UserDbRecord {
    image: String,
    display_name: String,
    target_calories: u64,
    target_fat: u64,
    target_protein: u64,
    target_carbohydrate: u64,
}

impl UserDbRecord {
    fn into_response_with_username(self, user_name: String) -> UserResponse {
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
pub struct UsersApiResponse {
    users: Vec<UserResponse>,
}

#[derive(Serialize)]
pub struct UserResponse {
    image: String,
    user_name: String,
    display_name: String,
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
    register(request, db_env, db)?;

    Ok(Response::builder(Status::CREATED).build())
}

fn register(
    request: UserRequest,
    db_env: &Env,
    db: &Database<Str, Str>,
) -> Result<(), heed::Error> {
    let macros = target_macros(request.target_calories);
    let record = UserDbRecord {
        image: request.image,
        display_name: request.display_name,
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

    Ok(())
}

pub fn http_get_users(db_env: &Env, db: &Database<Str, Str>) -> Response {
    get_users_inner(db_env, db).unwrap_or_else(|err| FjError::from(err).into())
}

fn get_users_inner(db_env: &Env, db: &Database<Str, Str>) -> anyhow::Result<Response> {
    let users = get_users(db_env, db)?;
    let users = UsersApiResponse { users };
    let data = serde_json::to_string(&users)?;

    Ok(Response::builder(Status::OK)
        .with_header(HeaderName::CONTENT_TYPE, "application/json")?
        .with_body(data)
        .into())
}

fn get_users(db_env: &Env, db: &Database<Str, Str>) -> anyhow::Result<Vec<UserResponse>> {
    let mut users = Vec::new();
    let rtxn = db_env.read_txn()?;

    for record in db.prefix_iter(&rtxn, "user.")? {
        let (key, value) = record?;
        let record = serde_json::from_str::<UserDbRecord>(value)?;
        let user_name = String::from(&key[5..]);
        users.push(record.into_response_with_username(user_name));
    }

    rtxn.commit()?;
    Ok(users)
}

/// Sets target macros based on the simple 50/30/20 rule.
fn target_macros(target_calories: u64) -> TargetMacros {
    TargetMacros {
        target_fat: target_calories / 8,
        target_protein: target_calories / 12,
        target_carbohydrate: target_calories / 45,
    }
}
