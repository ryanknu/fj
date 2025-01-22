use crate::error::FjError;
use heed::types::Str;
use heed::{Database, Env};
use oxhttp::model::{HeaderName, Request, Response, Status};
use serde::{Deserialize, Serialize};
use std::io::Read;
use std::str::FromStr;

#[derive(Clone, Deserialize)]
pub struct UserRequest {
    image: String,
    user_name: String,
    display_name: String,
    age: u64,
    gender: Gender,
    goal: FitnessGoal,
    factor: ActivityFactor,
    height: u64,
    weight: u64,
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

#[derive(Deserialize, Clone, Copy)]
enum ActivityFactor {
    Sedentary,
    LightActivity,
    ModerateActivity,
    VeryActive,
    ExtraActive,
}

#[derive(Deserialize, Clone, Copy)]
enum Gender {
    Male,
    Female,
}

#[derive(Deserialize, Clone, Copy)]
enum FitnessGoal {
    LoseWeight,
    Maintain,
}

impl From<ActivityFactor> for f64 {
    fn from(value: ActivityFactor) -> Self {
        match value {
            ActivityFactor::Sedentary => 1.2,
            ActivityFactor::LightActivity => 1.375,
            ActivityFactor::ModerateActivity => 1.55,
            ActivityFactor::VeryActive => 1.725,
            ActivityFactor::ExtraActive => 1.9,
        }
    }
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
    let target_calories = target_macros2(
        request.height,
        request.weight,
        request.age,
        request.goal,
        request.gender,
        request.factor,
    );
    let macros = target_macros(target_calories);

    let record = UserDbRecord {
        image: &request.image,
        display_name: &request.display_name,
        target_calories: target_calories,
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
        target_calories: target_calories,
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

/// Implements the Mifflin-St. Jeor algorithm for nutrition targets.
/// TEMPORARY: This returns a calorie goal.
fn target_macros2(
    height_cm: u64,
    weight_kg: u64,
    age: u64,
    goal: FitnessGoal,
    gender: Gender,
    activity_factor: ActivityFactor,
) -> u64 {
    let bmr = (10 * weight_kg) + (height_cm * 25 / 4) - (5 * age);
    let bmr = match gender {
        Gender::Male => bmr.saturating_add(5),
        Gender::Female => bmr.saturating_sub(161),
    };
    let factor = match goal {
        FitnessGoal::LoseWeight => 0.8,
        FitnessGoal::Maintain => 1.0,
    };

    let activity_factor: f64 = activity_factor.into();
    let calories = (bmr as f64 * activity_factor * factor) as u64;
    calories
}

/// Sets target macros based on the simple 50/30/20 rule.
fn target_macros(target_calories: u64) -> TargetMacros {
    TargetMacros {
        target_fat: target_calories / 8,
        target_protein: target_calories / 12,
        target_carbohydrate: target_calories / 45,
    }
}
