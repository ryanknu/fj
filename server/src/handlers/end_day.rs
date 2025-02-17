use crate::error::FjError;
use crate::state::extract_user_id;
use anyhow::bail;
use heed::types::Str;
use heed::{Database, Env, RoTxn};
use jiff::civil::Date;
use jiff::ToSpan;
use oxhttp::model::{Request, Response, Status};
use serde::Deserialize;
use std::ops::Add;
use std::str::FromStr;

pub fn get_current_date<'a, 'b>(
    user_key: &'b str,
    db: &Database<Str, Str>,
    rtxn: &'a RoTxn,
) -> anyhow::Result<(&'a str, &'a str)> {
    #[derive(Deserialize)]
    struct InnerStructure<'a> {
        #[serde(borrow)]
        current_date: &'a str,
    }

    let user_record = db.get(&rtxn, &user_key)?;
    let Some(user_record) = user_record else {
        bail!("User does not exist in db");
    };

    let user = serde_json::from_str::<InnerStructure>(user_record)?;
    Ok((user.current_date, user_record))
}

pub fn http_end_day(r: &mut Request, db_env: &Env, db: &Database<Str, Str>) -> Response {
    end_day(r, db_env, db).unwrap_or_else(|err| FjError::from(err).into())
}

pub fn end_day(r: &mut Request, db_env: &Env, db: &Database<Str, Str>) -> anyhow::Result<Response> {
    let user_id = extract_user_id(r)?;
    let user_key = format!("user.{user_id}");

    #[derive(Deserialize)]
    struct InnerStructure<'a> {
        #[serde(borrow)]
        current_date: &'a str,
    }

    let tomorrow = {
        // Start a write transaction.
        let mut wtxn = db_env.write_txn()?;

        // Retrieve user's current status from the DB.
        let (date, record) = get_current_date(&user_key, &db, &mut wtxn)?;
        let current_date = Date::from_str(date)?;

        // Calculate tomorrow
        let tomorrow = current_date.add(27.hours());
        let tomorrow = tomorrow.to_string();

        // OK, this is kinda gross, but I'm just going to find and replace the previous date with
        // the next date in the structure. If we ever store like, the created date or something,
        // this would overwrite that.
        let new_user = record.replace(date, &tomorrow);

        // Write and close transaction.
        db.put(&mut wtxn, &user_key, &new_user)?;
        wtxn.commit()?;

        tomorrow
    };

    Ok(Response::builder(Status::OK).with_body(format!(r#"{{"current_date": "{tomorrow}"}}"#)))
}
