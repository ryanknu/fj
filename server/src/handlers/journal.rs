use crate::error::FjError;
use crate::state::extract_user_id;
use heed::types::Str;
use heed::{Database, Env};
use oxhttp::model::{HeaderName, Headers, Request, Response, Status};
use std::io::Read;
use std::str::FromStr;

struct JournalEntry {
    /// Simple input, what did I eat?
    text: String,
    /// This is the journal record date. It is not the date of the timestamp. Format Y-m-d
    date: String,
    /// This field is only used for adjusting the amounts after initial save. I don't really care the units.
    qty: f64,
    /// Calories and macronutrients
    calories: u64,
    carbohydrate: u64,
    fat: u64,
    protein: u64,
}

struct JournalEntryRequest {
    text: String,
    date: String,
}

pub fn journal(r: &mut Request, db_env: &Env, db: &Database<Str, Str>) -> Response {
    let res = match &**r.method() {
        "GET" => get_journal(r, db_env, db),
        "POST" => post_journal(r, db_env, db),
        _ => Ok(Response::builder(Status::METHOD_NOT_ALLOWED).build()),
    };

    res.unwrap_or_else(|err| FjError::from(err).into())
}

fn get_journal(r: &mut Request, db_env: &Env, db: &Database<Str, Str>) -> anyhow::Result<Response> {
    let user_id = extract_user_id(&r)?;

    let rtxn = db_env.read_txn().unwrap();
    let res = db.get(&rtxn, &format!("{user_id}.name"))?.unwrap();
    let res = res.to_owned();

    Ok(Response::builder(Status::OK).with_body(res))
}

/// Posts a food log to your journal.
fn post_journal(
    r: &mut Request,
    db_env: &Env,
    db: &Database<Str, Str>,
) -> anyhow::Result<Response> {
    let mut name = String::new();
    let mut body = r.body_mut();
    body.read_to_string(&mut name)?;

    let user_id = extract_user_id(&r)?;

    let mut wtxn = db_env.write_txn()?;

    db.put(&mut wtxn, &format!("{user_id}.name"), &name)?;
    wtxn.commit()?;

    Ok(Response::builder(Status::NO_CONTENT).build())
}
