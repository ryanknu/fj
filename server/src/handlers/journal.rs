use crate::error::FjError;
use heed::types::Str;
use heed::{Database, Env};
use oxhttp::model::{HeaderName, Headers, Request, Response, Status};
use std::io::Read;
use std::str::FromStr;

pub fn extract_user_id(request: &Request) -> anyhow::Result<&str> {
    Ok(request
        .header(&HeaderName::from_str("x-fj-user")?)
        .unwrap()
        .to_str()?)
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
