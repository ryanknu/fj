use heed::types::Str;
use heed::{Database, Env};
use oxhttp::model::{Body, HeaderName, Request, Response, Status};
use std::io::Read;
use std::str::FromStr;

pub fn journal(r: &mut Request, db_env: &Env, db: &Database<Str, Str>) -> Response {
    match &**r.method() {
        "GET" => get_journal(r, db_env, db),
        "POST" => post_journal(r, db_env, db),
        _ => Response::builder(Status::METHOD_NOT_ALLOWED).build(),
    }
}

fn get_journal(r: &mut Request, db_env: &Env, db: &Database<Str, Str>) -> Response {
    let user = r
        .header(&HeaderName::from_str("x-fj-user").unwrap())
        .unwrap()
        .to_str()
        .unwrap();

    let rtxn = db_env.read_txn().unwrap();
    let res = db.get(&rtxn, &format!("{user}.name")).unwrap().unwrap();
    let res = res.to_owned();

    Response::builder(Status::OK).with_body(res)
}

/// Posts a food log to your journal.
fn post_journal(r: &mut Request, db_env: &Env, db: &Database<Str, Str>) -> Response {
    let mut name = String::new();
    let mut body = r.body_mut();
    body.read_to_string(&mut name).unwrap();

    let user = r
        .header(&HeaderName::from_str("x-fj-user").unwrap())
        .unwrap()
        .to_str()
        .unwrap();

    let mut wtxn = db_env.write_txn().unwrap();

    db.put(&mut wtxn, &format!("{}.name", &user), &name)
        .unwrap();
    wtxn.commit().unwrap();

    Response::builder(Status::NO_CONTENT).build()
}
