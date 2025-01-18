use heed::types::Str;
use heed::{Database, Env, RwTxn};
use oxhttp::model::{Method, Request, Response, Status};

pub fn journal(r: &Request, db_env: &Env) -> Response {
    match &**r.method() {
        "GET" => get_journal(r, db_env),
        "POST" => post_journal(r, db_env),
        _ => Response::builder(Status::METHOD_NOT_ALLOWED).build(),
    }
}

fn get_journal(r: &Request, db_env: &Env) -> Response {
    let db_env = db_env.clone();
    let rtxn = db_env.read_txn().unwrap();
    let db: Database<Str, Str> = db_env.open_database(&rtxn, None).unwrap().unwrap();

    let res = db.get(&rtxn, "hello").unwrap().unwrap();
    let res = res.to_owned();

    Response::builder(Status::OK).with_body(res)
}

fn post_journal(r: &Request, db_env: &Env) -> Response {
    let db_env = db_env.clone();
    let mut wtxn = db_env.write_txn().unwrap();
    let db: Database<Str, Str> = db_env.create_database(&mut wtxn, None).unwrap();

    db.put(&mut wtxn, "hello", "world").unwrap();
    wtxn.commit().unwrap();

    Response::builder(Status::NO_CONTENT).build()
}
