use crate::error::FjError;
use crate::handlers::end_day::get_current_date;
use crate::state::extract_user_id;
use heed::types::Str;
use heed::{Database, Env};
use oxhttp::model::{Request, Response, Status};
use serde::{Deserialize, Serialize};
use std::io::Read;
use std::time::SystemTime;

#[derive(Deserialize)]
struct JournalEntry {
    /// Simple input, what did I eat?
    text: String,
    qty: f64,
    qty_units: String,
    /// Calories and macronutrients
    calories: u64,
    carbohydrate: u64,
    fat: u64,
    protein: u64,
}

#[derive(Deserialize, Serialize)]
struct JournalEntryDbRecord<'a> {
    text: &'a str,
    timestamp: u128,
    qty: f64,
    qty_units: &'a str,
    calories: u64,
    carbohydrate: u64,
    fat: u64,
    protein: u64,
}

impl<'a> JournalEntryDbRecord<'a> {
    fn as_response_with_id(&self, id: &'a str) -> JournalEntryResponse<'a> {
        JournalEntryResponse {
            id,
            text: self.text,
            timestamp: self.timestamp,
            qty: self.qty,
            qty_units: self.qty_units,
            calories: self.calories,
            carbohydrate: self.carbohydrate,
            fat: self.fat,
            protein: self.protein,
        }
    }
}

#[derive(Serialize)]
struct JournalEntryResponse<'a> {
    id: &'a str,
    text: &'a str,
    timestamp: u128,
    qty: f64,
    qty_units: &'a str,
    calories: u64,
    carbohydrate: u64,
    fat: u64,
    protein: u64,
}

struct JournalEntryRequest {
    text: String,
    date: String,
}

#[derive(Serialize)]
struct JournalApiResponse<'a> {
    records: Vec<JournalEntryResponse<'a>>,
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
    let prefix_len = "entry..".len() + user_id.len();

    let mut records = Vec::new();
    let rtxn = db_env.read_txn()?;

    for record in db.prefix_iter(&rtxn, &format!("entry.{user_id}."))? {
        let (key, value) = record?;
        let record = serde_json::from_str::<JournalEntryDbRecord>(value)?;
        records.push(record.as_response_with_id(&key[prefix_len..]));
    }

    let res = serde_json::to_vec(&JournalApiResponse { records })?;

    rtxn.commit()?;

    Ok(Response::builder(Status::OK).with_body(res))
}

/// Posts a food log to your journal.
fn post_journal(
    r: &mut Request,
    db_env: &Env,
    db: &Database<Str, Str>,
) -> anyhow::Result<Response> {
    // Read the request payload
    let mut buf = Vec::new();
    let mut body = r.body_mut();
    body.read_to_end(&mut buf)?;
    let payload = serde_json::from_slice::<JournalEntry>(&buf)?;

    // Get the user's ID from headers
    let user_id = extract_user_id(&r)?;

    // Start a database transaction
    let mut wtxn = db_env.write_txn()?;

    let user_key = format!("user.{user_id}");
    let (date, _) = get_current_date(&user_key, db, &mut wtxn)?;
    let timestamp = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)?
        .as_millis();

    let record = JournalEntryDbRecord {
        text: &payload.text,
        timestamp,
        qty: payload.qty,
        qty_units: &payload.qty_units,
        calories: payload.calories,
        carbohydrate: payload.carbohydrate,
        fat: payload.fat,
        protein: payload.protein,
    };

    let record = serde_json::to_string(&record)?;

    let entry_key = format!("entry.{user_id}.{date}.{timestamp}");
    let recall_key = format!("food.{}", &payload.text[..16]);

    // Insert the record twice with two different keys, one for recall and one for the journal.
    db.put(&mut wtxn, &entry_key, &record)?;
    db.put(&mut wtxn, &recall_key, &record)?;
    wtxn.commit()?;

    Ok(Response::builder(Status::NO_CONTENT).build())
}
