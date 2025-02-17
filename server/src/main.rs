use heed::types::{Str, U32};
use heed::{Database, EnvOpenOptions};
use oxhttp::model::{Response, Status};
use oxhttp::Server;
use std::net::Ipv4Addr;
use std::time::Duration;

mod config;
mod error;
mod handlers;
mod model;
mod state;

/// Notes:
/// - [ ] Front end in elm
///   - Macro bars across the top (fat, protein, carb, calories), they can be really small, like 8px font
///   - Add a food by name
///   - Add a food by barcode (camera/scanner).. may not care
/// - [ ] Back end probably in axum
/// - [ ] Manually enter macros
/// - [ ] Query OpenFoodFacts in real time
/// - [ ] Utility to download OpenFoodFacts database
///   - [ ] Requires a Meilisearch instance
/// - [ ] Automatic entries (e.g. monster daily, or skip breakfast?)
/// - [ ] Containerfile based on linuxserver (see: https://github.com/linuxserver/docker-emby/blob/master/Dockerfile)
/// - [ ] Github docker release XML/JSON compatible with Unraid
/// - [ ] Use LMDB (heed/heed3) for primary storage in a /data dir
///
///
///
/// API:
/// - GET /v1/<user>/journal - retrieves the journal for the user for the last 31 days
/// - GET /v1/foods - returns all food items that were created by this user, for front-end search.
/// - GET /v1/food?term= - searches for food by term or barcode
/// - POST /v1/load-off - If meilisearch is configured, this will start a download and index of the OpenFoodFacts database
/// - GET /v1/load-off - Retrieves the status of any current OpenFoodFacts database download.
/// - POST /v1/<user>/journal - Upserts an entry in the journal. Date and ID must be specified. Daily totals are updated.

fn main() {
    println!("Hello, world!");

    // Open db
    let dir = std::env::current_dir().unwrap();
    let env = unsafe { EnvOpenOptions::new().open(dir).unwrap() };

    // TODO: Maybe open dbs for each user

    // Creates a default database, but...
    let mut wtxn = env.write_txn().unwrap();
    let db: Database<Str, Str> = env.clone().create_database(&mut wtxn, None).unwrap();
    wtxn.commit().unwrap();

    // TODO: We should definitely check content-length of request payloads and discard large ones.
    let mut server = Server::new(move |request| match request.url().path() {
        "/" => Response::builder(Status::OK).with_body("home"),
        "/v1/users" => handlers::users::http_get_users(&env, &db),
        "/v1/register" => handlers::users::http_post_register(request, &env, &db),
        "/v1/end-day" => handlers::end_day::http_end_day(request, &env, &db),
        "/journal" => handlers::journal::journal(request, &env, &db),
        _ => Response::builder(Status::NOT_FOUND).build(),
    });

    server = server
        .bind((Ipv4Addr::LOCALHOST, 8080))
        .with_global_timeout(Duration::from_secs(10))
        .with_max_concurrent_connections(128);

    server.spawn().unwrap().join().unwrap();
}
