pub struct Config {
    file_location: String,
    meilisearch: Option<MeilisearchConfig>,
}

pub struct MeilisearchConfig {
    host: String,
    api_key: String,
}
