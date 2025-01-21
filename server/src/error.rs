use oxhttp::model::{Response, Status};

pub struct FjError(anyhow::Error);

impl From<anyhow::Error> for FjError {
    fn from(e: anyhow::Error) -> Self {
        FjError(e)
    }
}

impl From<FjError> for Response {
    fn from(e: FjError) -> Response {
        Response::builder(Status::INTERNAL_SERVER_ERROR).with_body(e.0.to_string())
    }
}
