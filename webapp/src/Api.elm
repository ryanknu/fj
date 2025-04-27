module Api exposing (CommState(..), JobTypes(..), JournalEntry, RegisterUserInputs, User, WeightLossGoal(..), endDay, errorToString, loadJournal, loadUsers, registerUser)

import Http exposing (emptyBody, header, request)
import Json.Decode as JsonDecode exposing (Decoder, field)
import Json.Encode as JsonEncode



-- Application models


type JobTypes
    = LoadingRegisteredUsers
    | RegisteringUser
    | LoadingJournal


type CommState
    = Loading
    | WorkingOn JobTypes
    | Error String
    | Idle


type alias User =
    { image : String
    , userName : String
    , displayName : String
    , currentDate : String
    , targetCalories : Int
    , targetFat : Int
    , targetProtein : Int
    , targetCarbohydrate : Int
    }



-- HTTP Handling


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "the URI \"" ++ url ++ "\" was invalid."

        Http.Timeout ->
            "unable to reach the server, try again."

        Http.NetworkError ->
            "unable to reach the server, check your network connection. This app may not work properly."

        -- TODO: Coordinate with FjError on the back end to send readable messages to the front end.
        Http.BadStatus 500 ->
            "the server reported a problem, try again later."

        Http.BadStatus 400 ->
            "verify your information and try again."

        Http.BadStatus n ->
            "unknown error (HTTP status: " ++ String.fromInt n ++ ")"

        Http.BadBody errorMessage ->
            errorMessage


fjHttpRequest :
    { method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , decoder : Decoder a
    , handleResult : Result Http.Error a -> msg
    , user : Maybe User
    }
    -> Cmd msg
fjHttpRequest r =
    Http.request
        { method = r.method
        , headers =
            -- TODO: add universal accept header; add content-type header for post
            List.append r.headers <| Maybe.withDefault [] <| Maybe.map (\n -> [ header "x-fj-user" n.userName ]) r.user

        -- TODO: Manage this URL globally somehow.
        , url = "http://localhost:8080/v1" ++ r.path
        , body = r.body
        , expect = Http.expectJson r.handleResult r.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


fjHttpGet :
    { path : String
    , decoder : Decoder a
    , handleResult : Result Http.Error a -> msg
    , user : Maybe User
    }
    -> Cmd msg
fjHttpGet r =
    fjHttpRequest
        { method = "GET"
        , headers = []
        , path = r.path
        , body = emptyBody
        , decoder = r.decoder
        , handleResult = r.handleResult
        , user = r.user
        }


fjHttpPost :
    { path : String
    , decoder : Decoder c
    , body : a
    , bodyEncoder : a -> JsonEncode.Value
    , handleResult : Result Http.Error c -> msg
    , user : Maybe User
    }
    -> Cmd msg
fjHttpPost r =
    fjHttpRequest
        { method = "POST"
        , headers =
            [ header "content-type" "application/json"
            ]
        , path = r.path
        , body = Http.jsonBody <| r.bodyEncoder r.body
        , decoder = r.decoder
        , handleResult = r.handleResult
        , user = r.user
        }



-- Load Users


loadUsers : (Result Http.Error (List User) -> msg) -> Cmd msg
loadUsers handler =
    fjHttpGet
        { path = "/users"
        , decoder = field "users" (JsonDecode.list userDecoder)
        , handleResult = handler
        , user = Nothing
        }



-- Register Users


type WeightLossGoal
    = MaintainWeight
    | LoseWeight


type alias RegisterUserInputs =
    { image : String
    , userName : String
    , displayName : String
    , gender : String
    , age : Int
    , height : Int
    , weight : Int
    , goal : WeightLossGoal
    , factor : String
    }


goalToString : WeightLossGoal -> String
goalToString val =
    case val of
        MaintainWeight ->
            "Maintain"

        LoseWeight ->
            "LoseWeight"


encodeRegisterUserInputs : RegisterUserInputs -> JsonEncode.Value
encodeRegisterUserInputs inputs =
    JsonEncode.object
        [ ( "image", JsonEncode.string inputs.image )
        , ( "user_name", JsonEncode.string inputs.userName )
        , ( "display_name", JsonEncode.string inputs.displayName )
        , ( "gender", JsonEncode.string inputs.gender )
        , ( "age", JsonEncode.int inputs.age )
        , ( "height", JsonEncode.int inputs.height )
        , ( "weight", JsonEncode.int inputs.weight )
        , ( "goal", JsonEncode.string <| goalToString inputs.goal )
        , ( "factor", JsonEncode.string inputs.factor )
        ]


registerUser : RegisterUserInputs -> (Result Http.Error User -> msg) -> Cmd msg
registerUser inputs handler =
    fjHttpPost
        { path = "/register"
        , body = inputs
        , bodyEncoder = encodeRegisterUserInputs
        , decoder = userDecoder
        , handleResult = handler
        , user = Nothing
        }


decodeApply : Decoder a -> Decoder (a -> b) -> Decoder b
decodeApply =
    JsonDecode.map2 (|>)


userDecoder : Decoder User
userDecoder =
    JsonDecode.succeed User
        |> decodeApply (field "image" JsonDecode.string)
        |> decodeApply (field "user_name" JsonDecode.string)
        |> decodeApply (field "display_name" JsonDecode.string)
        |> decodeApply (field "current_date" JsonDecode.string)
        |> decodeApply (field "target_calories" JsonDecode.int)
        |> decodeApply (field "target_fat" JsonDecode.int)
        |> decodeApply (field "target_protein" JsonDecode.int)
        |> decodeApply (field "target_carbohydrate" JsonDecode.int)



-- Journals


type alias JournalEntry =
    { id : String
    , text : String
    , timestamp : String
    , quantity : Float
    , quantityUnits : String
    , calories : Int
    , carbohydrate : Int
    , fat : Int
    , protein : Int
    }


loadJournal : User -> (Result Http.Error (List JournalEntry) -> msg) -> Cmd msg
loadJournal user handler =
    fjHttpGet
        { path = "/journal"
        , decoder = field "records" (JsonDecode.list journalEntryDecoder)
        , handleResult = handler
        , user = Just user
        }


journalEntryDecoder : Decoder JournalEntry
journalEntryDecoder =
    JsonDecode.succeed JournalEntry
        |> decodeApply (field "id" JsonDecode.string)
        |> decodeApply (field "text" JsonDecode.string)
        |> decodeApply (field "timestamp" JsonDecode.string)
        |> decodeApply (field "qty" JsonDecode.float)
        |> decodeApply (field "qty_units" JsonDecode.string)
        |> decodeApply (field "calories" JsonDecode.int)
        |> decodeApply (field "carbohydrate" JsonDecode.int)
        |> decodeApply (field "fat" JsonDecode.int)
        |> decodeApply (field "protein" JsonDecode.int)



-- End Day


endDay : User -> (Result Http.Error String -> msg) -> Cmd msg
endDay user handler =
    fjHttpGet
        { path = "/end-day"
        , decoder = field "current_date" JsonDecode.string
        , handleResult = handler
        , user = Just user
        }
