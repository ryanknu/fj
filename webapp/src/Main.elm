module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import File.Select as FileSelect
import Html exposing (Html, button, div, fieldset, h1, h2, input, label, legend, main_, node, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JsonDecode exposing (Decoder, field, string)
import Json.Encode as JsonEncode exposing (dict)
import List exposing (isEmpty, map)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { commState : CommState
    , user : Maybe User
    , inputs : AllInputs
    , allUsers : List User
    , registering : Bool
    , workRemaining : Int
    , debug : String
    }


type alias AllInputs =
    { rgUserName : String
    , rgDisplayName : String
    , rgTargetCalories : String
    }


type JobTypes
    = LoadingRegisteredUsers
    | RegisteringUser


type CommState
    = Loading
    | WorkingOn JobTypes
    | Error String
    | Idle


type alias RegisteredUsersWrapper =
    { users : Array User }


type alias User =
    { image : String
    , userName : String
    , displayName : String
    , targetCalories : String
    , targetFat : String
    , targetProtein : String
    , targetCarbohydrate : String
    }


defaultInputs : AllInputs
defaultInputs =
    { rgUserName = ""
    , rgDisplayName = ""
    , rgTargetCalories = "1800"
    }


defaultModel : Model
defaultModel =
    { commState = WorkingOn LoadingRegisteredUsers
    , user = Nothing
    , inputs = defaultInputs
    , allUsers = []
    , registering = False
    , workRemaining = 0
    , debug = ""
    }


setRgUserName : String -> AllInputs -> AllInputs
setRgUserName value e =
    { e | rgUserName = value }


setRgDisplayName : String -> AllInputs -> AllInputs
setRgDisplayName value e =
    { e | rgDisplayName = value }


setRgTargetCalories : String -> AllInputs -> AllInputs
setRgTargetCalories value e =
    { e | rgTargetCalories = value }



-- On init, load list of users registered with the back end.


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel
    , Http.get
        { url = "http://localhost:8080/v1/users"
        , expect = Http.expectJson LoadRegisteredUsers registeredUsersDecoder
        }
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = LoadRegisteredUsers (Result Http.Error (List User))
      -- Text input key handlers
    | TxtStateRgUserName String
    | TxtStateRgDisplayName String
    | TxtStateRgTargetCalories String
      -- Register user: expectWhatever with () for success for 201 CREATED
    | GotoRegistration
    | UserRegistered (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadRegisteredUsers result ->
            case result of
                Ok allUsers ->
                    ( { model | commState = Idle, allUsers = allUsers }, Cmd.none )

                Err n ->
                    ( { model | workRemaining = 0, commState = Error ("loading config. " ++ errorToString n) }, Cmd.none )

        TxtStateRgUserName value ->
            ( { model | inputs = model.inputs |> setRgUserName value }, Cmd.none )

        TxtStateRgDisplayName value ->
            ( { model | inputs = model.inputs |> setRgDisplayName value }, Cmd.none )

        TxtStateRgTargetCalories value ->
            ( { model | inputs = model.inputs |> setRgTargetCalories value }, Cmd.none )

        -- Register new user
        -- RegisterUser ->
        --     ( { model | commState = WorkingOn RegisteringUser }, Cmd.none )
        GotoRegistration ->
            ( { model | registering = True }, Cmd.none )

        UserRegistered result ->
            ( model, Cmd.none )



-- VIEW


css : String
css =
    """
    html,body,p,ol,ul,li,dl,dt,dd,blockquote,figure,fieldset,legend,textarea,pre,iframe,hr,h1,h2,h3,h4,h5,h6{margin:0;padding:0}h1,h2,h3,h4,h5,h6{font-size:100%;font-weight:normal}ul{list-style:none}button,input,select{margin:0}html{box-sizing:border-box}*,*::before,*::after{box-sizing:inherit}img,video{height:auto;max-width:100%}iframe{border:0}table{border-collapse:collapse;border-spacing:0}td,th{padding:0}

    /* This is meant to be viewed on a phone */
    main { width: 100%; max-width: 450px; margin: auto; }
    body { font-family: sans-serif; }

    /* Dark mode color scheme */
    @media (prefers-color-scheme: dark) {
        body { background-color: black; }
        main,input,button { color: white; background-color: black; border-color: white; }
    }

    /* Typeography */
    h1 { font-size: 1.3rem; }
    h2 { font-size: 1.2rem; }
    h3 { font-size: 1.1rem; }

    input { font-size: 1.1rem; border: 1px solid; width: 100%; padding: 4px 10px; border-radius: 6px; margin-bottom: 1rem; }

    .padded { padding: 10px; }
    .userCircle { display: inline-block; position: relative; width: 200px; height: 200px; border-radius: 50%; background-color: gray; }
    .userCircle > div { position: absolute; bottom: 10px; left: 50%; transform: translateX(-50%); }

    .error { color: red; }
    """


reduceCommStateToMaybe : CommState -> Maybe String
reduceCommStateToMaybe model =
    case model of
        Error doing ->
            Just doing

        Idle ->
            Nothing

        WorkingOn _ ->
            Nothing

        Loading ->
            Nothing


errorNode : Model -> Html Msg
errorNode model =
    case reduceCommStateToMaybe model.commState of
        Just doing ->
            div [ class "error" ] [ text ("Error while " ++ doing) ]

        Nothing ->
            text ""


simpleInput : String -> String -> (String -> Msg) -> Html Msg
simpleInput lbl val onChange =
    div []
        [ input [ type_ "text", placeholder lbl, value val, onInput onChange ] []
        ]


userPickerChoiceView : User -> Html Msg
userPickerChoiceView user =
    div [ class "userCircle" ]
        [ div [] [ text user.displayName ]
        ]


userPickerView : Model -> Html Msg
userPickerView model =
    div []
        [ h2 [] [ text "Select User" ]
        , div []
            (case isEmpty model.allUsers of
                True ->
                    [ div [] [ text "There are no users" ] ]

                False ->
                    map userPickerChoiceView model.allUsers
            )
        , button [ onClick GotoRegistration ] [ text "Register a new user" ]
        ]



-- RK: TODO show invalid if taking a username someone else has, or if username is empty.
-- Also, spaces are not allowed, nor are capital letters, or numbers. just [a-z]


registerUserView : AllInputs -> Html Msg
registerUserView inputs =
    div []
        [ h2 [] [ text "Set up new user" ]
        , simpleInput "Username" inputs.rgUserName TxtStateRgUserName
        , simpleInput "Display Name" inputs.rgDisplayName TxtStateRgDisplayName
        , simpleInput "Target Calories" inputs.rgTargetCalories TxtStateRgTargetCalories
        ]



-- RK notes: If model.user is `Just user`, then render the main application. If it is Nothing, show list of registered users.
--           Maybe, we can save the selected user in localStorage.


view : Model -> Html Msg
view model =
    main_ [ class "padded" ]
        [ h1 [] [ text "Food Journal" ]
        , case model.user of
            Just user ->
                div [] []

            Nothing ->
                case model.registering of
                    True ->
                        registerUserView model.inputs

                    False ->
                        userPickerView model
        , div [] [ text model.debug ]
        , node "style" [] [ text css ]
        ]



-- HTTP


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage


registerUser : String -> String -> String -> Int -> Cmd Msg
registerUser userName displayName image targetCalories =
    Http.post
        { url = "http://localhost:8080/v1/register"
        , body =
            Http.jsonBody
                (JsonEncode.object
                    [ ( "image", JsonEncode.string image )
                    , ( "user_name", JsonEncode.string userName )
                    , ( "display_name", JsonEncode.string displayName )
                    , ( "target_calories", JsonEncode.int targetCalories )
                    ]
                )
        , expect = Http.expectWhatever UserRegistered
        }



-- JSON DECODER


registeredUsersDecoder : Decoder (List User)
registeredUsersDecoder =
    field "users" (JsonDecode.list userDecoder)


userDecoder : Decoder User
userDecoder =
    JsonDecode.map7 User
        (field "image" JsonDecode.string)
        (field "user_name" JsonDecode.string)
        (field "display_name" JsonDecode.string)
        (field "target_calories" JsonDecode.string)
        (field "target_fat" JsonDecode.string)
        (field "target_protein" JsonDecode.string)
        (field "target_carbohydrate" JsonDecode.string)
