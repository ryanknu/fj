module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Date
import File exposing (File)
import File.Select as Select
import Html exposing (Html, b, button, div, h1, h2, img, input, main_, nav, node, option, rt, select, text)
import Html.Attributes as Attributes exposing (class, disabled, placeholder, selected, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (emptyBody, header)
import Json.Decode as JsonDecode exposing (Decoder, field)
import Json.Encode as JsonEncode
import List exposing (isEmpty, map)
import ParseInt exposing (parseInt)
import Regex
import String exposing (length)
import Task



-- MAIN
-- TODO: Import Url.Builder and use relative urls


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
    , composing : Bool
    , error : Maybe String
    , debug : String
    }


type alias AllInputs =
    { rgUserName : String
    , rgDisplayName : String
    , rgImage : String
    , rgHeight : String
    , rgWeight : String
    , rgActivityFactor : String
    , rgAge : String
    , rgGender : String
    , rgGoal : String
    }


type alias NewJournalEntryModel =
    { name : String
    , quantity : Float
    , quantityUnits : String
    , calories : Int
    , carbohydrates : Int
    , fat : Int
    , protein : Int
    }


type JobTypes
    = LoadingRegisteredUsers
    | RegisteringUser
    | LoadingJournal


type CommState
    = Loading
    | WorkingOn JobTypes
    | Error String
    | Idle


type Screens
    = SelectUserScreen
    | RegisterUserScreen
    | JournalScreen
    | Composing


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


defaultInputs : AllInputs
defaultInputs =
    { rgUserName = ""
    , rgDisplayName = ""
    , rgImage = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAMAAAAoyzS7AAAAA1BMVEX09PYxuZVGAAAADUlEQVR42gECAP3/AAAAAgABUyucMAAAAABJRU5ErkJggg=="
    , rgHeight = "175"
    , rgWeight = "72"
    , rgActivityFactor = "LightActivity"
    , rgAge = "35"
    , rgGender = "Male"
    , rgGoal = "Maintain Weight"
    }


defaultModel : Model
defaultModel =
    { commState = WorkingOn LoadingRegisteredUsers
    , user = Nothing
    , inputs = defaultInputs
    , allUsers = []
    , registering = False
    , composing = False
    , error = Nothing
    , debug = ""
    }


setRgUserName : String -> AllInputs -> AllInputs
setRgUserName value e =
    { e | rgUserName = value }


rgUserNameValidator : Regex.Regex
rgUserNameValidator =
    Maybe.withDefault Regex.never <| Regex.fromString "^[a-z]+$"


setRgDisplayName : String -> AllInputs -> AllInputs
setRgDisplayName value e =
    { e | rgDisplayName = value }


setRgHeight : String -> AllInputs -> AllInputs
setRgHeight value e =
    { e | rgHeight = value }


setRgWeight : String -> AllInputs -> AllInputs
setRgWeight value e =
    { e | rgWeight = value }


setRgActivityFactor : String -> AllInputs -> AllInputs
setRgActivityFactor value e =
    { e | rgActivityFactor = value }


setRgAge : String -> AllInputs -> AllInputs
setRgAge value e =
    { e | rgAge = value }


setRgGender : String -> AllInputs -> AllInputs
setRgGender value e =
    { e | rgGender = value }


setRgGoal : String -> AllInputs -> AllInputs
setRgGoal value e =
    { e | rgGoal = value }


setRgImage : String -> AllInputs -> AllInputs
setRgImage value e =
    { e | rgImage = value }


setCurrentDate : User -> String -> User
setCurrentDate user newDate =
    { user | currentDate = newDate }


rgIsValid : AllInputs -> Bool
rgIsValid inputs =
    (length inputs.rgUserName > 0)
        && (length inputs.rgDisplayName > 0)
        && (inputs.rgImage /= defaultInputs.rgImage)
        -- TODO: validate ALL inputs!
        -- TODO: return a (List (String, String)) of errors, or something
        && Regex.contains rgUserNameValidator inputs.rgUserName


screen : Model -> Screens
screen m =
    case m.user of
        Just user ->
            if m.composing then
                Composing

            else
                JournalScreen

        Nothing ->
            if m.registering then
                RegisterUserScreen

            else
                SelectUserScreen



-- On init, load list of users registered with the back end.


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, fjApiLoadUsers )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = LoadRegisteredUsers (Result Http.Error (List User))
    | SelectUser User
      -- Text input key handlers
    | TxtStateRgUserName String
    | TxtStateRgDisplayName String
    | TxtStateRgHeight String
    | TxtStateRgWeight String
    | TxtStateRgActivityFactor String
    | TxtStateRgGender String
    | TxtStateRgAge String
    | TxtStateRgGoal String
      -- Registration image handler
    | RgImageRequested
    | RgImageSelected File
    | RgImageLoaded String
      -- Register new user messages
    | GotoRegistration
    | RegisterUser
    | UserRegistered (Result Http.Error User)
    | GotoCompose
      -- Journals
    | EndDayRequest
    | EndDayResponse (Result Http.Error String)
    | LoadJournal User (Result Http.Error (List JournalEntry))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadRegisteredUsers result ->
            case result of
                Ok allUsers ->
                    ( { model | commState = Idle, allUsers = allUsers }, Cmd.none )

                Err n ->
                    ( { model | commState = Error (errorToString n) }, Cmd.none )

        SelectUser user ->
            ( { model | commState = WorkingOn LoadingJournal }, fjApiLoadJournal user )

        RgImageRequested ->
            ( model, Select.file [ "image/png", "image/jpeg" ] RgImageSelected )

        RgImageSelected file ->
            ( model, Task.perform RgImageLoaded (File.toUrl file) )

        RgImageLoaded content ->
            if length content > 10240 then
                ( { model | error = Just "Image uploaded is too large. Please provide an image 10K or smaller." }, Cmd.none )

            else
                ( { model | inputs = model.inputs |> setRgImage content }, Cmd.none )

        TxtStateRgUserName value ->
            ( { model | inputs = model.inputs |> setRgUserName value }, Cmd.none )

        TxtStateRgDisplayName value ->
            ( { model | inputs = model.inputs |> setRgDisplayName value }, Cmd.none )

        TxtStateRgHeight value ->
            ( { model | inputs = model.inputs |> setRgHeight value }, Cmd.none )

        TxtStateRgWeight value ->
            ( { model | inputs = model.inputs |> setRgWeight value }, Cmd.none )

        TxtStateRgActivityFactor value ->
            ( { model | inputs = model.inputs |> setRgActivityFactor value }, Cmd.none )

        TxtStateRgAge value ->
            ( { model | inputs = model.inputs |> setRgAge value }, Cmd.none )

        TxtStateRgGender value ->
            ( { model | inputs = model.inputs |> setRgGender value }, Cmd.none )

        TxtStateRgGoal value ->
            ( { model | inputs = model.inputs |> setRgGoal value }, Cmd.none )

        -- Register new user
        RegisterUser ->
            case modelToRegisterUserInputs model of
                Just inputs ->
                    ( { model | commState = WorkingOn RegisteringUser }, fjApiRegisterUser inputs )

                Nothing ->
                    ( { model | error = Just "Cannot create user because invalid values were provided." }, Cmd.none )

        GotoRegistration ->
            ( { model | registering = True }, Cmd.none )

        -- TODO: clear all the inputs while transitioning back to registering = false. Can do this in loadRegisteredUsers though.
        UserRegistered result ->
            case result of
                Ok user ->
                    ( { model | registering = False, commState = WorkingOn LoadingJournal }, fjApiLoadJournal user )

                Err n ->
                    ( { model | commState = Error (errorToString n) }, Cmd.none )

        GotoCompose ->
            ( { model | composing = True }, Cmd.none )

        EndDayRequest ->
            case model.user of
                Nothing ->
                    ( { model | error = Just "Could not end day. User not chosen." }, Cmd.none )

                -- TODO: Set a working indicator, in case back end is slow.
                Just user ->
                    ( model, fjApiEndDay user )

        EndDayResponse result ->
            case ( result, model.user ) of
                ( Ok date, Just user ) ->
                    ( { model | commState = Idle, user = Just <| setCurrentDate user date }, Cmd.none )

                ( Err n, _ ) ->
                    ( { model | commState = Error <| "Coult not end day. " ++ errorToString n }, Cmd.none )

                _ ->
                    ( { model | commState = Error "Could not end day. An unexpected error occurred." }, Cmd.none )

        LoadJournal user result ->
            case result of
                Ok r ->
                    ( { model | user = Just user }, Cmd.none )

                Err n ->
                    ( { model | commState = Error <| "Could not load journal. " ++ errorToString n }, Cmd.none )



-- VIEW


css : String
css =
    """
    html,body,p,ol,ul,li,dl,dt,dd,blockquote,figure,fieldset,legend,textarea,pre,iframe,hr,h1,h2,h3,h4,h5,h6{margin:0;padding:0}h1,h2,h3,h4,h5,h6{font-size:100%;font-weight:normal}ul{list-style:none}button,input,select{margin:0}html{box-sizing:border-box}*,*::before,*::after{box-sizing:inherit}img,video{height:auto;max-width:100%}iframe{border:0}table{border-collapse:collapse;border-spacing:0}td,th{padding:0}

    /* This is meant to be viewed on a phone */
    main { width: 100%; }
    .phone-width { max-width: 450px; margin: auto; }
    body { font-family: sans-serif; font-size: 14pt; }
    button { background-color: rgb(22, 163, 74); }
    select { --webkit-appearance: none; }

    /* Dark mode color scheme */
    @media (prefers-color-scheme: dark) {
        body { background-color: rgb(29, 38, 47); color: white; }
        input,select { color: white; background-color: rgb(43, 54, 66); }
        input,button,select { border: 1px solid rgba(255, 255, 255, 0.157); color: white; }
    }

    nav { width: 100%; background-color: rgb(16, 22, 29); }

    /* Typeography */
    h1 { font-size: 1.6em; }
    h2 { font-size: 1.4em; }
    h3 { font-size: 1.1em; }

    /* Very basic input styles */
    input,select { font-size: 1em; width: 100%; padding: 4px 10px; border-radius: 6px; margin-bottom: 1em; }
    input[type=range] { margin-bottom: 0; accent-color: rebeccapurple; }
    button { font-size: 1em; outline: 1px solid black; width: 100%; padding: 4px 10px; border-radius: 6px; }
    button:hover { cursor: pointer; }
    button:hover,input:hover { opacity: 90%; }
    button:active { opacity: 100%; }
    button:disabled { opacity: 60%; cursor: inherit; }
    /* Use outline here so the element doesn't change size */
    :focus-visible { outline: 1px solid rebeccapurple; border: 1px solid transparent; }

    .p-1 { padding: 0.25em; }
    .p-2 { padding: 0.5em; }
    .p-4 { padding: 1em; }
    .pb-1 { padding-bottom: 0.25em; }
    .pb-2 { padding-bottom: 0.5em; }
    .pb-4 { padding-bottom: 1em; }

    .flex { display: flex; align-items: center; justify-content: center }
    .flex-grow { flex-grow: 1; }

    .user-choice-box { gap: 1em; height: 118.667px; }
    .userCircle { display: inline-block; position: relative; width: 100px; height: 100px; border-radius: 50%; }
    .userCircle img { position: absolute; height: 100%; width: 100%; border-radius: 50%; }
    .userCircle > div { position: absolute; bottom: -1em; width: 100%; text-align: center; }
    .userCircle.sm { position: relative; width: 33px; height: 33px; }

    .slider-row > div:first-child { min-width: 100px; }
    .slider-row > div:last-child { min-width: 75px; }

    .text-right { text-align: right; }
    .text-sm { font-size: 0.8em; }

    /* Radio button styles */
    .radios { width: 100%; display: flex; cursor: pointer; }
    .radios { padding-bottom: 1em; }
    .radios > div { background-color: rgb(55, 67, 81); width: 100%; text-align: center; }
    .radios > div:first-child > div { border-top-left-radius: 6px; border-bottom-left-radius: 6px; }
    .radios > div:last-child > div { border-top-right-radius: 6px; border-bottom-right-radius: 6px; }
    .radios > div:first-child { border-top-left-radius: 6px; border-bottom-left-radius: 6px; }
    .radios > div:last-child { border-top-right-radius: 6px; border-bottom-right-radius: 6px; }
    .radios div.active { background-color: rgb(43, 54, 66) }

    .error { background-color: rgb(186, 76, 64); padding: 0.25em; text-align: center; }
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
            div [ class "error" ] [ text ("Communication error: " ++ doing) ]

        Nothing ->
            text ""


txtErrorNode : Maybe String -> Html Msg
txtErrorNode error =
    case error of
        Just msg ->
            div [ class "error pb-4" ] [ text ("Error: " ++ msg) ]

        Nothing ->
            div [] []


simpleInput : String -> String -> (String -> Msg) -> Html Msg
simpleInput lbl val onChange =
    div []
        [ input [ type_ "text", placeholder lbl, value val, onInput onChange ] []
        ]


userPickerChoiceView : User -> Html Msg
userPickerChoiceView user =
    div [ class "userCircle", onClick <| SelectUser user ]
        [ img [ src user.image ] []
        , div [] [ text user.displayName ]
        ]


userPickerView : Model -> Html Msg
userPickerView model =
    div []
        [ h2 [] [ text "Select Journal" ]
        , paddedView <|
            div [ class "flex pb-4 user-choice-box" ]
                (case ( model.commState, isEmpty model.allUsers ) of
                    ( WorkingOn LoadingRegisteredUsers, _ ) ->
                        -- Don't display any text otherwise there's a text flash while loading.
                        [ text "" ]

                    ( _, True ) ->
                        [ text "There are no users" ]

                    ( _, False ) ->
                        map userPickerChoiceView model.allUsers
                )
        , button [ onClick GotoRegistration ] [ text "Start a new journal" ]
        ]


inchToFtIn : Int -> ( Int, Int )
inchToFtIn val =
    ( val // 12, modBy 12 val )


centimetersToFtIn : Int -> ( Int, Int )
centimetersToFtIn val =
    inchToFtIn <| round <| toFloat val * 0.393701


centimetersToFtInView : String -> String
centimetersToFtInView val =
    let
        ( foot, inch ) =
            centimetersToFtIn <| Result.withDefault 175 <| parseInt val
    in
    String.fromInt foot ++ "', " ++ String.fromInt inch ++ "\""


kilogramsToLb : Int -> Int
kilogramsToLb val =
    round <| toFloat val * 2.20462


kilogramsToLVbView : String -> String
kilogramsToLVbView val =
    (String.fromInt <| kilogramsToLb <| Result.withDefault 72 <| parseInt val) ++ "lb"


radiosView : List String -> String -> (String -> Msg) -> Html Msg
radiosView options selected onChoose =
    div [ class "radios text-sm" ] <|
        List.map
            (\lbl -> div [ onClick (onChoose lbl) ] [ radioOptionView ( lbl, selected == lbl ) ])
            options


radioOptionView : ( String, Bool ) -> Html Msg
radioOptionView ( lbl, isActive ) =
    div
        [ class
            (if isActive then
                "active"

             else
                ""
            )
        ]
        [ paddedView <| text lbl ]



-- RK: TODOs
-- Inline styles for validity would be a nice touch.
-- It'd be nice to show a camera icon on the circle if it is default.
-- When the message "there are no users" appears, make the box 118px tall and center the text vertically.
-- Put selectedUser in localStorage
-- Register new user needs a back or cancel button.
-- Icons: fat: bottle-droplet | carb: bowl-rice | protein: bacon?


registerUserView : AllInputs -> Maybe String -> Html Msg
registerUserView inputs error =
    div []
        [ h2 [] [ text "Start a new journal" ]
        , div [ class "flex pb-4", onClick RgImageRequested ]
            [ div []
                [ div [] [ text "Select photo" ]
                , div [ class "text-sm" ] [ text "Square, 10Kb max" ]
                ]
            , div [ class "flex-grow" ] []
            , div [ class "userCircle" ]
                [ img [ src inputs.rgImage ] []
                ]
            ]
        , simpleInput "Username" inputs.rgUserName TxtStateRgUserName
        , simpleInput "Display Name" inputs.rgDisplayName TxtStateRgDisplayName
        , radiosView [ "Male", "Female" ] inputs.rgGender TxtStateRgGender
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Age:" ]
            , input [ type_ "range", Attributes.min "20", Attributes.max "80", value inputs.rgAge, onInput TxtStateRgAge ] []
            , div [ class "text-right" ] [ text inputs.rgAge ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Height:" ]
            , input [ type_ "range", Attributes.min "92", Attributes.max "243", value inputs.rgHeight, onInput TxtStateRgHeight ] []
            , div [ class "text-right" ] [ text <| centimetersToFtInView inputs.rgHeight ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Weight:" ]
            , input [ type_ "range", Attributes.min "45", Attributes.max "137", value inputs.rgWeight, onInput TxtStateRgWeight ] []
            , div [ class "text-right" ] [ text <| kilogramsToLVbView inputs.rgWeight ]
            ]
        , radiosView [ "Maintain Weight", "Lose Weight" ] inputs.rgGoal TxtStateRgGoal
        , div []
            [ select [ onInput TxtStateRgActivityFactor ]
                [ option [ value "Sedentary" ] [ text "Sedentary (no exercise; desk job)" ]
                , option [ value "LightActivity", selected True ] [ text "Light Activity (exercise 1-3 days per week)" ]
                , option [ value "ModerateActivity" ] [ text "Moderate Activity (exercise 3-5 days per week)" ]
                , option [ value "VeryActive" ] [ text "Very Active (exercise 6-7 days per week)" ]
                , option [ value "ExtraActive" ] [ text "Extra Active (exercise 2x per day)" ]
                ]
            ]
        , txtErrorNode error
        , button
            [ disabled <| not <| rgIsValid inputs, onClick RegisterUser ]
            [ text "Create Journal" ]
        ]


currentDateView : User -> Html Msg
currentDateView user =
    div [ class "text-sm" ]
        [ case Date.fromIsoString user.currentDate of
            Ok date ->
                text <| (String.fromInt <| Date.weekdayNumber date) ++ " | " ++ user.currentDate

            Err msg ->
                text <| "error: " ++ msg
        ]


journalView : Html Msg
journalView =
    div []
        [ text <| "Logged in as: "
        , button [ onClick GotoCompose ] [ text "Add Entry" ]
        , button [ onClick EndDayRequest ] [ text "End Day" ]
        ]


composeView : Html Msg
composeView =
    -- TODO: Autocomplete foods that have been entered before -OR- OpenFoodFacts foods.
    div []
        [ h2 [] [ text "New Entry" ]
        , simpleInput "Enter food name" "" TxtStateRgUserName
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Amount:" ]
            , input [ type_ "range", Attributes.min "0", Attributes.max "100", value "", onInput TxtStateRgWeight ] []
            , div [ class "text-right" ] [ text <| kilogramsToLVbView "0" ]
            , select [] <|
                map
                    (\v -> option [] [ text v ])
                    [ "Cups", "Grams", "Ounces", "Pounds" ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Calories:" ]
            , input [ type_ "range", Attributes.min "10", Attributes.max "200", value "", onInput TxtStateRgWeight ] []
            , div [ class "text-right" ] [ text <| kilogramsToLVbView "0" ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Carbohydrate:" ]
            , input [ type_ "range", Attributes.min "10", Attributes.max "200", value "", onInput TxtStateRgWeight ] []
            , div [ class "text-right" ] [ text <| kilogramsToLVbView "0" ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Fat:" ]
            , input [ type_ "range", Attributes.min "10", Attributes.max "200", value "", onInput TxtStateRgWeight ] []
            , div [ class "text-right" ] [ text <| kilogramsToLVbView "0" ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Protein:" ]
            , input [ type_ "range", Attributes.min "10", Attributes.max "200", value "", onInput TxtStateRgWeight ] []
            , div [ class "text-right" ] [ text <| kilogramsToLVbView "0" ]
            ]
        , button [ onClick GotoCompose ] [ text "Enter" ]
        ]


topNavView : Maybe User -> List (Html Msg)
topNavView maybeUser =
    case maybeUser of
        Nothing ->
            [ paddedView <| h1 [] [ text "Food Journal" ]
            , div [ class "flex-grow" ] []
            ]

        Just user ->
            [ paddedSmView <|
                div []
                    [ div [ class "userCircle sm" ]
                        [ img [ src user.image ] []
                        ]
                    ]
            , div [ class "flex-grow" ] []
            , paddedView <| currentDateView <| user
            ]


phoneWidthView : Html Msg -> Html Msg
phoneWidthView html =
    div [ class "phone-width" ] [ html ]


paddedView : Html Msg -> Html Msg
paddedView html =
    div [ class "p-4" ] [ html ]


paddedSmView : Html Msg -> Html Msg
paddedSmView html =
    div [ class "p-2" ] [ html ]


view : Model -> Html Msg
view model =
    main_ []
        [ nav [ class "flex" ] <| topNavView model.user
        , errorNode model
        , phoneWidthView <|
            paddedView <|
                case screen model of
                    SelectUserScreen ->
                        userPickerView model

                    RegisterUserScreen ->
                        registerUserView model.inputs model.error

                    JournalScreen ->
                        journalView

                    Composing ->
                        composeView
        , div [] [ text model.debug ]
        , node "style" [] [ text css ]
        , node "meta" [ Attributes.name "viewport", Attributes.attribute "content" "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no" ] []
        ]



-- HTTP


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


type alias RegisterUserInputs =
    { image : String
    , userName : String
    , displayName : String
    , gender : String
    , age : Int
    , height : Int
    , weight : Int
    , goal : String
    , factor : String
    }


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
        , ( "goal", JsonEncode.string inputs.goal )
        , ( "factor", JsonEncode.string inputs.factor )
        ]


parseRgGoal : String -> Maybe String
parseRgGoal val =
    case val of
        "Maintain Weight" ->
            Just "Maintain"

        "Lose Weight" ->
            Just "LoseWeight"

        _ ->
            Nothing


modelToRegisterUserInputs : Model -> Maybe RegisterUserInputs
modelToRegisterUserInputs model =
    case ( parseInt model.inputs.rgAge, parseInt model.inputs.rgHeight, parseInt model.inputs.rgWeight ) of
        ( Ok age, Ok height, Ok weight ) ->
            -- TODO this nested case is because Elm won't support tuples of > 3 elements. We should fix this properly.
            case parseRgGoal model.inputs.rgGoal of
                Just goal ->
                    Just
                        { image = model.inputs.rgImage
                        , userName = model.inputs.rgUserName
                        , displayName = model.inputs.rgDisplayName
                        , gender = model.inputs.rgGender
                        , age = age
                        , height = height
                        , weight = weight
                        , goal = goal
                        , factor = model.inputs.rgActivityFactor
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


fjHttpRequest :
    { method : String
    , headers : List Http.Header
    , path : String
    , body : Http.Body
    , expect : Http.Expect Msg
    , user : Maybe User
    }
    -> Cmd Msg
fjHttpRequest r =
    Http.request
        { method = r.method
        , headers =
            -- TODO: add universal accept header; add content-type header for post
            List.append r.headers <| Maybe.withDefault [] <| Maybe.map (\n -> [ header "x-fj-user" n.userName ]) r.user

        -- TODO: Manage this URL globally somehow.
        , url = "http://localhost:8080/v1" ++ r.path
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }


fjHttpGet :
    { path : String
    , expect : Http.Expect Msg
    , user : Maybe User
    }
    -> Cmd Msg
fjHttpGet r =
    fjHttpRequest
        { method = "GET"
        , headers = []
        , path = r.path
        , body = emptyBody
        , expect = r.expect
        , user = r.user
        }


fjHttpPost :
    { path : String
    , expect : Http.Expect Msg
    , body : a
    , bodyEncoder : a -> JsonEncode.Value
    , user : Maybe User
    }
    -> Cmd Msg
fjHttpPost r =
    fjHttpRequest
        { method = "POST"
        , headers =
            [ header "content-type" "application/json"
            ]
        , path = r.path
        , body = Http.jsonBody <| r.bodyEncoder r.body
        , expect = r.expect
        , user = r.user
        }


fjApiRegisterUser : RegisterUserInputs -> Cmd Msg
fjApiRegisterUser inputs =
    fjHttpPost
        { path = "/register"
        , body = inputs
        , bodyEncoder = encodeRegisterUserInputs
        , expect = Http.expectJson UserRegistered userDecoder
        , user = Nothing
        }


fjApiLoadUsers : Cmd Msg
fjApiLoadUsers =
    fjHttpGet
        { path = "/users"
        , expect = Http.expectJson LoadRegisteredUsers registeredUsersDecoder
        , user = Nothing
        }


fjApiLoadJournal : User -> Cmd Msg
fjApiLoadJournal user =
    fjHttpGet
        { path = "/journal"
        , expect = Http.expectJson (LoadJournal user) journalDecoder
        , user = Just user
        }


fjApiEndDay : User -> Cmd Msg
fjApiEndDay user =
    fjHttpGet
        { path = "/end-day"
        , expect = Http.expectJson EndDayResponse fjApiDecoderCurrentDate
        , user = Just user
        }



-- JSON DECODER


registeredUsersDecoder : Decoder (List User)
registeredUsersDecoder =
    field "users" (JsonDecode.list userDecoder)


journalDecoder : Decoder (List JournalEntry)
journalDecoder =
    field "records" (JsonDecode.list journalEntryDecoder)


fjApiDecoderCurrentDate : Decoder String
fjApiDecoderCurrentDate =
    field "current_date" JsonDecode.string


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
