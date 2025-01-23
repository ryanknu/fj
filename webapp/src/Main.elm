module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, h1, h2, img, input, label, main_, nav, node, option, select, text)
import Html.Attributes as Attributes exposing (class, disabled, placeholder, selected, src, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
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
    | JournalScreen User


type alias RegisteredUsersWrapper =
    { users : Array User }


type alias User =
    { image : String
    , userName : String
    , displayName : String
    , targetCalories : Int
    , targetFat : Int
    , targetProtein : Int
    , targetCarbohydrate : Int
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
    , rgGoal = "Maintain"
    }


defaultModel : Model
defaultModel =
    { commState = WorkingOn LoadingRegisteredUsers
    , user = Nothing
    , inputs = defaultInputs
    , allUsers = []
    , registering = False
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
            JournalScreen user

        Nothing ->
            if m.registering then
                RegisterUserScreen

            else
                SelectUserScreen



-- On init, load list of users registered with the back end.


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, loadUsers )



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
    | TxtStateRgGenderMale String
    | TxtStateRgGenderFemale String
    | TxtStateRgAge String
    | TxtStateRgWantsWeightLossYes String
    | TxtStateRgWantsWeightLossNo String
      -- Registration image handler
    | RgImageRequested
    | RgImageSelected File
    | RgImageLoaded String
      -- Register new user messages
    | GotoRegistration
    | RegisterUser
    | UserRegistered (Result Http.Error User)
      -- Journals
    | LoadJournal (Result Http.Error String)


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
            ( { model | user = Just user }, Cmd.none )

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

        TxtStateRgGenderMale _ ->
            ( { model | inputs = model.inputs |> setRgGender "Male" }, Cmd.none )

        TxtStateRgGenderFemale _ ->
            ( { model | inputs = model.inputs |> setRgGender "Female" }, Cmd.none )

        TxtStateRgWantsWeightLossYes _ ->
            ( { model | inputs = model.inputs |> setRgGoal "LoseWeight" }, Cmd.none )

        TxtStateRgWantsWeightLossNo _ ->
            ( { model | inputs = model.inputs |> setRgGoal "Maintain" }, Cmd.none )

        -- Register new user
        RegisterUser ->
            case modelToRegisterUserInputs model of
                Just inputs ->
                    ( { model | commState = WorkingOn RegisteringUser }, registerUser inputs )

                Nothing ->
                    ( { model | error = Just "Cannot submit due to invalid values." }, Cmd.none )

        GotoRegistration ->
            ( { model | registering = True }, Cmd.none )

        -- TODO: clear all the inputs while transitioning back to registering = false. Can do this in loadRegisteredUsers though.
        -- TODO: we also need to call loadJournal here, so, the result here needs to have a User.
        UserRegistered result ->
            case result of
                Ok user ->
                    ( { model | registering = False, commState = WorkingOn LoadingJournal }, loadJournal user )

                Err n ->
                    ( { model | commState = Error (errorToString n) }, Cmd.none )

        LoadJournal result ->
            ( model, Cmd.none )



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
    input,select { font-size: 1em; outline: 1px solid black; width: 100%; padding: 4px 10px; border-radius: 6px; margin-bottom: 1em; }
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

    .userCircle { display: inline-block; position: relative; width: 200px; height: 200px; border-radius: 50%; }
    .userCircle img { position: absolute; height: 100%; width: 100%; border-radius: 50%; }
    .userCircle > div { position: absolute; bottom: -1em; width: 100%; text-align: center; }
    .userCircle.sm { position: relative; width: 100px; height: 100px; }

    .height div { min-width: 75px; }
    .text-right { text-align: right; }

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
    div [ class "userCircle sm", onClick (SelectUser user) ]
        [ img [ src user.image ] []
        , div [] [ text user.displayName ]
        ]


userPickerView : Model -> Html Msg
userPickerView model =
    div []
        [ h2 [] [ text "Select User" ]
        , paddedView <|
            div [ class "flex pb-4" ]
                (if isEmpty model.allUsers then
                    [ div [] [ text "There are no users" ] ]

                 else
                    map userPickerChoiceView model.allUsers
                )
        , button [ onClick GotoRegistration ] [ text "Register a new user" ]
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



-- RK: TODOs
-- Inline styles for validity would be a nice touch.
-- It'd be nice to show a camera icon on the circle if it is default.
-- When the message "there are no users" appears, make the box 118px tall and center the text vertically.
-- Put selectedUser in localStorage
-- Register new user needs a back or cancel button.


registerUserView : AllInputs -> Maybe String -> Html Msg
registerUserView inputs error =
    div []
        [ h2 [] [ text "Set up new user" ]
        , div [ class "flex pb-4" ]
            [ div [] [ text "Select photo" ]
            , div [ class "flex-grow" ] []
            , div [ class "userCircle sm", onClick RgImageRequested ]
                [ img [ src inputs.rgImage ] []
                ]
            ]
        , simpleInput "Username" inputs.rgUserName TxtStateRgUserName
        , simpleInput "Display Name" inputs.rgDisplayName TxtStateRgDisplayName
        , div [ class "flex height" ]
            [ div [] []
            , label []
                [ input [ type_ "radio", Attributes.name "gdr", onInput TxtStateRgGenderMale ] []
                , text "Male"
                ]
            , label []
                [ input [ type_ "radio", Attributes.name "gdr", onInput TxtStateRgGenderFemale ] []
                , text "Female"
                ]
            ]
        , div [ class "flex height" ]
            [ div [] [ text "Age:" ]
            , input [ type_ "range", Attributes.min "20", Attributes.max "80", value inputs.rgAge, onInput TxtStateRgAge ] []
            , div [ class "text-right" ] [ text inputs.rgAge ]
            ]
        , div [ class "flex height" ]
            [ div [] [ text "Height:" ]
            , input [ type_ "range", Attributes.min "92", Attributes.max "243", value inputs.rgHeight, onInput TxtStateRgHeight ] []
            , div [ class "text-right" ] [ text <| centimetersToFtInView inputs.rgHeight ]
            ]
        , div [ class "flex height" ]
            [ div [] [ text "Weight:" ]
            , input [ type_ "range", Attributes.min "45", Attributes.max "137", value inputs.rgWeight, onInput TxtStateRgWeight ] []
            , div [ class "text-right" ] [ text <| kilogramsToLVbView inputs.rgWeight ]
            ]
        , div [ class "flex height" ]
            [ div [] []
            , label []
                [ input [ type_ "radio", Attributes.name "wwl", onInput TxtStateRgWantsWeightLossNo ] []
                , text "Maintain Weight"
                ]
            , label []
                [ input [ type_ "radio", Attributes.name "wwl", onInput TxtStateRgWantsWeightLossYes ] []
                , text "Lose Weight"
                ]
            ]
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
            [ disabled (not (rgIsValid inputs)), onClick RegisterUser ]
            [ text "Create User" ]
        ]


mainJournalView : User -> Html Msg
mainJournalView user =
    div [] [ text ("Logged in as: " ++ user.displayName) ]


phoneWidthView : Html Msg -> Html Msg
phoneWidthView html =
    div [ class "phone-width" ] [ html ]


paddedView : Html Msg -> Html Msg
paddedView html =
    div [ class "p-4" ] [ html ]


view : Model -> Html Msg
view model =
    main_ []
        [ nav []
            [ paddedView <| h1 [] [ text "Food Journal" ]
            ]
        , errorNode model
        , phoneWidthView <|
            paddedView <|
                case screen model of
                    SelectUserScreen ->
                        userPickerView model

                    RegisterUserScreen ->
                        registerUserView model.inputs model.error

                    JournalScreen user ->
                        mainJournalView user
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

        -- TODO: Coordinate with FjError on the back end to send readable messages to the front end.
        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

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


modelToRegisterUserInputs : Model -> Maybe RegisterUserInputs
modelToRegisterUserInputs model =
    case ( parseInt model.inputs.rgAge, parseInt model.inputs.rgHeight, parseInt model.inputs.rgWeight ) of
        ( Ok age, Ok height, Ok weight ) ->
            Just
                { image = model.inputs.rgImage
                , userName = model.inputs.rgUserName
                , displayName = model.inputs.rgDisplayName
                , gender = model.inputs.rgGender
                , age = age
                , height = height
                , weight = weight
                , goal = model.inputs.rgGoal
                , factor = model.inputs.rgActivityFactor
                }

        _ ->
            Nothing


registerUser : RegisterUserInputs -> Cmd Msg
registerUser inputs =
    Http.post
        { url = "http://localhost:8080/v1/register"
        , body = Http.jsonBody <| encodeRegisterUserInputs inputs
        , expect = Http.expectJson UserRegistered userDecoder
        }


loadUsers : Cmd Msg
loadUsers =
    Http.get
        { url = "http://localhost:8080/v1/users"
        , expect = Http.expectJson LoadRegisteredUsers registeredUsersDecoder
        }


loadJournal : User -> Cmd Msg
loadJournal user =
    Http.request
        { method = "GET"
        , headers =
            [ header "x-fj-user" user.userName
            ]
        , url = "http://localhost:8080/v1/journal"
        , body = emptyBody
        , expect = Http.expectJson LoadJournal JsonDecode.string
        , timeout = Nothing
        , tracker = Nothing
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
        (field "target_calories" JsonDecode.int)
        (field "target_fat" JsonDecode.int)
        (field "target_protein" JsonDecode.int)
        (field "target_carbohydrate" JsonDecode.int)
