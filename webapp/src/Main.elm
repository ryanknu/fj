module Main exposing (Model, Msg(..), init, main, update, view)

import Api exposing (CommState(..), JobTypes(..), JournalEntry, User, endDay, errorToString, loadJournal, loadUsers)
import Browser
import Date
import File.Select as Select
import Html exposing (Html, b, button, div, h1, h2, img, input, main_, nav, node, option, rt, select, text)
import Html.Attributes as Attributes exposing (class, classList, disabled, placeholder, selected, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JsonDecode exposing (Decoder, field)
import Json.Encode as JsonEncode
import List exposing (isEmpty, map)
import ParseInt exposing (parseInt)
import Regex
import Registration exposing (Msg(..), update, view)
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
    , reg : Registration.Model
    , allUsers : List User
    , registering : Bool
    , composing : Bool
    , error : Maybe String
    , debug : String
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


type Screens
    = SelectUserScreen
    | RegisterUserScreen
    | JournalScreen
    | Composing


defaultModel : Model
defaultModel =
    { commState = WorkingOn LoadingRegisteredUsers
    , user = Nothing
    , reg = Registration.defaultInputs
    , allUsers = []
    , registering = False
    , composing = False
    , error = Nothing
    , debug = ""
    }


setCurrentDate : User -> String -> User
setCurrentDate user newDate =
    { user | currentDate = newDate }


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
    ( defaultModel, loadUsers LoadRegisteredUsers )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = LoadRegisteredUsers (Result Http.Error (List User))
    | SelectUser User
      -- Register new user messages
    | RegisterMsg Registration.Msg
    | GotoRegistration
    | GotoCompose
      -- Journals
    | EndDayRequest
    | EndDayResponse (Result Http.Error String)
    | LoadJournal User (Result Http.Error (List JournalEntry))
    | DoNothing String


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
            ( { model | commState = WorkingOn LoadingJournal }, loadJournal user (LoadJournal user) )

        -- Register new user
        GotoRegistration ->
            ( { model | registering = True }, Cmd.none )

        GotoCompose ->
            ( { model | composing = True }, Cmd.none )

        EndDayRequest ->
            case model.user of
                Nothing ->
                    ( { model | error = Just "Could not end day. User not chosen." }, Cmd.none )

                -- TODO: Set a working indicator, in case back end is slow.
                Just user ->
                    ( model, endDay user EndDayResponse )

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

        -- User successfully registered
        RegisterMsg (RegistrationCompleted user) ->
            ( { model | reg = Registration.defaultInputs, registering = False, commState = WorkingOn LoadingJournal }, loadJournal user (LoadJournal user) )

        -- Passthrough all other registration messages to module.
        RegisterMsg subMsg ->
            let
                ( resModel, resMsg ) =
                    Registration.update subMsg model.reg
            in
            ( { model | reg = resModel }, Cmd.map (\n -> RegisterMsg n) resMsg )

        DoNothing _ ->
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



-- RK: TODOs
-- Inline styles for validity would be a nice touch.
-- It'd be nice to show a camera icon on the circle if it is default.
-- When the message "there are no users" appears, make the box 118px tall and center the text vertically.
-- Put selectedUser in localStorage
-- Register new user needs a back or cancel button.
-- Icons: fat: bottle-droplet | carb: bowl-rice | protein: bacon?


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

        --, simpleInput "Enter food name" "" TxtStateRgUserName
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Amount:" ]
            , input [ type_ "range", Attributes.min "0", Attributes.max "100", value "", onInput DoNothing ] []
            , div [ class "text-right" ] [ text <| "0" ]
            , select [] <|
                map
                    (\v -> option [] [ text v ])
                    [ "Cups", "Grams", "Ounces", "Pounds" ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Calories:" ]
            , input [ type_ "range", Attributes.min "10", Attributes.max "200", value "", onInput DoNothing ] []
            , div [ class "text-right" ] [ text <| "0" ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Carbohydrate:" ]
            , input [ type_ "range", Attributes.min "10", Attributes.max "200", value "", onInput DoNothing ] []
            , div [ class "text-right" ] [ text <| "0" ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Fat:" ]
            , input [ type_ "range", Attributes.min "10", Attributes.max "200", value "", onInput DoNothing ] []
            , div [ class "text-right" ] [ text <| "0" ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Protein:" ]
            , input [ type_ "range", Attributes.min "10", Attributes.max "200", value "", onInput DoNothing ] []
            , div [ class "text-right" ] [ text <| "0" ]
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
                        Html.map (\n -> RegisterMsg n) (Registration.view model.reg)

                    JournalScreen ->
                        journalView

                    Composing ->
                        composeView
        , div [] [ text model.debug ]
        , node "style" [] [ text css ]
        , node "meta" [ Attributes.name "viewport", Attributes.attribute "content" "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no" ] []
        ]
