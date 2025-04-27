module Registration exposing (Model, Msg(..), defaultInputs, update, view)

import Api exposing (CommState(..), JobTypes(..), RegisterUserInputs, User, WeightLossGoal(..), errorToString)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, b, button, div, h1, h2, img, input, main_, nav, node, option, rt, select, text)
import Html.Attributes as Attributes exposing (class, classList, disabled, placeholder, selected, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import ParseInt exposing (parseInt)
import Regex exposing (Regex)
import Task


type alias Model =
    { userName : String
    , displayName : String
    , image : String
    , height : String
    , weight : String
    , activityFactor : String
    , age : String
    , gender : String
    , goal : String
    , problems : List Problem
    , completed : Bool
    , commState : CommState
    }


defaultInputs : Model
defaultInputs =
    { userName = ""
    , displayName = ""
    , image = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAMAAAAoyzS7AAAAA1BMVEX09PYxuZVGAAAADUlEQVR42gECAP3/AAAAAgABUyucMAAAAABJRU5ErkJggg=="
    , height = "175"
    , weight = "72"
    , activityFactor = "LightActivity"
    , age = "35"
    , gender = "Male"
    , goal = "Maintain Weight"
    , problems = []
    , completed = False
    , commState = Idle
    }


type Problem
    = UploadedImageTooLarge
    | InvalidInputSomewhere
    | HttpError Http.Error


type Msg
    = EnteredUserName String
    | EnteredDisplayName String
    | EnteredHeight String
    | EnteredWeight String
    | SelectedActivityFactor String
    | SelectedGender String
    | EnteredAge String
    | SelectedGoal String
      -- Image handlers
    | ImageRequested
    | ImageSelected File
    | ImageLoaded String
      -- Completion
    | RegisterUser
    | RegistrationProcessed (Result Http.Error User)
    | RegistrationCompleted User


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageRequested ->
            ( model, Select.file [ "image/png", "image/jpeg" ] ImageSelected )

        ImageSelected file ->
            ( model, Task.perform ImageLoaded (File.toUrl file) )

        ImageLoaded content ->
            if String.length content > 10240 then
                -- TODO: Should append to list instead.
                ( { model | problems = [ UploadedImageTooLarge ] }, Cmd.none )

            else
                ( { model | image = content }, Cmd.none )

        EnteredUserName value ->
            ( { model | userName = value }, Cmd.none )

        EnteredDisplayName value ->
            ( { model | displayName = value }, Cmd.none )

        EnteredHeight value ->
            ( { model | height = value }, Cmd.none )

        EnteredWeight value ->
            ( { model | weight = value }, Cmd.none )

        SelectedActivityFactor value ->
            ( { model | activityFactor = value }, Cmd.none )

        EnteredAge value ->
            ( { model | age = value }, Cmd.none )

        SelectedGender value ->
            ( { model | gender = value }, Cmd.none )

        SelectedGoal value ->
            ( { model | goal = value }, Cmd.none )

        RegisterUser ->
            case modelToRegisterUserInputs model of
                Just inputs ->
                    ( { model | commState = WorkingOn RegisteringUser }, Api.registerUser inputs RegistrationProcessed )

                Nothing ->
                    ( { model | problems = [ InvalidInputSomewhere ] }, Cmd.none )

        RegistrationProcessed (Ok user) ->
            ( model, Task.succeed () |> Task.perform (always (RegistrationCompleted user)) )

        RegistrationProcessed (Err e) ->
            ( { model | problems = [ HttpError e ] }, Cmd.none )

        -- This case is not meant to be handled here.
        RegistrationCompleted _ ->
            ( model, Cmd.none )


userNameValidator : Regex
userNameValidator =
    Maybe.withDefault Regex.never <| Regex.fromString "^[a-z]+$"


isValid : Model -> Bool
isValid inputs =
    (String.length inputs.userName > 0)
        && (String.length inputs.displayName > 0)
        && (inputs.image /= defaultInputs.image)
        -- TODO: validate ALL inputs!
        -- TODO: return a (List (String, String)) of errors, or something
        && Regex.contains userNameValidator inputs.userName


problemToString : Problem -> String
problemToString p =
    case p of
        UploadedImageTooLarge ->
            "Image uploaded is too large. Please provide an image 10K or smaller."

        InvalidInputSomewhere ->
            "Cannot create user because invalid values were provided."

        HttpError e ->
            errorToString e


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Start a new journal" ]
        , div [ class "flex pb-4", onClick ImageRequested ]
            [ div []
                [ div [] [ text "Select photo" ]
                , div [ class "text-sm" ] [ text "Square, 10Kb max" ]
                ]
            , div [ class "flex-grow" ] []
            , div [ class "userCircle" ]
                [ img [ src model.image ] []
                ]
            ]
        , simpleInput "Username" model.userName EnteredUserName
        , simpleInput "Display Name" model.displayName EnteredDisplayName
        , radiosView [ "Male", "Female" ] model.gender SelectedGender
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Age:" ]
            , input [ type_ "range", Attributes.min "20", Attributes.max "80", value model.age, onInput EnteredAge ] []
            , div [ class "text-right" ] [ text model.age ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Height:" ]
            , input [ type_ "range", Attributes.min "92", Attributes.max "243", value model.height, onInput EnteredHeight ] []
            , div [ class "text-right" ] [ text <| centimetersToFtInView model.height ]
            ]
        , div [ class "flex slider-row pb-4" ]
            [ div [] [ text "Weight:" ]
            , input [ type_ "range", Attributes.min "45", Attributes.max "137", value model.weight, onInput EnteredWeight ] []
            , div [ class "text-right" ] [ text <| kilogramsToLVbView model.weight ]
            ]
        , radiosView [ "Maintain Weight", "Lose Weight" ] model.goal SelectedGoal
        , div []
            [ select [ onInput SelectedActivityFactor ]
                [ option [ value "Sedentary" ] [ text "Sedentary (no exercise; desk job)" ]
                , option [ value "LightActivity", selected True ] [ text "Light Activity (exercise 1-3 days per week)" ]
                , option [ value "ModerateActivity" ] [ text "Moderate Activity (exercise 3-5 days per week)" ]
                , option [ value "VeryActive" ] [ text "Very Active (exercise 6-7 days per week)" ]
                , option [ value "ExtraActive" ] [ text "Extra Active (exercise 2x per day)" ]
                ]
            ]

        -- TODO: support more than one error
        , txtErrorNode (List.head model.problems)
        , button
            [ disabled <| not <| isValid model, onClick RegisterUser ]
            [ text "Create Journal" ]
        ]


simpleInput : String -> String -> (String -> Msg) -> Html Msg
simpleInput lbl val onChange =
    div []
        [ input [ type_ "text", placeholder lbl, value val, onInput onChange ] []
        ]


radiosView : List String -> String -> (String -> Msg) -> Html Msg
radiosView options selected onChoose =
    div [ class "radios text-sm" ] <|
        List.map
            (\lbl -> div [ onClick (onChoose lbl) ] [ radioOptionView ( lbl, selected == lbl ) ])
            options


radioOptionView : ( String, Bool ) -> Html Msg
radioOptionView ( lbl, isActive ) =
    div
        [ classList [ ( "active", isActive ) ] ]
        [ paddedView <| text lbl ]


paddedView : Html Msg -> Html Msg
paddedView html =
    div [ class "p-4" ] [ html ]


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


txtErrorNode : Maybe Problem -> Html Msg
txtErrorNode maybeProblem =
    case maybeProblem of
        Just problem ->
            div [ class "error pb-4" ] [ text ("Error: " ++ problemToString problem) ]

        Nothing ->
            div [] []


parseGoal : String -> Maybe WeightLossGoal
parseGoal input =
    case input of
        "Maintain Weight" ->
            Just MaintainWeight

        "Lose Weight" ->
            Just LoseWeight

        _ ->
            Nothing


modelToRegisterUserInputs : Model -> Maybe RegisterUserInputs
modelToRegisterUserInputs model =
    case ( parseInt model.age, parseInt model.height, parseInt model.weight ) of
        ( Ok age, Ok height, Ok weight ) ->
            -- TODO this nested case is because Elm won't support tuples of > 3 elements. We should fix this properly.
            case parseGoal model.goal of
                Just goal ->
                    Just
                        { image = model.image
                        , userName = model.userName
                        , displayName = model.displayName
                        , gender = model.gender
                        , age = age
                        , height = height
                        , weight = weight
                        , goal = goal
                        , factor = model.activityFactor
                        }

                _ ->
                    Nothing

        _ ->
            Nothing
