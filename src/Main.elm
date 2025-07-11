module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Controller exposing (Button(..), Controller)
import Dict
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Game exposing (Game)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Random.List
import Settings exposing (Settings)
import Styles
import Task
import Tetromino exposing (Tetromino)
import Tetromino.Letter as Letter exposing (Letter(..))
import Tetromino.Orientation as Orientation exposing (Orientation(..))
import Time
import Util


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown <| Decode.map KeyDown Controller.keyDecoder
        , Events.onKeyUp <| Decode.map KeyUp Controller.keyDecoder
        , Events.onAnimationFrame Tick
        ]


type Msg
    = KeyDown String
    | KeyUp String
    | Tick Time.Posix
    | ChangedSettings Settings.Msg
    | PlaygroundPressed
    | GameStarted Random.Seed
    | Noop


type alias Model =
    { controller : Controller
    , settings : Settings
    , game : Maybe Game
    , history : List Game
    , future : List Game
    }


init : Decode.Value -> ( Model, Cmd Msg )
init settingsValue =
    let
        settings =
            Decode.decodeValue Settings.decoder settingsValue
                |> Result.withDefault Settings.default
    in
    ( { controller = Controller.default
      , settings = settings
      , game = Nothing
      , history = []
      , future = []
      }
    , Random.generate GameStarted Random.independentSeed
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        GameStarted seed ->
            let
                game =
                    Game.init seed
            in
            ( respondToMsg game Game.SaveGameState model, Cmd.none )

        PlaygroundPressed ->
            ( model, Random.generate GameStarted Random.independentSeed )

        ChangedSettings settingsMsg ->
            let
                ( settings, cmd ) =
                    Settings.update settingsMsg model.settings

                buttonToBlur =
                    case settingsMsg of
                        Settings.SetAll ->
                            "set-all-button"

                        Settings.ClearAll ->
                            "clear-all-button"

                        Settings.ResetDefault ->
                            "reset-default-button"

                        Settings.Minimized ->
                            "minimize-settings-button"

                        Settings.SavedSettings ->
                            "save-settings-button"

                        _ ->
                            ""
            in
            ( { model | settings = settings }
            , Cmd.batch
                [ Task.attempt (\_ -> Noop) (Dom.blur buttonToBlur)
                , cmd
                ]
            )

        Tick time ->
            ( model.game
                |> Maybe.map (Game.tick time model.settings model.controller)
                |> Maybe.map (\( g, m ) -> respondToMsg g m model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        KeyDown key ->
            let
                errorIfPressing button =
                    if Controller.isPressed button model.controller then
                        Nothing

                    else
                        Just button

                onPress button =
                    let
                        controller =
                            Controller.press button model.controller
                    in
                    case model.game of
                        Nothing ->
                            model

                        Just game ->
                            let
                                ( g, m ) =
                                    Game.onPress button model.settings game
                            in
                            respondToMsg g m { model | controller = controller }
            in
            ( case model.settings.setInputMode of
                Settings.Setting button ->
                    { model
                        | settings =
                            model.settings
                                |> Settings.addMapping key button
                                |> Settings.setNextInput
                    }

                Settings.Inactive ->
                    model.settings.controls
                        |> Dict.get key
                        |> Maybe.andThen errorIfPressing
                        |> Maybe.map onPress
                        |> Maybe.withDefault model
            , Cmd.none
            )

        KeyUp key ->
            let
                errorIfReleased button =
                    if Controller.isReleased button model.controller then
                        Nothing

                    else
                        Just button

                onRelease button =
                    let
                        controller =
                            Controller.release button model.controller
                    in
                    case model.game of
                        Nothing ->
                            model

                        Just game ->
                            let
                                ( g, m ) =
                                    ( Game.onRelease button controller model.settings game, Game.Noop )
                            in
                            respondToMsg g m { model | controller = controller }
            in
            ( model.settings.controls
                |> Dict.get key
                |> Maybe.andThen errorIfReleased
                |> Maybe.map onRelease
                |> Maybe.withDefault model
            , Cmd.none
            )


respondToMsg : Game -> Game.Msg -> Model -> Model
respondToMsg game msg model =
    case msg of
        Game.Noop ->
            { model | game = Just game }

        Game.Undo ->
            case model.history of
                current :: prev :: before ->
                    { model
                        | game = Just prev
                        , history = prev :: before
                        , future = current :: model.future
                    }

                _ ->
                    -- TODO: play sound
                    model

        Game.Redo ->
            case model.future of
                next :: after ->
                    { model
                        | game = Just next
                        , history = next :: model.history
                        , future = after
                    }

                _ ->
                    -- TODO: play sound
                    model

        Game.SaveGameState ->
            { model
                | history = List.take 150 <| game :: model.history
                , future = []
                , game = Just game
            }


view : Model -> Browser.Document Msg
view model =
    { title = "tetris.elm"
    , body =
        [ E.layout
            [ Background.color Styles.backgroundColor
            , Font.color Styles.fontColor
            , Font.family
                [ Font.monospace
                ]
            , Font.bold
            , E.inFront (E.map ChangedSettings <| Settings.view model.settings)
            ]
          <|
            case model.game of
                Nothing ->
                    viewMainMenu model

                Just game ->
                    Game.view game
        ]
    }


viewMainMenu : Model -> Element Msg
viewMainMenu model =
    E.column [ E.centerX, E.centerY ]
        [ E.el [ E.centerX ] <| E.text "FLETRIS"
        , Input.button [ E.centerX ] { onPress = Just PlaygroundPressed, label = E.text "PLAYGROUND" }
        , Input.button [ E.centerX ] { onPress = Nothing, label = E.text "MARATHON" }
        , Input.button [ E.centerX ] { onPress = Nothing, label = E.text "LOCAL MULTIPLAYER" }
        , Input.button [ E.centerX ] { onPress = Nothing, label = E.text "SETTINGS" }
        ]
