port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation
import Controller exposing (Button(..), Controller)
import Dict exposing (Dict)
import Game exposing (Game)
import Html
import Html.Attributes as Attr
import Html.Events
import Html.Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Print as Print
import Random
import Render
import Resources exposing (Resources)
import Settings exposing (Settings)
import Task
import Tetromino exposing (Tetromino)
import Tetromino.Letter as Letter exposing (Letter(..))
import Tetromino.Orientation as Orientation exposing (Orientation(..))
import Time
import Url exposing (Url)
import Util


port logger : Encode.Value -> Cmd msg


port render : Encode.Value -> Cmd msg


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
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
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | Noop


type alias Model =
    { controller : Controller
    , settings : Settings
    , game : Game
    , history : List Game
    , future : List Game
    }


type alias Flags =
    { settings : Settings, resources : Resources, seed : Random.Seed }


flagDecoder : Decode.Decoder Flags
flagDecoder =
    Decode.map3 Flags
        (Decode.field "settings" Settings.decoder)
        (Decode.field "resources" Resources.decoder)
        (Decode.field "seed" Decode.int |> Decode.map Random.initialSeed)


init : Decode.Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd msg )
init flags _ _ =
    case Decode.decodeValue flagDecoder flags of
        Err error ->
            let
                _ =
                    Debug.log "error loading flags" ()

                _ =
                    error
                        |> Decode.errorToString
                        |> Debug.log "error"

                _ =
                    Print.prettyValue { columns = 80, indent = 0 } flags
                        |> Debug.log "flags"
            in
            Debug.todo "fuck you" ()

        Ok { settings, resources, seed } ->
            ( { controller = Controller.default
              , settings = settings
              , game = Game.init seed resources
              , history = []
              , future = []
              }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        UrlRequested _ ->
            ( model, Cmd.none )

        ChangedSettings settingsMsg ->
            let
                ( settings, cmd ) =
                    Settings.update settingsMsg model.settings
            in
            ( { model | settings = settings }, cmd )

        Tick time ->
            let
                newModel =
                    model.game
                        |> Game.tick time model.settings model.controller
                        |> (\( g, m ) -> respondToCmd g m model)

                commands =
                    Game.view newModel.game
            in
            ( newModel
            , render <| Encode.list Render.encodeCommand commands
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

                        ( g, m ) =
                            Game.onPress button model.settings model.game
                    in
                    respondToCmd g m { model | controller = controller }
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

                        ( g, m ) =
                            ( Game.onRelease button controller model.settings model.game, Game.NoCmd )
                    in
                    respondToCmd g m { model | controller = controller }
            in
            ( model.settings.controls
                |> Dict.get key
                |> Maybe.andThen errorIfReleased
                |> Maybe.map onRelease
                |> Maybe.withDefault model
            , Cmd.none
            )


respondToCmd : Game -> Game.Cmd -> Model -> Model
respondToCmd game cmd model =
    case cmd of
        Game.NoCmd ->
            { model | game = game }

        Game.Undo ->
            case model.history of
                current :: prev :: before ->
                    { model
                        | game = prev
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
                        | game = next
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
                , game = game
            }


view : Model -> Browser.Document Msg
view model =
    { title = "fletris"
    , body =
        let
            scale =
                3
        in
        [ Html.canvas [ Attr.width (scale * Game.screenWidth), Attr.height (scale * Game.screenHeight), Attr.id "game", Attr.style "display" "none" ] []
        , Html.canvas [ Attr.width (scale * Game.screenWidth), Attr.height (scale * Game.screenHeight), Attr.id "downscaled-game" ] []

        -- HACK: here to render palettes to so we can read their image data
        , Html.canvas [ Attr.width 16, Attr.height 16, Attr.id "palette", Attr.style "display" "none" ] []

        -- HACK: here to render minos so we can palette swap them before upscaling
        , Html.canvas [ Attr.width 16, Attr.height 16, Attr.id "palette-swapping-buffer", Attr.style "display" "none" ] []
        ]
    }
