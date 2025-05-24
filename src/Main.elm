module Main exposing (main)

import Board exposing (Board)
import Browser
import Browser.Events as Events
import Controller exposing (Button(..), Controller)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Random
import Random.List
import Styles
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Tetromino exposing (Letter(..), Rotation, Tetromino)
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
        [ Events.onKeyPress <|
            Decode.map (Maybe.withDefault Noop << Maybe.map Pressed) Controller.buttonDecoder
        , Events.onKeyUp <|
            Decode.map (Maybe.withDefault Noop << Maybe.map Released) Controller.buttonDecoder

        -- , Time.every 500 Tick
        ]


type Msg
    = Pressed Controller.Button
    | Released Controller.Button
    | NewBag (List Letter)
    | Noop
    | Tick Time.Posix
    | DasChanged Float
    | SdfChanged Float
    | ArrChanged Float


type alias Model =
    { board : Board
    , controller : Controller
    , hold : Maybe Letter
    , tetromino : Tetromino
    , dropTimer : Int
    , seed : Random.Seed
    , queue : List Letter
    , time : Time.Posix
    , das : Int
    , arr : Int
    , sdf : Int
    }


generateNewBag : Cmd Msg
generateNewBag =
    Random.generate NewBag (Random.List.shuffle [ I, L, J, S, Z, O, T ])


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = Time.millisToPosix 0
      , board = Board.empty
      , hold = Just S
      , tetromino = { letter = I, rotation = Tetromino.R0, x = Board.width // 2, y = Board.height - 2 }
      , dropTimer = defaultDropTime
      , seed = Random.initialSeed 0
      , queue = []
      , controller = Controller.default
      , das = 100
      , sdf = 20
      , arr = 2
      }
    , generateNewBag
    )


spawnNext : Model -> ( Model, Cmd Msg )
spawnNext model =
    case model.queue of
        [] ->
            ( model, Cmd.none )

        first :: rest ->
            case Board.spawn first model.board of
                Err _ ->
                    ( { model | queue = rest }, Cmd.none )

                Ok tetromino ->
                    ( { model | queue = rest, tetromino = tetromino }
                    , if List.length rest < 7 then
                        generateNewBag

                      else
                        Cmd.none
                    )


drop : Tetromino -> Board -> Board
drop tetromino board =
    board
        |> Board.place tetromino
        |> Board.clearLines
        |> Tuple.first


onRelease : Controller.Button -> Model -> Model
onRelease button model =
    { model
        | controller = Controller.release button model.controller
    }


onPress : Controller.Button -> Model -> ( Model, Cmd Msg )
onPress button modelBeforePress =
    let
        model =
            { modelBeforePress | controller = Controller.press button modelBeforePress.controller }
    in
    case button of
        SD ->
            ( model, Cmd.none )

        HD ->
            spawnNext
                { model
                    | board = drop (Board.ghost model.tetromino model.board) model.board
                }

        Hold ->
            case model.hold of
                Just hold ->
                    ( { model
                        | tetromino = Board.spawn hold model.board |> Result.withDefault model.tetromino
                        , hold = Just model.tetromino.letter
                      }
                    , Cmd.none
                    )

                Nothing ->
                    spawnNext { model | hold = Just model.tetromino.letter }

        Left ->
            ( { model | tetromino = moveTetromino Controller.DLeft model.board model.tetromino }
            , Cmd.none
            )

        Right ->
            ( { model | tetromino = moveTetromino Controller.DRight model.board model.tetromino }
            , Cmd.none
            )

        Cw ->
            ( { model | tetromino = rotateWithKickBack Tetromino.cw model.board model.tetromino }
            , Cmd.none
            )

        Ccw ->
            ( { model | tetromino = rotateWithKickBack Tetromino.ccw model.board model.tetromino }
            , Cmd.none
            )

        R180 ->
            ( { model | tetromino = rotateWithKickBack (Tetromino.ccw << Tetromino.ccw) model.board model.tetromino }
            , Cmd.none
            )


moveTetromino : Controller.Direction -> Board -> Tetromino -> Tetromino
moveTetromino direction board tetromino =
    let
        move =
            case direction of
                Controller.DLeft ->
                    Tetromino.left

                Controller.DRight ->
                    Tetromino.right

                Controller.Neutral ->
                    identity
    in
    tetromino
        |> move
        |> Board.collide board
        |> Result.withDefault tetromino


defaultDropTime : Int
defaultDropTime =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        DasChanged to ->
            ( { model | das = round to }, Cmd.none )

        SdfChanged to ->
            ( { model | sdf = round to }, Cmd.none )

        ArrChanged to ->
            ( { model | arr = round to }, Cmd.none )

        Tick time ->
            let
                dt =
                    Time.posixToMillis time - Time.posixToMillis model.time
            in
            { model
                | time = time
                , dropTimer = model.dropTimer - dt
            }
                |> dropOnTimeOut
                |> (\m -> ( m, Cmd.none ))

        Pressed button ->
            if Controller.isPressed button model.controller then
                ( model, Cmd.none )

            else
                onPress button model

        Released button ->
            ( onRelease button model, Cmd.none )

        NewBag bag ->
            ( { model | queue = List.append model.queue bag }, Cmd.none )


dropOnTimeOut : Model -> Model
dropOnTimeOut model =
    let
        ghost =
            Board.ghost model.tetromino model.board
    in
    if model.dropTimer > 0 then
        model

    else if ghost.y == model.tetromino.y then
        { model
            | board = drop model.tetromino model.board
        }

    else
        { model
            | tetromino =
                model.tetromino
                    |> Tetromino.down
                    |> Board.collide model.board
                    |> Result.withDefault model.tetromino
        }


rotateWithKickBack : (Rotation -> Rotation) -> Board -> Tetromino -> Tetromino
rotateWithKickBack rotate board tetromino =
    let
        offset =
            Tetromino.offsetTable tetromino.letter

        fromOffsets =
            [ 1, 2, 3, 4, 5 ] |> List.map (offset tetromino.rotation)

        toOffsets =
            [ 1, 2, 3, 4, 5 ] |> List.map (offset <| rotate tetromino.rotation)

        kickbacks =
            List.map2 (\( x2, y2 ) ( x1, y1 ) -> ( x2 - x1, y2 - y1 )) toOffsets fromOffsets
    in
    kickbacks
        |> List.filterMap
            (\( kx, ky ) ->
                let
                    kickedTetromino =
                        { tetromino | x = tetromino.x - kx, y = tetromino.y - ky, rotation = rotate tetromino.rotation }
                in
                Board.collide board kickedTetromino
                    |> Result.toMaybe
            )
        |> List.head
        |> Maybe.withDefault tetromino


minoSize : Int
minoSize =
    25


view : Model -> Browser.Document Msg
view model =
    { title = "tetris.elm"
    , body =
        [ E.layout
            [ Background.color Styles.backgroundColor
            , Font.color Styles.fontColor
            ]
            (viewBody model)
        ]
    }


render : Int -> Int -> List (Svg msg) -> Element msg
render width height =
    let
        w =
            String.fromInt width

        h =
            String.fromInt height
    in
    E.html
        << Svg.svg
            [ SvgA.width w
            , SvgA.height h
            , SvgA.viewBox ("0 0 " ++ w ++ " " ++ h)
            ]


toHexString : E.Color -> String
toHexString color =
    let
        truncate str =
            String.dropLeft (max 0 (String.length str - 2)) str

        intToHex dec =
            case dec of
                15 ->
                    "F"

                14 ->
                    "E"

                13 ->
                    "D"

                12 ->
                    "C"

                11 ->
                    "B"

                10 ->
                    "A"

                _ ->
                    if dec >= 16 then
                        truncate (intToHex (dec // 16) ++ intToHex (dec |> modBy 16))

                    else
                        String.fromInt dec

        { red, green, blue, alpha } =
            E.toRgb color

        ired =
            floor <| 255 * red

        igreen =
            floor <| 255 * green

        iblue =
            floor <| 255 * blue

        ialpha =
            floor <| 255 * alpha
    in
    Debug.log "final" <| "#" ++ intToHex ired ++ intToHex igreen ++ intToHex iblue


mino : Int -> Int -> Letter -> Svg msg
mino x y letter =
    Svg.rect
        [ SvgA.x (String.fromInt (minoSize * x))
        , SvgA.y (String.fromInt (minoSize * y))
        , SvgA.width (String.fromInt minoSize)
        , SvgA.height (String.fromInt minoSize)
        ]
        []


viewBody : Model -> Element Msg
viewBody model =
    E.row [ E.centerX, E.centerY ]
        [ -- Hold
          E.column
            [ E.alignTop
            , Border.color Styles.highlightColor
            , Border.width Styles.borderWidthPx
            , E.width Styles.sidebarWidth
            , E.height <| E.px 80
            ]
            [ E.el
                [ Background.color Styles.highlightColor
                , E.width E.fill
                ]
              <|
                E.text "HOLD"
            , case model.hold of
                Nothing ->
                    E.none

                Just letter ->
                    let
                        unit =
                            String.fromInt minoSize

                        tetromino =
                            Svg.g
                                [ SvgA.fill (toHexString (Tetromino.color letter)) ]
                                [ mino -1 0 letter
                                , mino 0 0 letter
                                , mino 0 -1 letter
                                , mino 1 -1 letter
                                ]
                    in
                    render (minoSize * 5) (minoSize * 3) [ tetromino ]
            ]
        , -- Board
          List.range 0 (Board.width * Board.height - 1)
            |> List.map
                (\i ->
                    let
                        x =
                            i |> remainderBy Board.width

                        y =
                            i // Board.width

                        cell =
                            if Tetromino.overlaps x y model.tetromino then
                                Board.Mino model.tetromino.letter

                            else
                                Board.get x y model.board

                        ghost =
                            Board.ghost model.tetromino model.board
                    in
                    E.el
                        [ E.width (E.px minoSize)
                        , E.height (E.px minoSize)
                        , Background.color <|
                            case cell of
                                Board.Empty ->
                                    Styles.boardColor

                                Board.Junk ->
                                    Styles.junkColor

                                Board.Mino t ->
                                    Tetromino.color t
                        , Border.width
                            (if cell == Board.Empty && Tetromino.overlaps x y ghost then
                                2

                             else
                                0
                            )
                        ]
                        E.none
                )
            |> Util.chunksOf Board.width
            |> List.reverse
            |> List.map (E.row [])
            |> E.column []
        , -- Next
          E.column
            [ E.width Styles.sidebarWidth
            , E.alignTop
            , Border.color Styles.highlightColor
            , Border.width Styles.borderWidthPx
            ]
            [ E.el
                [ Background.color Styles.highlightColor
                , E.width E.fill
                ]
              <|
                E.text "NEXT"
            , E.column []
                (model.queue
                    |> List.take 5
                    |> List.map (E.text << Tetromino.toString)
                )
            ]
        , (\_ -> E.none) <|
            E.column []
                [ E.text "Settings"
                , Input.slider []
                    { onChange = DasChanged
                    , label = Input.labelLeft [] (E.text "DAS")
                    , min = 1
                    , max = 300
                    , value = toFloat model.das
                    , thumb = Input.defaultThumb
                    , step = Just 1
                    }
                , Input.slider []
                    { onChange = ArrChanged
                    , label = Input.labelLeft [] (E.text "ARR")
                    , min = 1
                    , max = 1000
                    , value = toFloat model.arr
                    , thumb = Input.defaultThumb
                    , step = Just 1
                    }
                , Input.slider []
                    { onChange = SdfChanged
                    , label = Input.labelLeft [] (E.text "SDF")
                    , min = 2
                    , max = 50
                    , value = toFloat model.sdf
                    , thumb = Input.defaultThumb
                    , step = Just 1
                    }
                ]
        ]
