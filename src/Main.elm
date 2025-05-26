module Main exposing (main)

import Animator as A exposing (Animator, Timeline)
import Board exposing (Board)
import Browser
import Browser.Events as Events
import Controller exposing (Button(..), Controller)
import Draw exposing (Drawing)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Random
import Random.List
import Styles
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
        , A.toSubscription Tick model animator
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
    , seed : Random.Seed
    , queue : List Letter
    , time : Time.Posix
    , das : Float
    , arr : Float
    , sdf : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = Time.millisToPosix 0
      , board = Board.empty
      , hold = Just S
      , tetromino =
            { letter = S
            , rotation = Tetromino.R0
            , x = A.init Board.spawnX
            , y = A.init Board.spawnY
            }
      , seed = Random.initialSeed 0
      , queue = []
      , controller = Controller.default
      , das = 150
      , sdf = 6
      , arr = 50
      }
    , generateNewBag
    )


animator : Animator Model
animator =
    A.animator
        |> A.watching
            (.y << .tetromino)
            (\newY model ->
                { model
                    | tetromino =
                        let
                            tetromino =
                                model.tetromino
                        in
                        { tetromino | y = newY }
                }
            )
        |> A.watching
            (.x << .tetromino)
            (\newX model ->
                { model
                    | tetromino =
                        let
                            tetromino =
                                model.tetromino
                        in
                        { tetromino | x = newX }
                }
            )


onRelease : Controller.Button -> Model -> Model
onRelease button modelBeforeRelease =
    let
        model =
            { modelBeforeRelease
                | controller = Controller.release button modelBeforeRelease.controller
            }
    in
    case button of
        Left ->
            { model
                | tetromino =
                    if Controller.isPressed Right model.controller then
                        moveWithDas 1 model.das model.arr model.tetromino

                    else
                        Tetromino.stopX model.tetromino
            }

        Right ->
            { model
                | tetromino =
                    if Controller.isPressed Left model.controller then
                        moveWithDas -1 model.das model.arr model.tetromino

                    else
                        Tetromino.stopX model.tetromino
            }

        SD ->
            { model | tetromino = Tetromino.stopY model.tetromino }

        _ ->
            model


moveWithDas : Int -> Float -> Float -> Tetromino -> Tetromino
moveWithDas dir das arr tetromino =
    let
        x =
            A.current tetromino.x

        autoRepeat =
            List.range 2 Board.width
                |> List.map (\off -> x + off * dir)
                |> List.map
                    (\xn ->
                        A.event A.immediately xn
                    )
                |> List.intersperse (A.wait (A.millis arr))
    in
    { tetromino
        | x =
            tetromino.x
                |> A.interrupt
                    ([ A.event A.immediately (x + dir)
                     , A.wait (A.millis das)
                     ]
                        ++ autoRepeat
                    )
    }


hasCollision : Board -> Tetromino -> Bool
hasCollision board tetromino =
    let
        x =
            A.current tetromino.x

        y =
            A.current tetromino.y

        minos =
            Tetromino.minos tetromino
    in
    List.any (\( mx, my ) -> Board.get mx my board /= Board.Empty) minos
        || List.any (\( mx, my ) -> (mx < 0 || Board.width <= mx) || (my < -2 || Board.height <= my)) minos


baseFallTimeMs : Float
baseFallTimeMs =
    500


softDrop : Float -> Tetromino -> Tetromino
softDrop sdf tetromino =
    let
        y =
            A.current tetromino.y
    in
    { tetromino
        | y =
            tetromino.y
                |> A.interrupt
                    (List.range 1 (Board.height - y)
                        |> List.map (\yoff -> A.event A.immediately (y + yoff))
                        |> List.intersperse (A.wait (A.millis (baseFallTimeMs / sdf)))
                    )
    }


ghostY : Board -> Tetromino -> Int
ghostY board tetromino =
    let
        cy =
            A.current tetromino.y
    in
    List.range 0 Board.height
        |> Util.takeWhile
            (\yoff ->
                { tetromino
                    | y = A.go A.immediately (cy + yoff) tetromino.y
                }
                    |> hasCollision board
                    |> not
            )
        |> List.reverse
        |> List.head
        |> Maybe.withDefault cy


hardDrop : Board -> Tetromino -> Tetromino
hardDrop board tetromino =
    let
        cy =
            A.current tetromino.y
    in
    { tetromino | y = A.go A.immediately (ghostY board tetromino) tetromino.y }


onPress : Controller.Button -> Model -> ( Model, Cmd Msg )
onPress button modelBeforePress =
    let
        model =
            { modelBeforePress | controller = Controller.press button modelBeforePress.controller }
    in
    case button of
        SD ->
            ( { model | tetromino = softDrop model.sdf model.tetromino }, Cmd.none )

        HD ->
            ( { model | tetromino = hardDrop model.board model.tetromino }, Cmd.none )

        Hold ->
            case model.hold of
                Just hold ->
                    ( { model
                        | tetromino =
                            { letter = hold
                            , rotation = Tetromino.R0
                            , x = A.go A.immediately Board.spawnX model.tetromino.x
                            , y = A.go A.immediately Board.spawnY model.tetromino.y
                            }
                        , hold = Just model.tetromino.letter
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Left ->
            ( { model
                | tetromino =
                    if Controller.isPressed Right model.controller then
                        Tetromino.stopX model.tetromino

                    else
                        moveWithDas -1 model.das model.arr model.tetromino
              }
            , Cmd.none
            )

        Right ->
            ( { model
                | tetromino =
                    if Controller.isPressed Left model.controller then
                        Tetromino.stopX model.tetromino

                    else
                        moveWithDas 1 model.das model.arr model.tetromino
              }
            , Cmd.none
            )

        Cw ->
            ( { model
                | tetromino = rotateWithKickBack Tetromino.cw model.board model.tetromino
              }
            , Cmd.none
            )

        Ccw ->
            ( { model
                | tetromino = rotateWithKickBack Tetromino.ccw model.board model.tetromino
              }
            , Cmd.none
            )

        R180 ->
            ( { model
                | tetromino =
                    rotateWithKickBack Tetromino.ccw model.board <|
                        rotateWithKickBack Tetromino.ccw model.board <|
                            model.tetromino
              }
            , Cmd.none
            )


tick : Time.Posix -> Model -> Model
tick time model =
    let
        updated =
            A.update time animator model

        nextTetromino =
            updated.tetromino

        xAligned =
            { nextTetromino | x = model.tetromino.x }

        yAligned =
            { nextTetromino | y = model.tetromino.y }

        board =
            updated.board
    in
    case ( hasCollision board xAligned, hasCollision board yAligned, hasCollision board nextTetromino ) of
        ( False, _, False ) ->
            updated

        ( True, False, _ ) ->
            { updated
                | tetromino =
                    let
                        t =
                            updated.tetromino
                    in
                    { t | x = A.go A.immediately (A.previous t.x) t.x }
            }

        ( False, _, True ) ->
            { updated
                | tetromino =
                    let
                        t =
                            updated.tetromino
                    in
                    { t | y = A.go A.immediately (A.previous t.y) t.y }
            }

        ( True, True, _ ) ->
            { updated
                | tetromino =
                    let
                        t =
                            updated.tetromino
                    in
                    { t
                        | x = A.go A.immediately (A.previous t.x) t.x
                        , y = A.go A.immediately (A.previous t.y) t.y
                    }
            }


generateNewBag : Cmd Msg
generateNewBag =
    Random.generate NewBag (Random.List.shuffle [ I, L, J, S, Z, O, T ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        DasChanged to ->
            ( { model | das = to }, Cmd.none )

        SdfChanged to ->
            ( { model | sdf = to }, Cmd.none )

        ArrChanged to ->
            ( { model | arr = to }, Cmd.none )

        Tick time ->
            ( tick time model
            , Cmd.none
            )

        Pressed button ->
            if Controller.isPressed button model.controller then
                ( model, Cmd.none )

            else
                onPress button model

        Released button ->
            if Controller.isReleased button model.controller then
                ( model, Cmd.none )

            else
                ( onRelease button model, Cmd.none )

        NewBag bag ->
            ( { model | queue = List.append model.queue bag }, Cmd.none )


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

        movePiece ( kx, ky ) =
            { letter = tetromino.letter
            , x = A.go A.immediately (A.current tetromino.x - kx) tetromino.x
            , y = A.go A.immediately (A.current tetromino.y - ky) tetromino.y
            , rotation = rotate tetromino.rotation
            }
    in
    kickbacks
        |> Util.dropWhile
            (hasCollision board << movePiece)
        |> List.head
        |> Maybe.map movePiece
        |> Maybe.withDefault tetromino


view : Model -> Browser.Document Msg
view model =
    { title = "tetris.elm"
    , body =
        [ E.layout
            [ Background.color Styles.backgroundColor
            , Font.color Styles.fontColor
            ]
          <|
            viewBody model

        --(viewBody model)
        ]
    }


standaloneTetromino : List (E.Attribute msg) -> Letter -> Element msg
standaloneTetromino attrs letter =
    let
        offx =
            case letter of
                O ->
                    -1

                _ ->
                    0

        offy =
            case letter of
                I ->
                    0

                _ ->
                    1

        maxWidth =
            4

        maxHeight =
            2

        padding =
            0.5
    in
    Tetromino.draw { letter = letter, x = A.init 0, y = A.init 0, rotation = Tetromino.R0 }
        |> renderScaled
            -(offx + 1.5 + padding)
            -(offy + 0.5 + padding)
            (maxWidth + 2 * padding)
            ((if letter == I then
                1

              else
                2
             )
                + 2
                * padding
            )
            attrs


renderScaled : Float -> Float -> Float -> Float -> List (E.Attribute msg) -> Drawing msg -> Element msg
renderScaled x y w h attrs drawing =
    let
        unit =
            Styles.minoSize

        sx =
            x * unit

        sy =
            y * unit

        sw =
            w * unit

        sh =
            h * unit
    in
    drawing
        |> Draw.scale unit unit
        |> Draw.render sx sy sw sh attrs


viewBody : Model -> Element Msg
viewBody model =
    E.row [ E.centerX, E.centerY ]
        [ viewHold model
        , viewBoard model
        , viewNext model
        , E.none -- viewSettings model
        ]


viewHold : Model -> Element msg
viewHold model =
    E.column
        [ E.alignTop
        , Border.color Styles.highlightColor
        , Border.width Styles.borderWidthPx
        ]
        [ E.text "HOLD"
            |> E.el
                [ Background.color Styles.highlightColor
                , E.width E.fill
                ]
        , model.hold
            |> Maybe.map (standaloneTetromino [])
            |> Maybe.withDefault E.none
            |> E.el [ E.padding 10 ]
        ]


viewBoard : Model -> Element msg
viewBoard model =
    let
        tetromino =
            Tetromino.draw model.tetromino
                |> Draw.rotate (Tetromino.toRad model.tetromino.rotation)
                |> Draw.shift
                    (toFloat <| A.current model.tetromino.x)
                    (toFloat <| A.current model.tetromino.y)

        margin =
            0.1
    in
    List.range 0 9
        |> List.concatMap
            (\x ->
                List.range 0 21
                    |> List.map (\y -> ( x, y ))
            )
        |> List.filterMap
            (\( x, y ) ->
                case Board.get x y model.board of
                    Board.Empty ->
                        Nothing

                    Board.Junk ->
                        Nothing

                    Board.Mino letter ->
                        Tetromino.drawMino letter
                            |> Draw.shift (toFloat x) (toFloat y)
                            |> Draw.fill (Tetromino.color letter)
                            |> Just
            )
        |> Draw.flatten
        |> Draw.over tetromino
        |> renderScaled
            -(0.5 + margin)
            (0.5 - margin)
            (toFloat Board.width + 2 * margin)
            (toFloat Board.height + 2 * margin)
            [ E.alignTop, Background.color Styles.boardColor ]


viewNext : Model -> Element msg
viewNext model =
    E.column
        [ E.alignTop
        , Border.color Styles.highlightColor
        , Border.width Styles.borderWidthPx
        ]
        [ E.text "NEXT"
            |> E.el
                [ Background.color Styles.highlightColor
                , E.width E.fill
                ]
        , E.column
            [ E.padding 10
            , E.spacing 10
            , E.height <| E.px <| round <| 5 * 2 * 1.8 * Styles.minoSize
            ]
            (model.queue
                |> List.take 5
                |> List.map (standaloneTetromino [])
            )
        ]


viewSettings : Model -> Element Msg
viewSettings model =
    E.column []
        [ E.text "Settings"
        , Input.slider []
            { onChange = DasChanged
            , label = Input.labelLeft [] (E.text "DAS")
            , min = 1
            , max = 300
            , value = model.das
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        , Input.slider []
            { onChange = ArrChanged
            , label = Input.labelLeft [] (E.text "ARR")
            , min = 1
            , max = 1000
            , value = model.arr
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        , Input.slider []
            { onChange = SdfChanged
            , label = Input.labelLeft [] (E.text "SDF")
            , min = 2
            , max = 50
            , value = model.sdf
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        ]
