module Game exposing
    ( Cmd(..)
    , Game
    , init
    , onPress
    , onRelease
    , screenHeight
    , screenWidth
    , tick
    , view
    )

import Animate as A
import Board exposing (Board, width)
import Controller exposing (Button(..), Controller)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import List.Extra as List
import Random
import Random.List
import Render
import Resources exposing (Resources)
import Settings exposing (Settings)
import Tetromino exposing (Tetromino)
import Tetromino.Letter as Letter exposing (Letter(..))
import Tetromino.Orientation as Orientation exposing (Orientation(..))
import Texture exposing (Texture)
import Time
import Util


type Cmd
    = Undo
    | Redo
    | SaveGameState
    | NoCmd


type Msg
    = TextureLoaded Texture
    | NoMsg


type LineClear
    = Single
    | Double
    | Triple
    | Tetris


type Spin
    = Mini
    | Full


type alias Game =
    { seed : Random.Seed
    , board : Board
    , hold : Maybe Letter
    , tetromino : Tetromino
    , queue : List Letter
    , popupMessage : String
    , lineClear : Maybe LineClear
    , spin : Maybe ( Spin, Letter )
    , b2b : Int
    , combo : Int
    , score : Int
    , finesseFaults : Int
    , movements : Int
    , turn : Int
    , res : Resources
    }


bagGenerator : Random.Generator (List Letter)
bagGenerator =
    Random.List.shuffle [ I, T, O, J, L, S, Z ]


init : Random.Seed -> Resources -> Game
init s0 res =
    let
        ( bag, s1 ) =
            Random.step bagGenerator s0

        ( letter, queue ) =
            case bag of
                [] ->
                    ( I, [] )

                first :: rest ->
                    ( first, rest )
    in
    { board = Board.empty
    , hold = Nothing
    , tetromino = Board.spawn letter
    , seed = s1
    , queue = queue
    , lineClear = Nothing
    , spin = Nothing
    , score = 0
    , b2b = 0
    , combo = 0
    , movements = 0
    , finesseFaults = 0
    , popupMessage = ""
    , turn = 1
    , res = res
    }


screenWidth =
    767


screenHeight =
    395


view : Game -> List Render.Command
view game =
    let
        gridSize =
            16

        boardOrigin =
            { x = 133, y = 0 }

        board =
            List.range 0 (Board.width * Board.height - 1)
                |> List.concatMap
                    (\i ->
                        let
                            ( x, y ) =
                                ( i |> modBy Board.width, i // Board.width )
                        in
                        case Board.get x y game.board of
                            Board.Mino letter ->
                                [ Render.setPalette game.res.minoPalette (Letter.toInt letter)
                                , Render.drawTexture (toFloat <| gridSize * x + boardOrigin.x) (toFloat <| gridSize * y + boardOrigin.y) game.res.mino
                                ]

                            Board.Empty ->
                                []

                            Board.Junk ->
                                []
                    )

        drawTetromino : Tetromino -> Float -> Float -> List Render.Command
        drawTetromino tetromino offx offy =
            List.concat
                [ [ Render.setPalette game.res.minoPalette (Letter.toInt tetromino.letter) ]
                , tetromino
                    |> Tetromino.minos
                    |> List.map
                        (\( x, y ) ->
                            Render.drawTexture
                                (toFloat (gridSize * x) + offx)
                                (toFloat (gridSize * y) + offy)
                                game.res.mino
                        )
                , [ Render.resetPalette ]
                ]

        drawCenteredTetromino tetromino offx offy =
            let
                ioOffset =
                    if tetromino.letter == Letter.I || tetromino.letter == Letter.O then
                        -gridSize // 2

                    else
                        0

                iOffset =
                    if tetromino.letter == Letter.I then
                        -gridSize // 2

                    else
                        0
            in
            drawTetromino tetromino (offx + toFloat ioOffset) (offy + toFloat iOffset)

        interface =
            List.concat
                [ case game.hold of
                    Nothing ->
                        []

                    Just hold ->
                        drawCenteredTetromino { letter = hold, orientation = North, x = A.init { position = 0, motion = Nothing }, y = A.init 0 }
                            (boardOrigin.x - 55)
                            (boardOrigin.y + 63)
                , game.queue
                    |> List.take Settings.queueLength
                    |> List.indexedMap
                        (\i letter ->
                            drawCenteredTetromino { letter = letter, orientation = North, x = A.init { position = 0, motion = Nothing }, y = A.init 0 }
                                (toFloat (boardOrigin.x + Board.width * gridSize + 35))
                                (toFloat (boardOrigin.y + 63 + i * round (2.8 * gridSize)))
                        )
                    |> List.concat
                ]

        settings =
            [--Render.text "hello"
            ]
    in
    List.concat
        [ -- Background
          [ Render.drawTexture 0 0 game.res.decor
          , Render.drawTexture 0 0 game.res.text
          , Render.drawTexture -1 0 game.res.grid
          ]

        -- Board
        , board

        -- Ghost
        , List.concat
            [ [ Render.setPalette game.res.ghostPalette (Letter.toInt game.tetromino.letter) ]
            , game.tetromino
                |> Tetromino.minos
                |> List.map
                    (\( x, y ) ->
                        Render.drawTexture
                            (toFloat (gridSize * x) + boardOrigin.x)
                            (toFloat (gridSize * (y + ghostY game.board game.tetromino - Tetromino.getY game.tetromino)) + boardOrigin.y)
                            game.res.ghost
                    )
            , [ Render.resetPalette ]
            ]

        -- Tetromino
        , drawTetromino game.tetromino boardOrigin.x boardOrigin.y

        -- Foreground
        , [ Render.drawTexture 0 0 game.res.vines ]
        , interface
        ]



-- standaloneTetromino : Maybe Letter -> Texture
-- standaloneTetromino letter =
--     E.none
-- viewLineClear : LineClear -> Element msg
-- viewLineClear lineClear =
--     E.text <|
--         case lineClear of
--             Single ->
--                 "SINGLE!"
--             Double ->
--                 "DOUBLE!!"
--             Triple ->
--                 "TRIPLE!!!"
--             Tetris ->
--                 "HHHHNGHGNHN"
-- viewCombo : Int -> Element msg
-- viewCombo combo =
--     E.text <|
--         if combo >= 2 then
--             "x" ++ String.fromInt combo ++ " COMBO!"
--         else
--             ""
-- viewSpin : Maybe ( Spin, Letter ) -> Element msg
-- viewSpin field =
--     case field of
--         Just ( Full, letter ) ->
--             E.text <| "full on " ++ Letter.toString letter ++ " spin!"
--         Just ( Mini, letter ) ->
--             E.text <| "mini " ++ Letter.toString letter ++ " spin!"
--         Nothing ->
--             E.text ""
-- viewStats : Game -> Element msg
-- viewStats game =
--     let
--         viewStat ( name, value ) =
--             E.text <| name ++ "\n" ++ value
--     in
--     [ ( "SCORE", String.fromInt game.score )
--     , ( "FINESSE"
--       , let
--             num =
--                 game.turn - 1 - game.finesseFaults
--             den =
--                 game.turn - 1
--             ratio =
--                 "(" ++ String.fromInt num ++ "/" ++ String.fromInt den ++ ")"
--             format n =
--                 let
--                     str =
--                         n |> String.fromFloat
--                     maxLength =
--                         6
--                 in
--                 if n == 100 then
--                     "100." ++ String.repeat (maxLength - 3) "0" ++ "%"
--                 else if str |> String.contains "." then
--                     str
--                         |> String.left maxLength
--                         |> String.padRight maxLength '0'
--                         |> Util.flip String.append "%"
--                 else
--                     str
--                         |> String.left maxLength
--                         |> Util.flip String.append "."
--                         |> String.padRight maxLength '0'
--                         |> Util.flip String.append "%"
--             percent =
--                 format (100 * toFloat num / toFloat den)
--         in
--         if den == 0 then
--             ratio ++ " or " ++ format 100
--         else
--             ratio ++ " or " ++ percent
--       )
--     , ( "MOVEMENTS", String.fromInt game.movements )
--     ]
--         |> List.map viewStat
--         |> E.column [ E.alignTop ]
-- viewNext : Game -> Element msg
-- viewNext game =
--     E.column
--         [ E.alignTop
--         , Border.color Styles.highlightColor
--         , Border.width Styles.borderWidthPx
--         , Border.roundEach
--             { topLeft = 0
--             , topRight = Styles.borderRadiusPx
--             , bottomLeft = 0
--             , bottomRight = Styles.borderRadiusPx
--             }
--         ]
--         [ E.text "NEXT"
--             |> E.el
--                 [ Background.color Styles.highlightColor
--                 , E.width E.fill
--                 , Styles.defaultPadding
--                 ]
--         , E.column
--             [ Styles.defaultPadding
--             , Styles.defaultSpacing
--             , E.height <| E.px <| round <| 5 * 2 * 1.8 * Styles.minoSize
--             ]
--             (game.queue
--                 |> List.take Settings.queueLength
--                 |> List.map (standaloneTetromino [] << Just)
--             )
--         ]
-- viewHold : Game -> Surface msg
-- viewHold game =
--     E.column
--         [ E.alignTop
--         , Border.color Styles.highlightColor
--         , Border.width Styles.borderWidthPx
--         , Border.roundEach
--             { topLeft = Styles.borderRadiusPx
--             , topRight = 0
--             , bottomLeft = Styles.borderRadiusPx
--             , bottomRight = 0
--             }
--         , E.width (E.fill |> E.minimum Styles.sidebarWidthPx)
--         ]
--         [ E.text "HOLD"
--             |> E.el
--                 [ Background.color Styles.highlightColor
--                 , Styles.defaultPadding
--                 , E.width E.fill
--                 ]
--         , game.hold
--             |> standaloneTetromino []
--         ]
--
-- viewSettings : Settings -> Surface Settings.Msg
-- viewSettings settings =
-- viewControls : Settings -> Element Msg
-- viewControls settings =
--     let
--         width =
--             Controller.allButtons
--                 |> List.map
--                     (\b ->
--                         [ name b
--                         , arrowStr
--                         , keys b
--                         ]
--                             |> List.map String.length
--                             |> List.sum
--                     )
--                 |> List.maximum
--                 |> Maybe.map ((+) 2)
--                 |> Maybe.map ((+) (String.length arrowStr))
--                 |> Maybe.withDefault 8
--         name button =
--             case button of
--                 Cw ->
--                     "ROTATE CLOCKWISE"
--                 R180 ->
--                     "ROTATE 180 DEGREES"
--                 Ccw ->
--                     "ROTATE COUNTER-CLOCKWISE"
--                 Left ->
--                     "MOVE LEFT"
--                 Right ->
--                     "MOVE RIGHT"
--                 HD ->
--                     "HARD DROP"
--                 SD ->
--                     "SOFT DROP"
--                 Hold ->
--                     "HOLD"
--                 Undo ->
--                     "UNDO"
--                 Redo ->
--                     "REDO"
--         format str =
--             if str == " " then
--                 "[SPACE]"
--             else if String.length str > 1 then
--                 "[" ++ str ++ "]"
--             else
--                 String.toUpper str
--         keys button =
--             settings.controls
--                 |> Dict.toList
--                 |> List.filter (Tuple.second >> (==) button)
--                 |> List.map Tuple.first
--                 |> List.map format
--                 |> String.join ","
--         arrowStr =
--             "-> "
--         arrow button =
--             if settings.setInputMode == Setting button then
--                 arrowStr
--             else
--                 ""
--         mappingAsString button =
--             let
--                 n =
--                     name button
--                 a =
--                     arrow button
--                 k =
--                     keys button
--                 dots =
--                     String.repeat (width - String.length n - String.length a - String.length k) "."
--             in
--             n ++ dots ++ a ++ k
--     in
-- viewHandling : Settings.Handling -> Element HandlingMsg
-- viewHandling handling =


beginMovement : Settings.Handling -> Controller -> Tetromino -> Tetromino
beginMovement handling controller tetromino =
    let
        direction =
            Controller.direction controller

        moving =
            tetromino.x |> A.current |> .motion |> (/=) Nothing
    in
    if moving then
        tetromino

    else if direction > 0 then
        Tetromino.move handling Tetromino.Das Tetromino.Right tetromino

    else if direction < 0 then
        Tetromino.move handling Tetromino.Das Tetromino.Left tetromino

    else
        tetromino


tick : Time.Posix -> Settings -> Controller -> Game -> ( Game, Cmd )
tick time settings controller game =
    let
        tetromino =
            game.tetromino
                |> moveTetromino time
    in
    ( { game
        | tetromino =
            tetromino
                |> collide game.board game.tetromino
      }
    , NoCmd
    )


moveTetromino : Time.Posix -> Tetromino -> Tetromino
moveTetromino time tetromino =
    { tetromino
        | x = A.update time tetromino.x
        , y = A.update time tetromino.y
    }



-- TODO: filter redundant collision checks


collide : Board -> Tetromino -> Tetromino -> Tetromino
collide board prev next =
    let
        ( prevx, prevy ) =
            ( Tetromino.getX prev
            , Tetromino.getY prev
            )

        ( nextx, nexty ) =
            ( Tetromino.getX next
            , Tetromino.getY next
            )

        collided =
            prev
                |> resolve board prevx nextx Tetromino.setX
                |> resolve board prevy nexty Tetromino.setY
    in
    if Tetromino.getX collided == nextx && Tetromino.getY collided == nexty then
        next

    else if Tetromino.getX collided == nextx then
        { collided | x = next.x }

    else if Tetromino.getY collided == nexty then
        { collided | y = next.y }

    else
        collided


isColliding : Board -> Tetromino -> Bool
isColliding board tetromino =
    let
        minos =
            Tetromino.minos tetromino

        collision () =
            List.any (\( mx, my ) -> Board.get mx my board /= Board.Empty) minos

        oob () =
            List.any (\( mx, my ) -> (mx < 0 || Board.width <= mx) || (my < -2 || Board.height <= my)) minos
    in
    collision () || oob ()


baseFallTimeMs : Float
baseFallTimeMs =
    500


softDrop : Settings.Sdf -> Tetromino -> Tetromino
softDrop sdf tetromino =
    let
        y =
            A.current tetromino.y

        events =
            case sdf of
                Settings.SdfInfinite ->
                    [ A.go A.immediately 30 ]

                Settings.SdfGradual f ->
                    List.range 1 (Board.height - y)
                        |> List.map (\yoff -> A.go A.immediately (y + yoff))
                        |> List.intersperse (A.wait (A.millis <| baseFallTimeMs / f))
    in
    { tetromino
        | y = A.interrupt events tetromino.y
    }


ghostY : Board -> Tetromino -> Int
ghostY board tetromino =
    let
        cy =
            A.current tetromino.y
    in
    List.range 0 Board.height
        |> List.takeWhile
            (\yoff ->
                not <|
                    isColliding board <|
                        { tetromino | y = A.init (cy + yoff) }
            )
        |> List.reverse
        |> List.head
        |> Maybe.map ((+) cy)
        |> Maybe.withDefault cy


hardDrop : Board -> Tetromino -> Tetromino
hardDrop board tetromino =
    { tetromino | y = A.init (ghostY board tetromino) }


nextTetromino : Game -> Game
nextTetromino game =
    { game
        | tetromino =
            game.queue
                |> List.head
                |> Maybe.withDefault I
                |> Board.spawn
        , queue =
            game.queue
                |> List.tail
                |> Maybe.withDefault []
    }


topOut : Game -> Game
topOut game =
    if game.tetromino |> Tetromino.minos |> List.any (\( mx, my ) -> my < 0) then
        { game
            | board = Board.empty
            , hold = Nothing
            , popupMessage = ""
        }

    else
        game


clearLines : Game -> Game
clearLines game =
    let
        lines =
            List.range 0 (Board.height - 1)
                |> List.filter
                    (\y ->
                        List.range 0 (Board.width - 1)
                            |> List.all (\x -> Board.get x y game.board /= Board.Empty)
                    )

        lineClear =
            case List.length lines of
                1 ->
                    Just Single

                2 ->
                    Just Double

                3 ->
                    Just Triple

                4 ->
                    Just Tetris

                _ ->
                    Nothing

        combo =
            if lineClear == Nothing then
                0

            else
                1 + game.combo
    in
    { game
        | board = List.foldl Board.clearLine game.board lines
        , lineClear = lineClear
        , combo = combo
    }


restockQueue : Game -> Game
restockQueue game =
    let
        ( queue, seed ) =
            if List.length game.queue < Settings.queueLength then
                Random.step (bagGenerator |> Random.map (List.append game.queue)) game.seed

            else
                ( game.queue, game.seed )
    in
    { game
        | queue = queue
        , seed = seed
    }


resolveFinesse : Game -> Game
resolveFinesse game =
    { game
        | finesseFaults =
            if game.movements > 2 then
                game.finesseFaults + 1

            else
                game.finesseFaults
        , movements = 0
    }


onRelease : Controller.Button -> Controller -> Settings -> Game -> Game
onRelease button controller settings game =
    case button of
        Left ->
            { game
                | tetromino =
                    if game.tetromino.x |> A.current |> .motion |> Maybe.map (\( _, dir ) -> dir == Tetromino.Left) |> (==) (Just True) then
                        let
                            t =
                                game.tetromino
                        in
                        { t | x = A.stop t.x }

                    else
                        game.tetromino
            }

        Right ->
            { game
                | tetromino =
                    if game.tetromino.x |> A.current |> .motion |> Maybe.map (\( _, dir ) -> dir == Tetromino.Right) |> (==) (Just True) then
                        let
                            t =
                                game.tetromino
                        in
                        { t | x = A.stop t.x }

                    else
                        game.tetromino
            }

        SD ->
            { game
                | tetromino =
                    let
                        t =
                            game.tetromino
                    in
                    { t | y = A.stop t.y }
            }

        _ ->
            game


getSpin : (Orientation -> Orientation) -> Board -> Tetromino -> Maybe ( Spin, Letter )
getSpin rotate board tetromino =
    let
        x =
            Tetromino.getX tetromino

        y =
            Tetromino.getY tetromino

        mostCornersBlocked =
            [ ( x - 1, y - 1 )
            , ( x + 1, y - 1 )
            , ( x - 1, y + 1 )
            , ( x + 1, y + 1 )
            ]
                |> List.filter (\( xx, yy ) -> Board.get xx yy board /= Board.Empty)
                |> List.length
                |> (<=) 3
    in
    if tetromino.letter == T && mostCornersBlocked then
        Just ( Full, T )

    else
        Nothing


onPress : Controller.Button -> Settings -> Game -> ( Game, Cmd )
onPress button settings game =
    let
        pressRotate rotate =
            case getKickback rotate game.board game.tetromino of
                Nothing ->
                    game

                Just ( kx, ky ) ->
                    let
                        t =
                            game.tetromino
                    in
                    { game
                        | tetromino =
                            { t
                                | orientation = rotate t.orientation
                                , x = t.x |> A.map (\xx -> { xx | position = xx.position + kx })
                                , y = t.y |> A.map ((+) ky)
                            }
                        , spin = getSpin rotate game.board game.tetromino
                    }
    in
    case button of
        SD ->
            ( { game | tetromino = softDrop settings.handling.sdf game.tetromino }, NoCmd )

        HD ->
            ( { game
                | tetromino = hardDrop game.board game.tetromino
              }
                |> endTurn
            , SaveGameState
            )

        Hold ->
            ( case game.hold of
                Just hold ->
                    { game
                        | tetromino = Board.spawn hold
                        , hold = Just game.tetromino.letter
                    }

                Nothing ->
                    { game | hold = Just game.tetromino.letter } |> nextTetromino
            , NoCmd
            )

        Left ->
            ( { game
                | tetromino = Tetromino.move settings.handling Tetromino.Das Tetromino.Left game.tetromino
                , movements = game.movements + 1
              }
            , NoCmd
            )

        Right ->
            ( { game
                | tetromino = Tetromino.move settings.handling Tetromino.Das Tetromino.Right game.tetromino
                , movements = game.movements + 1
              }
            , NoCmd
            )

        Cw ->
            ( pressRotate Orientation.cw, NoCmd )

        Ccw ->
            ( pressRotate Orientation.ccw, NoCmd )

        R180 ->
            ( pressRotate (Orientation.ccw << Orientation.ccw), NoCmd )

        Controller.Undo ->
            ( game, Undo )

        Controller.Redo ->
            ( game, Redo )


endTurn : Game -> Game
endTurn game =
    { game
        | board = Board.place game.tetromino game.board
        , turn = game.turn + 1
    }
        |> resolveFinesse
        |> topOut
        |> nextTetromino
        |> clearLines
        |> restockQueue


resolve : Board -> Int -> Int -> (Int -> Tetromino -> Tetromino) -> Tetromino -> Tetromino
resolve board prev next move tetromino =
    let
        dist =
            abs (next - prev)

        dir =
            Util.sign (next - prev)

        moved =
            move next tetromino
    in
    if moved |> (not << isColliding board) then
        moved

    else
        List.range 0 dist
            |> List.map (\offset -> move (prev + offset * dir) tetromino)
            |> List.takeWhile (not << isColliding board)
            |> List.last
            |> Maybe.withDefault tetromino


getKickback : (Orientation -> Orientation) -> Board -> Tetromino -> Maybe ( Int, Int )
getKickback rotate board tetromino =
    let
        offset =
            Tetromino.offsetTable tetromino.letter

        fromOffsets =
            [ 1, 2, 3, 4, 5 ]
                |> List.map (offset tetromino.orientation)

        toOffsets =
            [ 1, 2, 3, 4, 5 ]
                |> List.map (offset <| rotate tetromino.orientation)

        kickbacks =
            List.map2 (\( x2, y2 ) ( x1, y1 ) -> ( x2 - x1, y2 - y1 )) fromOffsets toOffsets

        movePiece ( kx, ky ) =
            { tetromino
                | orientation = rotate tetromino.orientation
                , x =
                    tetromino.x
                        |> A.map (\x -> { x | position = x.position + kx })
                , y =
                    tetromino.y |> A.map ((+) ky)
            }
    in
    kickbacks
        |> List.dropWhile
            (isColliding board << movePiece)
        |> List.head
