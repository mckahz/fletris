module Board exposing
    ( Board
    , Cell(..)
    , clearLine
    , decode
    , empty
    , encode
    , get
    , height
    , place
    , spawn
    , spawnX
    , spawnY
    , width
    )

import Animate as A
import Array exposing (Array)
import Json.Decode as Decode
import Json.Encode as Encode
import Tetromino exposing (Tetromino)
import Tetromino.Letter as Letter exposing (Letter(..))
import Tetromino.Orientation as Orientation exposing (Orientation(..))


type Cell
    = Empty
    | Junk
    | Mino Letter



-- TODO: Make board a record like {ts = [(0, 0)], ls, is....} so we can batch palette swaps when rendering


type Board
    = Board (Array Cell)


encodeCell : Cell -> Encode.Value
encodeCell cell =
    case cell of
        Empty ->
            Encode.int 8

        Junk ->
            Encode.int 7

        Mino letter ->
            Letter.encode letter


decodeCell : Decode.Decoder Cell
decodeCell =
    Decode.oneOf
        [ Decode.int
            |> Decode.andThen
                (\i ->
                    case i of
                        8 ->
                            Decode.succeed Empty

                        7 ->
                            Decode.succeed Junk

                        _ ->
                            Decode.fail "not a letter"
                )
        , Letter.decode |> Decode.map Mino
        ]


encode : Board -> Encode.Value
encode (Board board) =
    Encode.array encodeCell board


decode : Decode.Decoder Board
decode =
    Decode.array decodeCell
        |> Decode.map Board


empty : Board
empty =
    Array.repeat (width * height) Empty
        |> Board


place : Tetromino -> Board -> Board
place tetromino b0 =
    tetromino
        |> Tetromino.minos
        |> List.foldl (\( mx, my ) board -> board |> set mx my (Mino tetromino.letter)) b0


clearLine : Int -> Board -> Board
clearLine y (Board board) =
    [ Array.repeat width Empty
    , Array.slice 0 (y * width) board
    , Array.slice ((y + 1) * width) (Array.length board) board
    ]
        |> List.foldr Array.append Array.empty
        |> Board


width : Int
width =
    10


height : Int
height =
    22


get : Int -> Int -> Board -> Cell
get x y (Board board) =
    board
        |> Array.get (y * width + x)
        |> Maybe.withDefault Empty


set : Int -> Int -> Cell -> Board -> Board
set x y c (Board board) =
    board
        |> Array.set (y * width + x) c
        |> Board


spawnX : Int
spawnX =
    width // 2 - 1


spawnY : Int
spawnY =
    1


spawn : Letter -> Tetromino
spawn letter =
    { letter = letter
    , orientation = North
    , x = A.init { position = spawnX, motion = Nothing }
    , y = A.init spawnY
    }
