module Board exposing
    ( Board
    , Cell(..)
    , empty
    , get
    , height
    , spawnX
    , spawnY
    , width
    )

import Array exposing (Array)
import Tetromino exposing (Letter)


type Cell
    = Empty
    | Junk
    | Mino Letter


type Board
    = Board (Array Cell)


empty : Board
empty =
    Board <| Array.repeat (width * height) Empty


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


spawnX : Int
spawnX =
    width // 2


spawnY : Int
spawnY =
    2
