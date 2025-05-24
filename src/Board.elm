module Board exposing
    ( Board
    , Cell(..)
    , clearLines
    , collide
    , empty
    , get
    , ghost
    , height
    , place
    , spawn
    , width
    )

import Array exposing (Array)
import Element as E exposing (Color)
import Tetromino exposing (Letter(..), Rotation(..), Tetromino)


type Cell
    = Empty
    | Junk
    | Mino Letter


type Board
    = Board (Array Cell)


type Obstructed
    = Obstructed


spawn : Letter -> Board -> Result Obstructed Tetromino
spawn letter board =
    { letter = letter
    , rotation = R0
    , x = width // 2 - 1
    , y = height - 2
    }
        |> collide board


drop : Board -> Board
drop board =
    board


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


collide : Board -> Tetromino -> Result Obstructed Tetromino
collide board tetromino =
    let
        minos =
            Tetromino.minos tetromino

        oob () =
            List.any
                (\( mx, my ) ->
                    mx < 0 || my < 0 || mx >= width || my >= height
                )
                minos

        collision () =
            minos
                |> List.any (\( px, py ) -> get px py board /= Empty)
    in
    if collision () || oob () then
        Err Obstructed

    else
        Ok tetromino


clearLines : Board -> ( Board, Int )
clearLines (Board b0) =
    let
        completeLines =
            List.range 0 (height - 1)
                |> List.reverse
                |> List.filter
                    (\y ->
                        b0
                            |> Array.slice (y * width) ((y + 1) * width)
                            |> Array.toList
                            |> List.all ((/=) Empty)
                    )
    in
    ( completeLines
        |> List.foldl
            (\line board ->
                [ Array.slice 0 (line * width) board
                , Array.slice ((line + 1) * width) (Array.length board) board
                , Array.repeat width Empty
                ]
                    |> List.foldr Array.append Array.empty
            )
            b0
        |> Board
    , List.length completeLines
    )


ghost : Tetromino -> Board -> Tetromino
ghost tetromino board =
    List.range -1 tetromino.y
        |> List.reverse
        |> List.filterMap
            (\y ->
                let
                    shifted =
                        { tetromino | y = y }
                in
                case collide board shifted of
                    Err _ ->
                        Just { shifted | y = shifted.y + 1 }

                    Ok _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault tetromino


place : Tetromino -> Board -> Board
place tetromino (Board b0) =
    tetromino
        |> Tetromino.minos
        |> List.foldl (\( mx, my ) board -> board |> Array.set (my * width + mx) (Mino tetromino.letter)) b0
        |> Board
