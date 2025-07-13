module Util exposing (chunksOf, flip, hex, sign)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Color exposing (Color)


hex : Int -> Color
hex x =
    Color.fromRgba
        { red = toFloat (x |> shiftLeftBy 0 |> shiftRightBy 6) / 256
        , green = toFloat (x |> shiftLeftBy 0 |> shiftRightBy 6) / 256
        , blue = toFloat (x |> shiftLeftBy 0 |> shiftRightBy 6) / 256
        , alpha = toFloat (x |> shiftLeftBy 0 |> shiftRightBy 6) / 256
        }


flip : (b -> a -> c) -> a -> b -> c
flip f a b =
    f b a


sign : number -> number
sign x =
    if x > 0 then
        1

    else if x == 0 then
        0

    else
        -1


chunksOf : Int -> List a -> List (List a)
chunksOf size list =
    if List.isEmpty list then
        []

    else
        List.take size list :: chunksOf size (List.drop size list)
