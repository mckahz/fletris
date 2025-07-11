module Util exposing (chunksOf, flip, sign, toHexString)

import Element exposing (Color, toRgb)


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


toHexString : Color -> String
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
            toRgb color

        ired =
            floor <| 255 * red

        igreen =
            floor <| 255 * green

        iblue =
            floor <| 255 * blue

        ialpha =
            floor <| 255 * alpha
    in
    "#" ++ intToHex ired ++ intToHex igreen ++ intToHex iblue


chunksOf : Int -> List a -> List (List a)
chunksOf size list =
    if List.isEmpty list then
        []

    else
        List.take size list :: chunksOf size (List.drop size list)
