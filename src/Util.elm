module Util exposing (chunksOf, dropWhile, takeWhile, toHexString)

import Element exposing (Color, toRgb)


takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list =
    let
        iter l acc =
            case l of
                [] ->
                    acc

                h :: t ->
                    if p h then
                        iter t (h :: acc)

                    else
                        acc
    in
    iter list []
        |> List.reverse


dropWhile : (a -> Bool) -> List a -> List a
dropWhile p list =
    case list of
        [] ->
            []

        h :: t ->
            if p h then
                dropWhile p t

            else
                h :: t


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
