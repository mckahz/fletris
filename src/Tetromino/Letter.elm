module Tetromino.Letter exposing
    ( Letter(..)
    , all
    , color
    , decode
    , encode
    , generator
    , toInt
    , toString
    )

import Color exposing (Color)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Util exposing (hex)


type Letter
    = I
    | L
    | J
    | S
    | Z
    | O
    | T


all : List Letter
all =
    [ I
    , L
    , J
    , S
    , Z
    , O
    , T
    ]


generator : Random.Generator Letter
generator =
    Random.uniform I [ J, L, S, Z, T, O ]


toInt : Letter -> Int
toInt letter =
    case letter of
        I ->
            0

        L ->
            1

        J ->
            2

        S ->
            3

        Z ->
            4

        O ->
            5

        T ->
            6


encode : Letter -> Encode.Value
encode =
    Encode.int << toInt


decode : Decode.Decoder Letter
decode =
    Decode.int
        |> Decode.map
            (\i ->
                case i of
                    1 ->
                        L

                    2 ->
                        J

                    3 ->
                        S

                    4 ->
                        Z

                    5 ->
                        O

                    6 ->
                        T

                    _ ->
                        I
            )


toString : Letter -> String
toString letter =
    case letter of
        Z ->
            "Z"

        L ->
            "L"

        O ->
            "O"

        S ->
            "S"

        I ->
            "I"

        J ->
            "J"

        T ->
            "T"


color : Letter -> Color
color letter =
    case letter of
        Z ->
            hex 0xED6E6F00

        L ->
            hex 0xE74AE800

        O ->
            hex 0xF6CB5700

        S ->
            hex 0x7CFC9C00

        I ->
            hex 0x98FCF800

        J ->
            hex 0x4646F500

        T ->
            hex 0x9740C300
