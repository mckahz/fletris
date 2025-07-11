module Tetromino.Letter exposing
    ( Letter(..)
    , color
    , decode
    , encode
    , generator
    , toString
    )

import Element exposing (Color)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Styles


type Letter
    = I
    | L
    | J
    | S
    | Z
    | O
    | T


generator : Random.Generator Letter
generator =
    Random.uniform I [ J, L, S, Z, T, O ]


encode : Letter -> Encode.Value
encode letter =
    Encode.int <|
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
            Styles.zColor

        L ->
            Styles.lColor

        O ->
            Styles.oColor

        S ->
            Styles.sColor

        I ->
            Styles.iColor

        J ->
            Styles.jColor

        T ->
            Styles.tColor
