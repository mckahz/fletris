module Tetromino.Orientation exposing
    ( Orientation(..)
    , ccw
    , cw
    , decode
    , encode
    , toRad
    )

import Json.Decode as Decode
import Json.Encode as Encode



-- 270 == west
-- 90 == east


encode : Orientation -> Encode.Value
encode orientation =
    Encode.int <|
        case orientation of
            North ->
                0

            East ->
                1

            South ->
                2

            West ->
                3


decode : Decode.Decoder Orientation
decode =
    Decode.int
        |> Decode.andThen
            (\i ->
                case i of
                    0 ->
                        Decode.succeed North

                    1 ->
                        Decode.succeed East

                    2 ->
                        Decode.succeed South

                    3 ->
                        Decode.succeed West

                    _ ->
                        Decode.fail "invalid rotation"
            )


type Orientation
    = North
    | West
    | South
    | East


toRad : Orientation -> Float
toRad orientation =
    case orientation of
        North ->
            0

        East ->
            pi / 2

        South ->
            pi

        West ->
            -pi / 2


ccw : Orientation -> Orientation
ccw orientation =
    case orientation of
        North ->
            West

        East ->
            North

        South ->
            East

        West ->
            South


cw : Orientation -> Orientation
cw orientation =
    case orientation of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North
