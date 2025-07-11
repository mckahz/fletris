module Controller exposing
    ( Button(..)
    , Controller
    , allButtons
    , buttonDecoder
    , buttonEncoder
    , default
    , direction
    , isPressed
    , isReleased
    , keyDecoder
    , press
    , release
    )

import Json.Decode as Decode
import Json.Encode as Encode


type Controller
    = Controller (List Button)


default : Controller
default =
    Controller []


allButtons : List Button
allButtons =
    [ Left, Right, Cw, R180, Ccw, Hold, SD, HD, Undo, Redo ]


press : Button -> Controller -> Controller
press button (Controller controller) =
    Controller <| button :: controller


release : Button -> Controller -> Controller
release button (Controller controller) =
    Controller <| List.filter ((/=) button) controller


isPressed : Button -> Controller -> Bool
isPressed button (Controller controller) =
    List.member button controller


isReleased : Button -> Controller -> Bool
isReleased button (Controller controller) =
    not (List.member button controller)


type Button
    = Left
    | Right
    | Cw
    | Ccw
    | R180
    | SD
    | HD
    | Hold
    | Undo
    | Redo


direction : Controller -> Int
direction controller =
    case ( isPressed Left controller, isPressed Right controller ) of
        ( True, False ) ->
            -1

        ( False, True ) ->
            1

        ( _, _ ) ->
            0


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map String.toLower


buttonDecoder : Decode.Decoder Button
buttonDecoder =
    Decode.int
        |> Decode.map
            (\int ->
                case int of
                    0 ->
                        HD

                    1 ->
                        SD

                    2 ->
                        Hold

                    3 ->
                        Left

                    4 ->
                        Right

                    5 ->
                        Cw

                    6 ->
                        R180

                    7 ->
                        Ccw

                    8 ->
                        Undo

                    _ ->
                        Redo
            )


buttonEncoder : Button -> Encode.Value
buttonEncoder button =
    Encode.int <|
        case button of
            HD ->
                0

            SD ->
                1

            Hold ->
                2

            Left ->
                3

            Right ->
                4

            Cw ->
                5

            R180 ->
                6

            Ccw ->
                7

            Undo ->
                8

            Redo ->
                9
