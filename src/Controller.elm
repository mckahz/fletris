module Controller exposing
    ( Controller
    , default
    , press
    , release
    , isPressed
    , isReleased
    , Direction(..)
    , direction
    , Button(..)
    , buttonDecoder
    )

import Json.Decode as Decode

type Controller = Controller (List Button)

default : Controller
default = Controller []


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

type Direction = DLeft | Neutral | DRight

direction : Controller -> Direction
direction controller =
    case (isPressed Left controller, isPressed Right controller) of
        (True, False) -> DLeft
        (False, True) -> DRight
        (_, _) -> Neutral


buttonDecoder : Decode.Decoder (Maybe Button)
buttonDecoder =
  Decode.field "key" Decode.string
  |> Decode.map toButton

toButton : String -> Maybe Button
toButton string =
  case string of
    "a" -> Just Hold
    "A" -> Just Hold
    "s" -> Just Left
    "S" -> Just Left
    "d" -> Just SD
    "D" -> Just SD
    "f" -> Just Right
    "F" -> Just Right
    "j" -> Just Ccw
    "J" -> Just Ccw
    "k" -> Just R180
    "K" -> Just R180
    "l" -> Just Cw
    "L" -> Just Cw
    " " -> Just HD
    _ -> Nothing
