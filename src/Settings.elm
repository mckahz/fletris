port module Settings exposing
    ( Arr(..)
    , Handling
    , HandlingMsg(..)
    , Msg(..)
    , Sdf(..)
    , SetInputMode(..)
    , Settings
    , addMapping
    , decoder
    , default
    , encode
    , queueLength
    , setNextInput
    , startSetInputMode
    , update
    )

import Controller exposing (Button(..))
import Dict exposing (Dict)
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List


port saveSettings : Encode.Value -> Cmd msg


type Msg
    = HandlingMsg HandlingMsg
    | SetAll
    | ClearAll
    | ResetDefault
    | SavedSettings
    | Minimized


type HandlingMsg
    = DasChanged Float
    | SdfChanged Float
    | ArrChanged Float
    | DcdChanged Float


type alias Handling =
    { das : Float
    , arr : Arr
    , sdf : Sdf
    , dcd : Float
    }


type Arr
    = ArrGradual Float
    | ArrInfinite


type Sdf
    = SdfGradual Float
    | SdfInfinite


type alias Settings =
    { handling : Handling
    , controls : Dict String Controller.Button
    , setInputMode : SetInputMode
    , minimized : Bool
    }


maxSdf : Float
maxSdf =
    50


minArr : Float
minArr =
    0


queueLength : Int
queueLength =
    5


sdfToFloat : Sdf -> Float
sdfToFloat sdf =
    case sdf of
        SdfGradual f ->
            f

        SdfInfinite ->
            maxSdf


arrToFloat : Arr -> Float
arrToFloat arr =
    case arr of
        ArrGradual f ->
            f

        ArrInfinite ->
            minArr


toSdf : Float -> Sdf
toSdf float =
    if float >= maxSdf then
        SdfInfinite

    else
        SdfGradual float


toArr : Float -> Arr
toArr float =
    if float <= minArr then
        ArrInfinite

    else
        ArrGradual float


defaultKey : Button -> String
defaultKey button =
    case button of
        Hold ->
            "a"

        Left ->
            "s"

        SD ->
            "d"

        Right ->
            "f"

        Ccw ->
            "j"

        R180 ->
            "k"

        Cw ->
            "l"

        HD ->
            " "

        Undo ->
            "u"

        Redo ->
            "r"


default : Settings
default =
    { handling =
        { das = 150
        , sdf = SdfGradual 6
        , arr = ArrGradual 50
        , dcd = 10
        }
    , setInputMode = Inactive
    , minimized = True
    , controls =
        Controller.allButtons
            |> List.map (\b -> ( defaultKey b, b ))
            |> Dict.fromList
    }


addMapping : String -> Button -> Settings -> Settings
addMapping key button settings =
    { settings | controls = settings.controls |> Dict.insert key button }


type SetInputMode
    = Setting Button
    | Inactive


startSetInputMode : Settings -> Settings
startSetInputMode settings =
    { settings
        | setInputMode =
            Controller.allButtons
                |> List.head
                |> Maybe.map Setting
                |> Maybe.withDefault Inactive
    }


setNextInput : Settings -> Settings
setNextInput settings =
    { settings
        | setInputMode =
            let
                remainingButtons =
                    case settings.setInputMode of
                        Inactive ->
                            Controller.allButtons

                        Setting button ->
                            Controller.allButtons
                                |> List.dropWhile ((/=) button)
                                |> List.drop 1
            in
            case List.head remainingButtons of
                Nothing ->
                    Inactive

                Just button ->
                    Setting button
    }


handlingDecoder : Decode.Decoder Handling
handlingDecoder =
    Decode.map4 Handling
        (Decode.field "das" Decode.float)
        (Decode.field "arr" Decode.float |> Decode.map toArr)
        (Decode.field "sdf" Decode.float |> Decode.map toSdf)
        (Decode.field "dcd" Decode.float)


decoder : Decode.Decoder Settings
decoder =
    Decode.oneOf
        [ Decode.map4 Settings
            (Decode.field "handling" handlingDecoder)
            (Decode.field "controls" <| Decode.dict Controller.buttonDecoder)
            (Decode.succeed default.setInputMode)
            (Decode.succeed default.minimized)
        , Decode.null default
        ]


encodeHandling handling =
    Encode.object
        [ ( "das", Encode.float handling.das )
        , ( "arr", Encode.float <| arrToFloat handling.arr )
        , ( "sdf", Encode.float <| sdfToFloat handling.sdf )
        , ( "dcd", Encode.float handling.dcd )
        ]


encode : Settings -> Encode.Value
encode settings =
    Encode.object
        [ ( "handling", encodeHandling settings.handling )
        , ( "controls", Encode.dict identity Controller.buttonEncoder settings.controls )
        ]


updateHandling : HandlingMsg -> Handling -> Handling
updateHandling msg handling =
    case msg of
        DasChanged to ->
            { handling | das = to }

        SdfChanged to ->
            { handling | sdf = toSdf to }

        ArrChanged to ->
            { handling | arr = toArr to }

        DcdChanged to ->
            { handling | dcd = to }


update : Msg -> Settings -> ( Settings, Cmd msg )
update msg settings =
    case msg of
        HandlingMsg m ->
            ( { settings | handling = updateHandling m settings.handling }, Cmd.none )

        SavedSettings ->
            ( settings, saveSettings <| encode settings )

        SetAll ->
            ( settings, Cmd.none )

        Minimized ->
            ( { settings | minimized = not settings.minimized }, Cmd.none )

        ClearAll ->
            ( { settings | controls = Dict.empty }, Cmd.none )

        ResetDefault ->
            ( { settings | controls = default.controls }, Cmd.none )
