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
    , view
    )

import Controller exposing (Button(..))
import Dict exposing (Dict)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Styles


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


labelAttrs : List (E.Attribute msg)
labelAttrs =
    [ E.centerY
    , E.height (E.px 25)
    , E.width (E.px 120)
    ]


view : Settings -> Element Msg
view settings =
    let
        maxWidth =
            800
    in
    E.column
        [ E.width
            (if settings.minimized then
                E.shrink

             else
                E.fill |> E.maximum maxWidth
            )
        , Styles.defaultSpacing
        , Background.color Styles.backgroundColor
        , Border.color Styles.highlightColor
        , Border.widthEach
            { bottom = Styles.borderWidthPx
            , top = 0
            , left = 0
            , right = Styles.borderWidthPx
            }
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = 0
            , bottomRight = Styles.borderRadiusPx
            }
        ]
        (E.row
            [ Background.color Styles.highlightColor
            , E.width E.fill
            , Styles.defaultPadding
            ]
            [ Input.button
                [ E.htmlAttribute <| Html.Attributes.id "minimize-settings-button"
                ]
                { onPress = Just Minimized
                , label =
                    E.row [ Styles.defaultSpacing, Styles.defaultPadding ]
                        [ if settings.minimized then
                            E.text ">"

                          else
                            E.text "V"
                        , E.el Styles.heading <| E.text "SETTINGS"
                        ]
                }
            ]
            :: (if settings.minimized then
                    []

                else
                    [ E.map HandlingMsg <| viewHandling settings.handling
                    , viewControls settings
                    , Input.button [ E.alignRight, E.htmlAttribute <| Html.Attributes.id "save-settings-button" ]
                        { onPress = Just SavedSettings
                        , label = E.text "SAVE"
                        }
                    ]
               )
        )


viewControls : Settings -> Element Msg
viewControls settings =
    let
        width =
            Controller.allButtons
                |> List.map
                    (\b ->
                        [ name b
                        , arrowStr
                        , keys b
                        ]
                            |> List.map String.length
                            |> List.sum
                    )
                |> List.maximum
                |> Maybe.map ((+) 2)
                |> Maybe.map ((+) (String.length arrowStr))
                |> Maybe.withDefault 8

        name button =
            case button of
                Cw ->
                    "ROTATE CLOCKWISE"

                R180 ->
                    "ROTATE 180 DEGREES"

                Ccw ->
                    "ROTATE COUNTER-CLOCKWISE"

                Left ->
                    "MOVE LEFT"

                Right ->
                    "MOVE RIGHT"

                HD ->
                    "HARD DROP"

                SD ->
                    "SOFT DROP"

                Hold ->
                    "HOLD"

                Undo ->
                    "UNDO"

                Redo ->
                    "REDO"

        format str =
            if str == " " then
                "[SPACE]"

            else if String.length str > 1 then
                "[" ++ str ++ "]"

            else
                String.toUpper str

        keys button =
            settings.controls
                |> Dict.toList
                |> List.filter (Tuple.second >> (==) button)
                |> List.map Tuple.first
                |> List.map format
                |> String.join ","

        arrowStr =
            "-> "

        arrow button =
            if settings.setInputMode == Setting button then
                arrowStr

            else
                ""

        mappingAsString button =
            let
                n =
                    name button

                a =
                    arrow button

                k =
                    keys button

                dots =
                    String.repeat (width - String.length n - String.length a - String.length k) "."
            in
            n ++ dots ++ a ++ k
    in
    E.column [ E.width E.fill, Styles.defaultPadding ]
        ([ E.el Styles.subheading <| E.text "CONTROLS"
         , Input.button [ E.htmlAttribute <| Html.Attributes.id "reset-default-button" ]
            { onPress = Just ResetDefault
            , label = E.text "RESET TO DEFAULT"
            }
         , Input.button [ E.htmlAttribute <| Html.Attributes.id "clear-all-button" ]
            { onPress = Just ClearAll
            , label = E.text "CLEAR ALL"
            }
         , Input.button [ E.htmlAttribute <| Html.Attributes.id "set-all-button" ]
            { onPress = Just SetAll
            , label = E.text "SET ALL"
            }
         ]
            ++ List.map (\button -> E.text <| mappingAsString button) Controller.allButtons
        )


viewHandling : Handling -> Element HandlingMsg
viewHandling handling =
    E.column [ E.width E.fill, Styles.defaultPadding, E.spacing 5 ]
        [ E.el Styles.subheading <| E.text "HANDLING"
        , Input.slider Styles.sliderTrack
            { onChange = DasChanged
            , label =
                Input.labelAbove labelAttrs
                    (E.text <|
                        "DAS ("
                            ++ String.fromFloat handling.das
                            ++ "ms)"
                    )
            , min = 1
            , max = 300
            , value = handling.das
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        , Input.slider
            Styles.sliderTrack
            { onChange = ArrChanged
            , label =
                Input.labelAbove labelAttrs
                    (E.text <|
                        case handling.arr of
                            ArrGradual f ->
                                "ARR (" ++ String.fromFloat f ++ " ms)"

                            ArrInfinite ->
                                "ARR (INSTANT)"
                    )
            , min = 0
            , max = 100
            , value = arrToFloat handling.arr
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        , Input.slider
            Styles.sliderTrack
            { onChange = SdfChanged
            , label =
                Input.labelAbove labelAttrs
                    (E.text <|
                        case handling.sdf of
                            SdfGradual f ->
                                "SDF (x" ++ String.fromFloat f ++ ")"

                            SdfInfinite ->
                                "SDF (INSTANT)"
                    )
            , min = 2
            , max = maxSdf
            , value = sdfToFloat handling.sdf
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        , Input.slider Styles.sliderTrack
            { onChange = DcdChanged
            , label =
                Input.labelAbove labelAttrs (E.text <| "DCD (" ++ String.fromFloat handling.dcd ++ "ms)")
            , min = 0
            , max = 333
            , value = handling.dcd
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        ]
