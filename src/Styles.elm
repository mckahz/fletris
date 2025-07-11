module Styles exposing
    ( backgroundColor
    , boardColor
    , borderRadiusPx
    , borderWidthPx
    , debugMagenta
    , defaultPadding
    , defaultSpacing
    , fontColor
    , ghostColor
    , gridColor
    , gridWidth
    , heading
    , highlightColor
    , iColor
    , jColor
    , junkColor
    , lColor
    , minoSize
    , oColor
    , sColor
    , sidebarWidthPx
    , sliderTrack
    , subheading
    , tColor
    , zColor
    )

import Element as E exposing (Color, Length, px, rgb255, rgba255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


defaultSpacing : E.Attribute msg
defaultSpacing =
    E.spacing 4


defaultPadding : E.Attribute msg
defaultPadding =
    E.padding 4


sliderTrack : List (E.Attribute msg)
sliderTrack =
    [ E.height (E.px 30)
    , E.behindContent
        (E.el
            [ E.width E.fill
            , E.height (E.px 2)
            , E.centerY
            , Background.color sliderTrackColor
            , Border.rounded 2
            ]
            E.none
        )
    ]


heading : List (E.Attribute msg)
heading =
    [ Font.size 32
    ]


subheading : List (E.Attribute msg)
subheading =
    [ Font.size 25
    ]


debugMagenta : Color
debugMagenta =
    rgba255 255 100 255 255


gridWidth : Float
gridWidth =
    0.5


gridColor : Color
gridColor =
    rgb255 255 255 255


sliderTrackColor : Color
sliderTrackColor =
    rgb255 0 0 0


minoSize : Float
minoSize =
    20


backgroundColor : Color
backgroundColor =
    rgb255 0x33 0x2D 0x3F


highlightColor : Color
highlightColor =
    rgb255 0x00 0x00 0x00


fontColor : Color
fontColor =
    rgb255 0xFF 0xFF 0xFF


borderRadiusPx : Int
borderRadiusPx =
    5


sidebarWidthPx : Int
sidebarWidthPx =
    100


borderWidthPx : Int
borderWidthPx =
    2


zColor =
    rgb255 0xED 0x6E 0x6F


lColor =
    rgb255 0xE7 0x4A 0xE8


oColor =
    rgb255 0xF6 0xCB 0x57


sColor =
    rgb255 0x7C 0xFC 0x9C


iColor =
    rgb255 0x98 0xFC 0xF8


jColor =
    rgb255 0x46 0x46 0xF5


tColor =
    rgb255 0x97 0x40 0xC3


junkColor =
    rgb255 0x99 0x99 0x99


boardColor =
    rgb255 0x22 0x22 0x22


ghostColor =
    rgba255 0xDD 0xDD 0xDD 0x00
