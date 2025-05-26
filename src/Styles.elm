module Styles exposing
    ( backgroundColor
    , boardColor
    , borderWidthPx
    , fontColor
    , highlightColor
    , iColor
    , jColor
    , junkColor
    , lColor
    , minoSize
    , oColor
    , sColor
    , sidebarWidth
    , tColor
    , zColor
    )

import Element exposing (Color, Length, px, rgb255)


minoSize : Float
minoSize =
    25


backgroundColor : Color
backgroundColor =
    rgb255 0x33 0x2D 0x3F


highlightColor : Color
highlightColor =
    rgb255 0x00 0x00 0x00


fontColor : Color
fontColor =
    rgb255 0xFF 0xFF 0xFF


sidebarWidth : Length
sidebarWidth =
    px 100


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
