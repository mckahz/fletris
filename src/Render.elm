module Render exposing (Command, drawTexture, encodeCommand, resetPalette, setPalette)

import Json.Encode as Encode
import Texture exposing (Texture)


type Command
    = DrawTexture Float Float Texture
    | SetPalette Texture Int
    | ResetPalette


encodeCommand : Command -> Encode.Value
encodeCommand command =
    case command of
        SetPalette palette n ->
            Encode.object
                [ ( "fn", Encode.string "setPalette" )
                , ( "image", palette.image )
                , ( "n", Encode.int n )
                ]

        DrawTexture x y texture ->
            Encode.object
                [ ( "fn", Encode.string "drawImage" )
                , ( "image", texture.image )
                , ( "x", Encode.string <| String.fromFloat x )
                , ( "y", Encode.string <| String.fromFloat y )
                ]

        ResetPalette ->
            Encode.object
                [ ( "fn", Encode.string "resetPalette" ) ]


drawTexture : Float -> Float -> Texture -> Command
drawTexture =
    DrawTexture


setPalette : Texture -> Int -> Command
setPalette =
    SetPalette


resetPalette : Command
resetPalette =
    ResetPalette
