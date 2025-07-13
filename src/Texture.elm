module Texture exposing (Texture, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode


type alias Texture =
    { image : Encode.Value
    , width : Int
    , height : Int
    }


decoder : Decode.Decoder Texture
decoder =
    Decode.map3 Texture
        Decode.value
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)


encode : Texture -> Encode.Value
encode texture =
    Encode.object
        [ ( "image", texture.image )
        , ( "width", Encode.int texture.width )
        , ( "height", Encode.int texture.height )
        ]
