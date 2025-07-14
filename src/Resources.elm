module Resources exposing (Resources, decoder)

import Json.Decode as Decode
import Texture exposing (Texture)


type alias Resources =
    { mino : Texture
    , minoPalette : Texture
    , ghost : Texture
    , ghostPalette : Texture
    , vines : Texture
    , text : Texture
    , decor : Texture
    , grid : Texture
    }


map8 fn =
    Decode.map8 fn


decoder : Decode.Decoder Resources
decoder =
    map8 Resources
        (Decode.field "mino" Texture.decoder)
        (Decode.field "mino-palette" Texture.decoder)
        (Decode.field "ghost" Texture.decoder)
        (Decode.field "ghost-palette" Texture.decoder)
        (Decode.field "vines" Texture.decoder)
        (Decode.field "text" Texture.decoder)
        (Decode.field "decor" Texture.decoder)
        (Decode.field "grid" Texture.decoder)
