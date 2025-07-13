module Resources exposing (Resources, decoder)

import Json.Decode as Decode
import Texture exposing (Texture)


type alias Resources =
    { mino : Texture
    , palette : Texture
    , vines : Texture
    , text : Texture
    , decor : Texture
    , grid : Texture
    }


decoder : Decode.Decoder Resources
decoder =
    Decode.map6 Resources
        (Decode.field "mino" Texture.decoder)
        (Decode.field "palette" Texture.decoder)
        (Decode.field "vines" Texture.decoder)
        (Decode.field "text" Texture.decoder)
        (Decode.field "decor" Texture.decoder)
        (Decode.field "grid" Texture.decoder)
