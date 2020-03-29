module Agent exposing (Agent, decoder)

import Json.Decode as D


type alias Agent =
    { pos : ( Float, Float )
    }


decoder : D.Decoder Agent
decoder =
    D.map2 (\x y -> { pos = ( x, y ) })
        (D.field "x" D.float)
        (D.field "y" D.float)
