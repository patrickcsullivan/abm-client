module Cell exposing (Cell, decoder)

import Json.Decode as D


type alias Cell =
    { pos : ( Int, Int )
    , grass : Int
    }


decoder : D.Decoder Cell
decoder =
    D.map3 (\x y g -> { pos = ( x, y ), grass = g })
        (D.field "x" D.int)
        (D.field "y" D.int)
        (D.field "grass" D.int)
