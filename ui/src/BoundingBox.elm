module BoundingBox exposing (BoundingBox, encode)

import Json.Encode as E


type alias BoundingBox =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


encode : BoundingBox -> E.Value
encode box =
    E.object
        [ ( "min_x", E.float box.minX )
        , ( "max_x", E.float box.maxX )
        , ( "min_y", E.float box.minY )
        , ( "max_y", E.float box.maxY )
        ]
