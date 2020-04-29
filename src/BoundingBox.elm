module BoundingBox exposing (BoundingBox, encode)

import Json.Encode as E


type alias BoundingBox =
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    }


encode : BoundingBox -> E.Value
encode box =
    E.object
        [ ( "x_min", E.float box.xMin )
        , ( "x_max", E.float box.xMax )
        , ( "y_min", E.float box.yMin )
        , ( "y_max", E.float box.yMax )
        ]
