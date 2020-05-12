module Network.FromClient exposing (FromClient(..), encode)

import BoundingBox exposing (BoundingBox)
import Json.Encode as E


{-| `FromClient` is a mmessage is sent from a client to a simulation server.
-}
type FromClient
    = RegisterInterest BoundingBox


encode : FromClient -> E.Value
encode msg =
    case msg of
        RegisterInterest region ->
            E.object
                [ ( "RegisterInterest", encodeBoundingBox region ) ]


encodeBoundingBox : BoundingBox -> E.Value
encodeBoundingBox box =
    E.object
        [ ( "x_min", E.float box.xMin )
        , ( "x_max", E.float box.xMax )
        , ( "y_min", E.float box.yMin )
        , ( "y_max", E.float box.yMax )
        ]
