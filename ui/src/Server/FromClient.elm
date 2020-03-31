module Server.FromClient exposing (FromClient(..), encode)

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
        [ ( "min_x", E.float box.minX )
        , ( "max_x", E.float box.maxX )
        , ( "min_y", E.float box.minY )
        , ( "max_y", E.float box.maxY )
        ]
