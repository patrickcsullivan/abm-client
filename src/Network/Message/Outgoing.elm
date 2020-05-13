module Network.Message.Outgoing exposing (Outgoing(..), encode)

import Json.Encode as E


{-| `Outgoing` is a mmessage is sent from a client to a simulation server.
-}
type Outgoing
    = Outgoing


encode : Outgoing -> E.Value
encode _ =
    E.object []
