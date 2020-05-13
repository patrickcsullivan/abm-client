module Network.Message.Incoming exposing (AgentState, Incoming, decoder)

import Json.Decode as D


{-| `Incoming` is a message that is sent to a client from a simulation server.
-}
type alias Incoming =
    { agentStates : List AgentState
    }


type alias AgentState =
    { position : ( Float, Float )
    , heading : Float
    }


decoder : D.Decoder Incoming
decoder =
    D.field "agent_states" (D.list agentStateDecoder)
        |> D.map (\states -> { agentStates = states })


agentStateDecoder : D.Decoder AgentState
agentStateDecoder =
    D.map2 (\pos h -> { position = pos, heading = h })
        (D.field "position" positionDecoder)
        (D.field "heading" D.float)


positionDecoder : D.Decoder ( Float, Float )
positionDecoder =
    D.map2 (\x y -> ( x, y ))
        (D.index 0 D.float)
        (D.index 1 D.float)
