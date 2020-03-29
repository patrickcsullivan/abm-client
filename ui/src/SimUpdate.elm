module SimUpdate exposing (SimUpdate(..), decoder)

import Agent exposing (Agent)
import Cell exposing (Cell)
import Json.Decode as D


type SimUpdate
    = SimTime Float
    | CellUpdate Cell
    | AgentUpdate Agent


decoder : D.Decoder SimUpdate
decoder =
    D.oneOf
        [ D.field "SimTime" D.float |> D.map SimTime
        , D.field "CellUpdate" Cell.decoder |> D.map CellUpdate
        , D.field "AgentUpdate" Agent.decoder |> D.map AgentUpdate
        ]
