module Network.ToClient exposing (CellUpdate, ToClient, decoder)

import Json.Decode as D
import Maybe.Extra as Maybe


{-| `ToClient` is a message that is sent to a client from a simulation server.
-}
type alias ToClient =
    { cellUpdates : List CellUpdate
    }


type alias CellUpdate =
    { x : Int
    , y : Int
    , growthAmt : Int
    }


decoder : D.Decoder ToClient
decoder =
    let
        updateListDecoder =
            D.list (D.maybe cellUpdateDecoder)
                |> D.map Maybe.values
    in
    D.field "cell_updates" updateListDecoder
        |> D.map (\ups -> { cellUpdates = ups })


cellUpdateDecoder : D.Decoder CellUpdate
cellUpdateDecoder =
    D.map3 (\x y g -> { x = x, y = y, growthAmt = g })
        (D.field "x" D.int)
        (D.field "y" D.int)
        (D.field "growth_amt" D.int)
