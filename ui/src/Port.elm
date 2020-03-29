port module Port exposing (connect, onConnectionOpened, onSimUpdated, registerInterest)

import BoundingBox exposing (BoundingBox)
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import SimUpdate exposing (SimUpdate(..))


port connectPort : E.Value -> Cmd msg


port registerInterestPort : E.Value -> Cmd msg


port connectionOpenedPort : (E.Value -> msg) -> Sub msg


port simUpdatedPort : (E.Value -> msg) -> Sub msg


{-| Attempt to connect to a server at the given url.
-}
connect : String -> Cmd msg
connect =
    E.string >> connectPort


{-| Register interest in a region of the map.
-}
registerInterest : BoundingBox -> Cmd msg
registerInterest =
    BoundingBox.encode >> registerInterestPort


{-| Creates message when a port communicates that a connection to the server has
been opened.
-}
onConnectionOpened : msg -> Sub msg
onConnectionOpened msg =
    connectionOpenedPort (\_ -> msg)


{-| Creates message when a port communicates a list of simulation updates.
-}
onSimUpdated : (List SimUpdate -> msg) -> msg -> Sub msg
onSimUpdated onDecodeSuccess onDecodeFail =
    let
        decoder =
            D.list (D.maybe SimUpdate.decoder)
    in
    simUpdatedPort
        (D.decodeValue decoder
            >> Result.toMaybe
            >> Maybe.map (Maybe.values >> onDecodeSuccess)
            >> Maybe.withDefault onDecodeFail
        )
