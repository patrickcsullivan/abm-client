port module Network.Port exposing
    ( connect
    , onConnectionError
    , onConnectionOpened
    , onReceiveFromServer
    , sendToServer
    )

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Network.Message.Incoming exposing (Incoming)
import Network.Message.Outgoing exposing (Outgoing)


port connectPort : E.Value -> Cmd msg


port sendToServerPort : E.Value -> Cmd msg


port connectionOpenedPort : (E.Value -> msg) -> Sub msg


port connectionErrorPort : (E.Value -> msg) -> Sub msg


port recieveFromServerPort : (E.Value -> msg) -> Sub msg


{-| Attempts to connect to a server at the given url.
-}
connect : String -> Cmd msg
connect =
    E.string >> connectPort


{-| Sends a message to the connected server.
-}
sendToServer : Outgoing -> Cmd msg
sendToServer =
    Network.Message.Outgoing.encode >> sendToServerPort


{-| Creates message when a port communicates that a connection to the server has
been opened.
-}
onConnectionOpened : msg -> Sub msg
onConnectionOpened msg =
    connectionOpenedPort (\_ -> msg)


{-| Creates message when a port communicates that a connection error has
occured.
-}
onConnectionError : (Maybe String -> msg) -> Sub msg
onConnectionError msg =
    connectionErrorPort (D.decodeValue D.string >> Result.toMaybe >> msg)


{-| Creates message when the connected server sends a message to the client.
-}
onReceiveFromServer : (Incoming -> msg) -> msg -> Sub msg
onReceiveFromServer onDecodeSuccess onDecodeFail =
    recieveFromServerPort
        (D.decodeValue Network.Message.Incoming.decoder
            >> Result.toMaybe
            >> Maybe.map onDecodeSuccess
            >> Maybe.withDefault onDecodeFail
        )
