port module Server.Port exposing (connect, onConnectionOpened, onReceiveFromServer, sendToServer)

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import Server.FromClient exposing (FromClient)
import Server.ToClient exposing (ToClient)
import SimUpdate exposing (SimUpdate(..))


port connectPort : E.Value -> Cmd msg


port sendToServerPort : E.Value -> Cmd msg


port connectionOpenedPort : (E.Value -> msg) -> Sub msg


port recieveFromServerPort : (E.Value -> msg) -> Sub msg


{-| Attempts to connect to a server at the given url.
-}
connect : String -> Cmd msg
connect =
    E.string >> connectPort


{-| Sends a message to the connected server.
-}
sendToServer : FromClient -> Cmd msg
sendToServer =
    Server.FromClient.encode >> sendToServerPort


{-| Creates message when a port communicates that a connection to the server has
been opened.
-}
onConnectionOpened : msg -> Sub msg
onConnectionOpened msg =
    connectionOpenedPort (\_ -> msg)


{-| Creates message when the connected server sends a message to the client.
-}
onReceiveFromServer : (ToClient -> msg) -> msg -> Sub msg
onReceiveFromServer onDecodeSuccess onDecodeFail =
    recieveFromServerPort
        (D.decodeValue Server.ToClient.decoder
            >> Result.toMaybe
            >> Maybe.map onDecodeSuccess
            >> Maybe.withDefault onDecodeFail
        )
