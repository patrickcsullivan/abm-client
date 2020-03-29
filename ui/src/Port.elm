port module Port exposing (connect, onConnectionOpened, onSimUpdated, registerInterest)

import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as Maybe
import SimUpdate exposing (SimUpdate(..))


port connectPort : E.Value -> Cmd msg


port registerInterestPort : E.Value -> Cmd msg


port connectionOpenedPort : (E.Value -> msg) -> Sub msg


port simUpdatedPort : (E.Value -> msg) -> Sub msg


{-| Attempt to connect to the server.
-}
connect : String -> Cmd msg
connect url =
    connectPort (E.string url)


{-| Register interest in a region of the map.
-}
registerInterest : Float -> Float -> Float -> Float -> Cmd msg
registerInterest minX maxX minY maxY =
    [ ( "minX", E.float minX )
    , ( "maxX", E.float maxX )
    , ( "minY", E.float minY )
    , ( "maxY", E.float maxY )
    ]
        |> E.object
        |> registerInterestPort


{-| Creates message when a port communicates that a connection to the server has
been opened and provides map size.
-}
onConnectionOpened : (( Int, Int ) -> msg) -> msg -> Sub msg
onConnectionOpened onDecodeSuccess onDecodeFail =
    connectionOpenedPort
        (D.decodeValue mapSizeDecoder
            >> Result.toMaybe
            >> Maybe.map onDecodeSuccess
            >> Maybe.withDefault onDecodeFail
        )


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


mapSizeDecoder : D.Decoder ( Int, Int )
mapSizeDecoder =
    D.map2 (\x y -> ( x, y ))
        (D.field "x" D.int)
        (D.field "y" D.int)
