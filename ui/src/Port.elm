port module Port exposing (connect, onConnectionOpened, registerInterest)

import Json.Decode as D
import Json.Encode as E


port connectPort : E.Value -> Cmd msg


port registerInterestPort : E.Value -> Cmd msg


port connectionOpenedPort : (E.Value -> msg) -> Sub msg


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
been opened.
-}
onConnectionOpened : (( Int, Int ) -> msg) -> msg -> Sub msg
onConnectionOpened withMapSize withoutMapSize =
    connectionOpenedPort
        (D.decodeValue mapSizeDecoder
            >> Result.toMaybe
            >> Maybe.map withMapSize
            >> Maybe.withDefault withoutMapSize
        )


mapSizeDecoder : D.Decoder ( Int, Int )
mapSizeDecoder =
    D.map2 (\x y -> ( x, y ))
        (D.field "x" D.int)
        (D.field "y" D.int)
