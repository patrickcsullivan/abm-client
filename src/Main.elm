module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Events exposing (onResize)
import Html exposing (Html, a, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program ( Int, Int ) State Msg
main =
    Browser.element
        { init = \windowSize -> ( init windowSize, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.batch [ Browser.Events.onResize WindowResize ]
        }


type alias State =
    { windowSize : ( Int, Int )
    , serverUrl : String
    , loadable : Loadable GameState
    }


type Loadable a
    = NotStarted
    | Loading
    | Error String
    | Loaded a


type alias GameState =
    { mapWidth : Int
    , mapHeight : Int
    }


init : ( Int, Int ) -> State
init windowSize =
    { windowSize = windowSize
    , serverUrl = ""
    , loadable = NotStarted
    }


type Msg
    = WindowResize Int Int
    | Connect
    | ChangeServerUrl String


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        WindowResize x y ->
            { state
                | windowSize = ( x, y )
            }
                |> withNoCmd

        Connect ->
            { state
                | loadable = Loaded { mapWidth = 50, mapHeight = 50 }
            }
                |> Debug.log "State"
                |> withNoCmd

        ChangeServerUrl url ->
            { state
                | serverUrl = url
            }
                |> withNoCmd


withNoCmd : a -> ( a, Cmd Msg )
withNoCmd x =
    ( x, Cmd.none )


view : State -> Html Msg
view state =
    div [ class "page" ]
        [ topToolbarView state.serverUrl
        , paneView state.loadable
        , bottomToolbarView
        ]


topToolbarView : String -> Html Msg
topToolbarView serverUrl =
    div [ class "top-toolbar" ]
        [ input [ class "server-input", placeholder "Server URL", value serverUrl, onInput ChangeServerUrl ] []
        , a [ class "connect-button", onClick Connect ] [ text "Connect" ]
        ]


bottomToolbarView : Html Msg
bottomToolbarView =
    div [ class "bottom-toolbar" ] [ text "toolbar" ]


paneView : Loadable GameState -> Html Msg
paneView loadable =
    let
        content =
            case loadable of
                NotStarted ->
                    statusMessageView "Connect to a server to start."

                Loading ->
                    statusMessageView "Connecting."

                Error e ->
                    statusMessageView ("Error occured: " ++ e)

                Loaded gs ->
                    gameView gs
    in
    div [ class "pane" ] [ content ]


statusMessageView : String -> Html Msg
statusMessageView msg =
    div [ class "status-message" ] [ text msg ]


gameView : GameState -> Html Msg
gameView _ =
    div [ class "status-message" ] [ text "Connected." ]
