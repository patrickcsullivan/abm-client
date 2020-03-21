module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program () State Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias State =
    { serverUrl : String
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


init : State
init =
    { serverUrl = ""
    , loadable = NotStarted
    }


type Msg
    = Connect
    | ChangeServerUrl String


update : Msg -> State -> State
update msg state =
    case msg of
        Connect ->
            { state
                | loadable = Loaded { mapWidth = 50, mapHeight = 50 }
            }

        ChangeServerUrl url ->
            { state
                | serverUrl = url
            }


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

                Loaded _ ->
                    statusMessageView "Connected."
    in
    div [ class "pane" ] [ content ]


statusMessageView : String -> Html Msg
statusMessageView msg =
    div [ class "status-message" ] [ text msg ]
