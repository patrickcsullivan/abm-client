module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program () State Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias State =
    { uiState : UIState
    , gameState : Maybe GameState
    }


type alias GameState =
    { mapWidth : Int
    , mapHeight : Int
    }


type alias UIState =
    { serverUrl : String
    }


init : State
init =
    { uiState = initUIState
    , gameState = Nothing
    }


initUIState : UIState
initUIState =
    { serverUrl = ""
    }


type Msg
    = Connect
    | ChangeServerUrl String


update : Msg -> State -> State
update msg state =
    case msg of
        Connect ->
            { state
                | gameState = Just { mapWidth = 50, mapHeight = 50 }
            }

        ChangeServerUrl url ->
            { state
                | uiState = { serverUrl = url }
            }


view : State -> Html Msg
view state =
    div [ class "page" ]
        [ div [ class "top-toolbar" ]
            [ input [ class "server-input", placeholder "Server URL", value state.uiState.serverUrl, onInput ChangeServerUrl ] []
            , a [ class "connect-button" ] [ text "Connect" ]
            ]
        , div [ class "pane" ]
            [ text "content" ]
        , div [ class "bottom-toolbar" ]
            [ text "toolbar" ]
        ]
