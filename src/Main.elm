module Main exposing (Msg(..), main, update, view)

import Browser
import Browser.Events exposing (onResize)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Html exposing (Html, a, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)


main : Program ( Int, Int ) State Msg
main =
    Browser.element
        { init = \windowSize -> init windowSize
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.batch [ Browser.Events.onResize ResizeWindow ]
        }


type alias State =
    { windowSize : ( Int, Int )
    , resources : Resources
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


init : ( Int, Int ) -> ( State, Cmd Msg )
init windowSize =
    ( { windowSize = windowSize
      , resources = Resources.init
      , serverUrl = ""
      , loadable = NotStarted
      }
    , Cmd.map LoadResources
        (Resources.loadTextures
            [ "resources/grass0.png"
            , "resources/grass1.png"
            , "resources/grass2.png"
            , "resources/grass3.png"
            , "resources/grass4.png"
            ]
        )
    )


type Msg
    = ResizeWindow Int Int
    | LoadResources Resources.Msg
    | Connect
    | ChangeServerUrl String


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ResizeWindow x y ->
            { state
                | windowSize = ( x, y )
            }
                |> withNoCmd

        LoadResources rMsg ->
            { state
                | resources = Resources.update rMsg state.resources
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
