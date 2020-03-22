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
import Keyboard
import Keyboard.Arrows


main : Program ( Int, Int ) State Msg
main =
    Browser.element
        { init = \windowSize -> init windowSize
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onResize ResizeWindow
                    , Browser.Events.onAnimationFrameDelta Tick
                    , Sub.map PressKeys Keyboard.subscriptions
                    ]
        }



-- MODEL


type alias State =
    { time : Float -- time in ms
    , windowSize : ( Int, Int )
    , keys : List Keyboard.Key -- keys currently pressed
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
    , cells : List Cell
    , agents : List Agent
    , camera : Camera
    }


type alias Cell =
    { pos : ( Int, Int )
    , grass : Int
    }


type alias Agent =
    { pos : ( Float, Float )
    }


init : ( Int, Int ) -> ( State, Cmd Msg )
init windowSize =
    ( { time = 0
      , windowSize = windowSize
      , keys = []
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



-- UPDATE / MESSAGE


type Msg
    = ChangeServerUrl String
    | Connect
    | LoadResources Resources.Msg
    | PressKeys Keyboard.Msg
    | ResizeWindow Int Int
    | Tick Float


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ChangeServerUrl url ->
            { state
                | serverUrl = url
            }
                |> withNoCmd

        Connect ->
            { state
                | loadable = Loaded testGameState
            }
                |> withNoCmd

        LoadResources rMsg ->
            { state
                | resources = Resources.update rMsg state.resources
            }
                |> withNoCmd

        PressKeys kMsg ->
            { state
                | keys = Keyboard.update kMsg state.keys
            }
                |> withNoCmd

        ResizeWindow x y ->
            { state
                | windowSize = ( x, y )
            }
                |> withNoCmd

        Tick dt ->
            { state
                | time = state.time + dt
                , loadable =
                    state.loadable
                        |> mapLoadable (mapCamera (moveCamera state.keys dt))
            }
                |> withNoCmd


withNoCmd : a -> ( a, Cmd Msg )
withNoCmd x =
    ( x, Cmd.none )


mapLoadable : (a -> b) -> Loadable a -> Loadable b
mapLoadable f loadable =
    case loadable of
        NotStarted ->
            NotStarted

        Loading ->
            Loading

        Error e ->
            Error e

        Loaded a ->
            Loaded (f a)


mapCamera : (Camera -> Camera) -> GameState -> GameState
mapCamera f gs =
    { gs
        | camera = f gs.camera
    }


moveCamera : List Keyboard.Key -> Float -> Camera -> Camera
moveCamera keys dt camera =
    let
        arrows =
            Keyboard.Arrows.arrows keys

        v =
            -- 1 unit per second
            1

        dx =
            toFloat arrows.x * v * dt / 1000

        dy =
            toFloat arrows.y * v * dt / 1000
    in
    Camera.moveBy ( dx, dy ) camera



-- VIEW


view : State -> Html Msg
view state =
    div [ class "page" ]
        [ topToolbarView state.serverUrl
        , paneView state.time state.windowSize state.resources state.loadable
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
    div [ class "bottom-toolbar" ] []


paneView : Float -> ( Int, Int ) -> Resources -> Loadable GameState -> Html Msg
paneView time windowSize resources loadable =
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
                    gameView time windowSize gs.camera resources gs.cells gs.agents
    in
    div [ class "pane" ] [ content ]


statusMessageView : String -> Html Msg
statusMessageView msg =
    div [ class "status-message" ] [ text msg ]


gameView : Float -> ( Int, Int ) -> Camera -> Resources -> List Cell -> List Agent -> Html Msg
gameView time windowSize camera resources cells agents =
    Game.render
        { time = time / 1000
        , size = windowSize
        , camera = camera
        }
        (render resources cells agents)


render : Resources -> List Cell -> List Agent -> List Renderable
render resources _ _ =
    [ Render.spriteWithOptions
        { position = ( 0, 0, 0 )
        , size = ( 10, 5 )
        , texture = Resources.getTexture "resources/grass4.png" resources
        , rotation = 0
        , pivot = ( 0, 0 )
        , tiling = ( 10, 5 )
        }
    ]



-- TEST DATA


testGameState : GameState
testGameState =
    { mapWidth = 50
    , mapHeight = 50
    , cells = testCells
    , agents = testAgents
    , camera = Camera.custom (\( w, h ) -> ( w / 100, h / 100 )) ( 0, 0 )
    }


testCells : List Cell
testCells =
    posRange ( 0, 0 ) ( 25, 25 )
        |> List.map
            (\p ->
                { pos = p
                , grass = modBy 4 (Tuple.first p + Tuple.second p)
                }
            )


testAgents : List Agent
testAgents =
    posRange ( 5, 3 ) ( 12, 8 )
        |> List.map
            (\p ->
                { pos =
                    ( (Tuple.first p |> toFloat) + 0.5
                    , (Tuple.second p |> toFloat) + 0.5
                    )
                }
            )


posRange : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
posRange ( xi, yi ) ( xf, yf ) =
    let
        xs =
            List.range xi xf
    in
    List.range yi yf
        |> List.concatMap (\y -> List.map (\x -> ( x, y )) xs)
