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
import Json.Encode as Encode
import Keyboard
import Keyboard.Arrows
import Port


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
                    , Port.onConnectionOpened ConnectionOpened NoOp
                    ]
        }



-- MODEL


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
    { time : Float -- time in ms
    , keys : List Keyboard.Key -- keys currently pressed
    , resources : Resources
    , mapWidth : Int
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
    ( { serverUrl = ""
      , windowSize = windowSize
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


initGameState : ( Int, Int ) -> ( GameState, Cmd Msg )
initGameState ( mapWidth, mapHeight ) =
    ( { time = 0
      , keys = []
      , resources = Resources.init
      , mapWidth = mapWidth
      , mapHeight = mapHeight
      , cells = testCells ( mapWidth, mapHeight )
      , agents = testAgents
      , camera =
            Camera.custom
                (\( w, h ) -> ( w / 75, h / 75 ))
                ( toFloat mapWidth / 2, toFloat mapHeight / 2 )
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
    | ClickConnect
    | ConnectionOpened ( Int, Int )
    | LoadResources Resources.Msg
    | PressKeys Keyboard.Msg
    | NoOp
    | ResizeWindow Int Int
    | Tick Float


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ChangeServerUrl url ->
            ( { state
                | serverUrl = url
              }
            , Cmd.none
            )

        ClickConnect ->
            ( { state
                | loadable = Loading
              }
            , Port.connect state.serverUrl
            )

        ConnectionOpened mapSize ->
            let
                ( gs, cmd ) =
                    initGameState mapSize
            in
            ( { state
                | loadable = Loaded gs
              }
            , cmd
            )

        LoadResources rMsg ->
            updateGameState (updateResources rMsg) state

        NoOp ->
            ( state, Cmd.none )

        PressKeys kMsg ->
            updateGameState (updateKeys kMsg) state

        ResizeWindow x y ->
            ( { state
                | windowSize = ( x, y )
              }
            , Cmd.none
            )

        Tick dt ->
            updateGameState (updateOnTick dt state.windowSize) state


updateGameState : (GameState -> ( GameState, Cmd Msg )) -> State -> ( State, Cmd Msg )
updateGameState f state =
    case state.loadable of
        Loaded gs ->
            let
                ( newGS, cmd ) =
                    f gs
            in
            ( { state | loadable = Loaded newGS }
            , cmd
            )

        _ ->
            ( state, Cmd.none )


updateResources : Resources.Msg -> GameState -> ( GameState, Cmd Msg )
updateResources rMsg gs =
    ( { gs
        | resources = Resources.update rMsg gs.resources
      }
    , Cmd.none
    )


updateKeys : Keyboard.Msg -> GameState -> ( GameState, Cmd Msg )
updateKeys kMsg gs =
    ( { gs
        | keys = Keyboard.update kMsg gs.keys
      }
    , Cmd.none
    )


updateOnTick : Float -> ( Int, Int ) -> GameState -> ( GameState, Cmd Msg )
updateOnTick dt viewportSize gs =
    let
        arrows =
            Keyboard.Arrows.arrows gs.keys

        v =
            -- units per second
            8

        cameraMove : Maybe ( Float, Float, Float )
        cameraMove =
            if arrows.x == 0 && arrows.y == 0 then
                Nothing

            else
                Just
                    ( toFloat arrows.x * v * dt / 1000
                    , toFloat arrows.y * v * dt / 1000
                    , 0
                    )
    in
    case cameraMove of
        Just ( dx, dy, _ ) ->
            gs
                |> updateTime dt
                |> updateCamera (Camera.moveBy ( dx, dy ))
                |> pruneRenderables viewportSize
                |> (\g -> ( g, Cmd.none ))

        Nothing ->
            ( gs |> updateTime dt, Cmd.none )


updateTime : Float -> GameState -> GameState
updateTime dt gs =
    { gs
        | time = gs.time + dt
    }


updateCamera : (Camera -> Camera) -> GameState -> GameState
updateCamera f gs =
    { gs
        | camera = f gs.camera
    }


pruneRenderables : ( Int, Int ) -> GameState -> GameState
pruneRenderables viewportSize gs =
    let
        center =
            gs.camera |> Camera.getPosition

        size =
            gs.camera
                |> Camera.getViewSize (viewportSize |> Tuple.mapBoth toFloat toFloat)

        ( regionMinCorner, regionMaxCorner ) =
            regionOfInterest center size
    in
    { gs
      -- TODO: Prune cells.
        | cells = gs.cells |> List.filter (isCellInRegion regionMinCorner regionMaxCorner)
    }


{-| Get the region (in game coordinates) for which the client should track cells
and agents
-}
regionOfInterest : ( Float, Float ) -> ( Float, Float ) -> ( ( Float, Float ), ( Float, Float ) )
regionOfInterest ( camCenterX, camCenterY ) ( camSizeX, camSizeY ) =
    let
        buffer =
            2
    in
    ( ( camCenterX - camSizeX / 2 - buffer
      , camCenterY - camSizeY / 2 - buffer
      )
    , ( camCenterX + camSizeX / 2 + buffer
      , camCenterY + camSizeY / 2 + buffer
      )
    )


isCellInRegion : ( Float, Float ) -> ( Float, Float ) -> Cell -> Bool
isCellInRegion ( minX, minY ) ( maxX, maxY ) cell =
    let
        ( posX, posY ) =
            cell.pos |> Tuple.mapBoth toFloat toFloat
    in
    (posX <= maxX)
        && (posY <= maxY)
        && -- Add cell width, because if edge of cell is touching region it should be included.
           (posX + 1 >= minX)
        && (posY + 1 >= minY)



-- VIEW


view : State -> Html Msg
view state =
    div [ class "page" ]
        [ topToolbarView state.serverUrl
        , paneView state.windowSize state.loadable
        , bottomToolbarView
        ]


topToolbarView : String -> Html Msg
topToolbarView serverUrl =
    div [ class "top-toolbar" ]
        [ input [ class "server-input", placeholder "Server URL", value serverUrl, onInput ChangeServerUrl ] []
        , a [ class "connect-button", onClick ClickConnect ] [ text "Connect" ]
        ]


bottomToolbarView : Html Msg
bottomToolbarView =
    div [ class "bottom-toolbar" ] []


paneView : ( Int, Int ) -> Loadable GameState -> Html Msg
paneView windowSize loadable =
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
                    gameView windowSize gs
    in
    div [ class "pane" ] [ content ]


statusMessageView : String -> Html Msg
statusMessageView msg =
    div [ class "status-message" ] [ text msg ]


gameView : ( Int, Int ) -> GameState -> Html Msg
gameView windowSize gs =
    Game.render
        { time = gs.time / 1000
        , size = windowSize
        , camera = gs.camera
        }
        (renderCells gs.resources gs.cells)



-- render : Resources -> List Cell -> List Agent -> List Renderable
-- render resources _ _ =
--     [ Render.spriteWithOptions
--         { position = ( 0, 0, 0 )
--         , size = ( 10, 5 )
--         , texture = Resources.getTexture "resources/grass4.png" resources
--         , rotation = 0
--         , pivot = ( 0, 0 )
--         , tiling = ( 10, 5 )
--         }
--     ]


renderCells : Resources -> List Cell -> List Renderable
renderCells resources =
    List.map (cellSprite resources)


cellSprite : Resources -> Cell -> Renderable
cellSprite resources cell =
    let
        texture =
            case cell.grass of
                0 ->
                    "resources/grass0.png"

                1 ->
                    "resources/grass1.png"

                2 ->
                    "resources/grass2.png"

                3 ->
                    "resources/grass3.png"

                _ ->
                    "resources/grass4.png"
    in
    Render.sprite
        { texture = Resources.getTexture texture resources
        , position =
            ( Tuple.first cell.pos |> toFloat
            , Tuple.second cell.pos * 1 |> toFloat
            )
        , size = ( 1, 1 )
        }



-- TEST DATA


testCells : ( Int, Int ) -> List Cell
testCells mapSize =
    posRange ( 0, 0 ) mapSize
        |> List.map
            (\p ->
                { pos = p
                , grass = modBy 5 (Tuple.first p + Tuple.second p)
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
