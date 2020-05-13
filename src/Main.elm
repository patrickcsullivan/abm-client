module Main exposing (Msg(..), main, update, view)

import BoundingBox exposing (BoundingBox)
import Browser
import Browser.Events exposing (onResize)
import Color
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Html exposing (Html, a, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Keyboard
import Keyboard.Arrows
import Network.Message.Incoming exposing (Incoming)
import Network.Message.Outgoing exposing (Outgoing(..))
import Network.Port


main : Program ( Int, Int ) State Msg
main =
    Browser.element
        { init = \viewportSize -> init viewportSize
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onResize ResizeWindow
                    , Browser.Events.onAnimationFrameDelta Tick
                    , Sub.map PressKeys Keyboard.subscriptions
                    , Network.Port.onConnectionOpened ConnectionOpen
                    , Network.Port.onConnectionError ConnectionError
                    , Network.Port.onReceiveFromServer ReceiveMsgFromServer NoOp
                    ]
        }



-- MODEL


type alias State =
    { viewportSize : ( Int, Int )
    , serverUrl : String
    , loadable : Loadable GameState
    }


type Loadable a
    = NotStarted
    | Loading
    | Error (Maybe String)
    | Loaded a


type alias GameState =
    { time : Float -- time in ms
    , keys : List Keyboard.Key -- keys currently pressed
    , resources : Resources
    , cells : List Cell
    , agents : List Agent
    , camera : Camera
    }


type alias Agent =
    { position : ( Float, Float )
    , heading : Float -- heading in radians
    }


type alias Cell =
    { pos : ( Int, Int )
    }


init : ( Int, Int ) -> ( State, Cmd Msg )
init viewportSize =
    ( { serverUrl = "ws://127.0.0.1:54321"
      , viewportSize = viewportSize
      , loadable = NotStarted
      }
    , Cmd.none
    )


initGameState : ( Int, Int ) -> ( GameState, Cmd Msg )
initGameState viewportSize =
    let
        camera =
            Camera.custom
                (\( w, h ) -> ( w / 15, h / 15 ))
                ( 0, 0 )

        interest =
            regionOfInterest viewportSize camera
    in
    ( { time = 0
      , keys = []
      , resources = Resources.init
      , cells = cellsInRegion interest
      , agents = []
      , camera = camera
      }
      -- Send a single message to the server (for now) so it knows we're listening.
    , Network.Port.sendToServer Outgoing
    )



-- UPDATE / MESSAGE


type Msg
    = ChangeServerUrl String
    | ClickConnect
    | ConnectionError (Maybe String)
    | ConnectionOpen
    | LoadResources Resources.Msg
    | PressKeys Keyboard.Msg
    | NoOp
    | ResizeWindow Int Int
    | Tick Float
    | ReceiveMsgFromServer Incoming


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
            , Network.Port.connect state.serverUrl
            )

        ConnectionError maybeMsg ->
            ( { state
                | loadable = Error maybeMsg
              }
            , Cmd.none
            )

        ConnectionOpen ->
            let
                ( gs, cmd ) =
                    initGameState state.viewportSize
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
                | viewportSize = ( x, y )
              }
            , Cmd.none
            )

        Tick dt ->
            updateGameState (updateOnTick dt state.viewportSize) state

        ReceiveMsgFromServer sMsg ->
            updateGameState (updateAgents sMsg) state


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


updateAgents : Incoming -> GameState -> ( GameState, Cmd Msg )
updateAgents msg gs =
    let
        updatedAgents =
            msg.agentStates
                |> List.map
                    (\state -> { position = state.position, heading = state.heading })
    in
    ( { gs
        | agents = updatedAgents
      }
    , Cmd.none
    )



-- updateCells : ToClient -> ( Int, Int ) -> GameState -> ( GameState, Cmd Msg )
-- updateCells sMsg viewportSize gs =
--     let
--         interest =
--             regionOfInterest viewportSize gs.camera
--         updatedCells =
--             sMsg.cellUpdates
--                 |> List.map
--                     (\up ->
--                         { pos = ( up.x, up.y )
--                         , growthAmt = up.growthAmt
--                         }
--                     )
--                 -- Filter cells in case server sent back uniteresting ones.
--                 |> filterCellsInRegion interest
--     in
--     ( { gs
--         | cells = updatedCells
--       }
--     , Cmd.none
--     )


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
            let
                updatedTime =
                    gs.time + dt

                updatedCamera =
                    Camera.moveBy ( dx, dy ) gs.camera

                interest =
                    regionOfInterest viewportSize updatedCamera

                updatedCells =
                    cellsInRegion interest
            in
            ( { gs
                | time = updatedTime
                , camera = updatedCamera
                , cells = updatedCells
              }
            , Cmd.none
              -- , Network.Port.sendToServer (RegisterInterest interest)
            )

        Nothing ->
            ( { gs
                | time = gs.time + dt
              }
            , Cmd.none
            )


{-| Returns cells that are partially or entirely within the given region in
game coordinates.
-}
cellsInRegion : BoundingBox -> List Cell
cellsInRegion region =
    List.range 0 (16 * 16 - 1)
        |> List.map (\i -> ( modBy 16 i, i // 16 ))
        |> List.map (\( x, y ) -> { pos = ( x, y ) })
        |> List.filter
            (isCellIn region)



-- {-| Keep only cells that are partially or entirely within the given region in
-- game coordinate.
-- -}
-- filterCellsInRegion : BoundingBox -> List Cell -> List Cell
-- filterCellsInRegion region =
--     List.filter (isCellIn region)


{-| Gets the region (in game coordinates) for which the client should track
cells and agents.
-}
regionOfInterest : ( Int, Int ) -> Camera -> BoundingBox
regionOfInterest viewportSize camera =
    let
        ( camCenterX, camCenterY ) =
            camera |> Camera.getPosition

        ( camSizeX, camSizeY ) =
            camera
                |> Camera.getViewSize (viewportSize |> Tuple.mapBoth toFloat toFloat)

        -- Extra space around the camera that is also of interest
        extra =
            2
    in
    { xMin = camCenterX - camSizeX / 2 - extra
    , xMax = camCenterX + camSizeX / 2 + extra
    , yMin = camCenterY - camSizeY / 2 - extra
    , yMax = camCenterY + camSizeY / 2 + extra
    }


{-| Checks if cell is within the region defined by the bounding box.
-}
isCellIn : BoundingBox -> Cell -> Bool
isCellIn box cell =
    let
        ( posX, posY ) =
            cell.pos |> Tuple.mapBoth toFloat toFloat
    in
    (posX <= box.xMax)
        && (posY <= box.yMax)
        && -- Add cell width, because if edge of cell is touching region it should be included.
           (posX + 1 >= box.xMin)
        && (posY + 1 >= box.yMin)



-- VIEW


view : State -> Html Msg
view state =
    div [ class "page" ]
        [ topToolbarView state.serverUrl
        , paneView state.viewportSize state.loadable
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
paneView viewportSize loadable =
    let
        content =
            case loadable of
                NotStarted ->
                    statusMessageView "Connect to a server to start."

                Loading ->
                    statusMessageView "Connecting."

                Error maybeE ->
                    let
                        eMsg =
                            case maybeE of
                                Just e ->
                                    "Error occured: " ++ e

                                Nothing ->
                                    "Error occured"
                    in
                    statusMessageView eMsg

                Loaded gs ->
                    gameView viewportSize gs
    in
    div [ class "pane" ] [ content ]


statusMessageView : String -> Html Msg
statusMessageView msg =
    div [ class "status-message" ] [ text msg ]


gameView : ( Int, Int ) -> GameState -> Html Msg
gameView viewportSize gs =
    Game.render
        { time = gs.time / 1000
        , size = viewportSize
        , camera = gs.camera
        }
        (renderCells gs.cells
            ++ renderAgents gs.agents
        )


renderCells : List Cell -> List Renderable
renderCells =
    List.map cellShape


cellShape : Cell -> Renderable
cellShape cell =
    Render.shape
        Render.rectangle
        { color = Color.green
        , position =
            ( 5 * Tuple.first cell.pos |> toFloat
            , 5 * Tuple.second cell.pos |> toFloat
            )
        , size = ( 5, 5 )
        }


renderAgents : List Agent -> List Renderable
renderAgents agents =
    let
        _ =
            Debug.log "Agents" agents
    in
    List.concatMap agentShape agents


agentShape : Agent -> List Renderable
agentShape agent =
    let
        ( x, y ) =
            agent.position
    in
    [ Render.shapeWithOptions
        Render.triangle
        { color = Color.blue
        , position = ( x, y, 1 )
        , size = ( -0.5, 1.5 )
        , rotation = agent.heading - 1.57079632679
        , pivot = ( 0, 0.5 )
        }
    , Render.shapeWithOptions
        Render.triangle
        { color = Color.red
        , position = ( x, y, 1 )
        , size = ( 0.5, 1.5 )
        , rotation = agent.heading - 1.57079632679
        , pivot = ( 0, 0.5 )
        }
    ]


testAgents : List Agent
testAgents =
    [ { heading = -0.9453345, position = ( 64.698494, 77.30139 ) }
    , { heading = 2.332968, position = ( 16.833345, 79.98595 ) }
    , { heading = 2.3652072, position = ( 14.322161, 79.98436 ) }
    , { heading = 2.3636627, position = ( 26.76448, 79.99975 ) }
    , { heading = 2.039263, position = ( 27.603529, 79.992134 ) }
    , { heading = 1.8874136, position = ( 26.9267, 79.99786 ) }
    , { heading = 1.797899, position = ( 6.861332, 79.99554 ) }
    , { heading = 2.4860365, position = ( 17.305418, 79.98092 ) }
    , { heading = 1.784747, position = ( 21.7909, 79.98643 ) }
    , { heading = 1.993496, position = ( 26.641142, 79.988525 ) }
    , { heading = 2.2453573, position = ( 26.847433, 79.9828 ) }
    , { heading = 2.1097722, position = ( 31.061018, 79.98663 ) }
    , { heading = 2.5811248, position = ( 22.692007, 79.99733 ) }
    , { heading = 1.922796, position = ( 14.726786, 79.98764 ) }
    , { heading = 1.8642715, position = ( 31.827425, 79.98732 ) }
    , { heading = 1.7165364, position = ( 1.7492739, 79.991325 ) }
    , { heading = 2.5094285, position = ( 17.489233, 79.99363 ) }
    , { heading = 2.1079273, position = ( 21.15088, 79.995834 ) }
    , { heading = 1.8915762, position = ( 36.57551, 79.990265 ) }
    , { heading = 1.7060518, position = ( 21.752823, 79.99394 ) }
    , { heading = 2.213872, position = ( 29.459764, 79.9429 ) }
    , { heading = 2.100556, position = ( 31.031744, 79.99661 ) }
    , { heading = 1.629924, position = ( 31.959215, 79.99392 ) }
    , { heading = 1.6469615, position = ( 21.922392, 79.991875 ) }
    , { heading = 1.7818282, position = ( 31.735058, 77.07137 ) }
    ]
