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
import Server.FromClient exposing (FromClient(..))
import Server.Port
import Server.ToClient exposing (ToClient)


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
                    , Server.Port.onConnectionOpened ConnectionOpen
                    , Server.Port.onConnectionError ConnectionError
                    , Server.Port.onReceiveFromServer ReceiveMsgFromServer NoOp
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
    { pos : ( Float, Float )
    }


type alias Cell =
    { pos : ( Int, Int )
    , grass : Int
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
                (\( w, h ) -> ( w / 75, h / 75 ))
                ( 0, 0 )

        interest =
            regionOfInterest viewportSize camera
    in
    ( { time = 0
      , keys = []
      , resources = Resources.init
      , cells = []
      , agents = []
      , camera = camera
      }
    , Cmd.batch
        [ Cmd.map LoadResources
            -- TODO: Would be better to load this in init and save them in a PartiallyLoaded state.
            (Resources.loadTextures
                [ "resources/grass0.png"
                , "resources/grass1.png"
                , "resources/grass2.png"
                , "resources/grass3.png"
                , "resources/grass4.png"
                ]
            )
        , Server.Port.sendToServer (RegisterInterest interest)
        ]
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
    | ReceiveMsgFromServer ToClient


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
            , Server.Port.connect state.serverUrl
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
            updateGameState (applyServerUpdates sMsg state.viewportSize) state


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


applyServerUpdates : ToClient -> ( Int, Int ) -> GameState -> ( GameState, Cmd Msg )
applyServerUpdates sMsg viewportSize gs =
    let
        interest =
            regionOfInterest viewportSize gs.camera

        updatedCells =
            sMsg.cellUpdates
                |> List.map
                    (\up ->
                        { pos = ( up.x, up.y )
                        , grass = up.grass
                        }
                    )
                -- Filter cells in case server sent back uniteresting ones.
                |> filterCellsInRegion interest

        updatedAgents =
            List.concat [ mockAgents 56 63 0 7, mockAgents 0 7 56 63 ]
                |> filterAgentsInRegion interest
    in
    ( { gs
        | cells = updatedCells
        , agents = updatedAgents
      }
    , Cmd.none
    )


updateOnTick : Float -> ( Int, Int ) -> GameState -> ( GameState, Cmd Msg )
updateOnTick dt viewportSize gs =
    let
        _ =
            Debug.log "FPS" (1000.0 / dt)

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
                    filterCellsInRegion interest gs.cells
            in
            ( { gs
                | time = updatedTime
                , camera = updatedCamera
                , cells = updatedCells
              }
            , Server.Port.sendToServer (RegisterInterest interest)
            )

        Nothing ->
            ( { gs
                | time = gs.time + dt
              }
            , Cmd.none
            )


{-| Keep only cells that are partially or entirely within the given region in
game coordinate.
-}
filterCellsInRegion : BoundingBox -> List Cell -> List Cell
filterCellsInRegion region =
    List.filter (isCellIn region)


{-| Keep only agents that are partially or entirely within the given region in
game coordinate.
-}
filterAgentsInRegion : BoundingBox -> List Agent -> List Agent
filterAgentsInRegion region =
    List.filter (isAgentIn region)


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
        && -- Add cell width because if edge of cell is touching region it should be included.
           (posX + 1 >= box.xMin)
        && (posY + 1 >= box.yMin)


{-| Checks if agent is within the region defined by the bounding box.
-}
isAgentIn : BoundingBox -> Agent -> Bool
isAgentIn box agent =
    let
        ( posX, posY ) =
            agent.pos
    in
    -- Subract agent width because if min edge of agent is within max bounds it should be included
    (posX - 0.5 <= box.xMax)
        && (posY - 0.5 <= box.yMax)
        -- Add agent width because if max edge of agent is within min bounds it should be included.
        && (posX + 0.5 >= box.xMin)
        && (posY + 0.5 >= box.yMin)



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
        (List.concat
            [ renderableAgents gs.agents
            , renderableCells gs.resources gs.cells
            ]
        )


renderableCells : Resources -> List Cell -> List Renderable
renderableCells resources =
    List.map (cellSprite resources)


renderableAgents : List Agent -> List Renderable
renderableAgents =
    List.map agentCircle


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
    Render.spriteZ
        { texture = Resources.getTexture texture resources
        , position =
            ( Tuple.first cell.pos |> toFloat
            , Tuple.second cell.pos |> toFloat
            , 0
            )
        , size = ( 1, 1 )
        }


agentCircle : Agent -> Renderable
agentCircle agent =
    let
        ( width, height ) =
            ( 0.8, 0.8 )

        ( x, y ) =
            agent.pos
    in
    Render.shapeZ
        Render.circle
        { color = Color.hsl 0.061 0.61 0.28
        , position =
            ( x - width / 2.0
            , y - height / 2.0
            , 0.1
            )
        , size = ( width, height )
        }


mockAgents : Int -> Int -> Int -> Int -> List Agent
mockAgents xMin xMax yMin yMax =
    range2d xMin xMax yMin yMax
        |> List.map (\( x, y ) -> { pos = ( toFloat x + 0.5, toFloat y + 0.5 ) })


range2d : Int -> Int -> Int -> Int -> List ( Int, Int )
range2d xMin xMax yMin yMax =
    List.range yMin yMax
        |> List.concatMap (\y -> List.range xMin xMax |> List.map (\x -> ( x, y )))
