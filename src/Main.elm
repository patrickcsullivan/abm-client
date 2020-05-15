module Main exposing (Msg(..), main, update, view)

import BoundingBox exposing (BoundingBox)
import Browser
import Browser.Events exposing (onResize)
import Camera exposing (Camera)
import Color exposing (Color)
import Html exposing (Html, a, div, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Keyboard
import Keyboard.Arrows
import Network.Message.Incoming exposing (Incoming)
import Network.Message.Outgoing exposing (Outgoing(..))
import Network.Port
import Scale
import Scale.Color
import TypedSvg exposing (circle, svg)
import TypedSvg.Attributes exposing (fill, viewBox)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..))


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
            Camera.at ( 40.0, 40.0 )

        interest =
            regionOfInterest viewportSize camera
    in
    ( { time = 0
      , keys = []
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
            32

        cameraMove : Maybe ( Float, Float )
        cameraMove =
            if arrows.x == 0 && arrows.y == 0 then
                Nothing

            else
                Just
                    ( toFloat arrows.x * v * dt / 1000
                    , -1 * toFloat arrows.y * v * dt / 1000
                    )
    in
    case cameraMove of
        Just ( dx, dy ) ->
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
regionOfInterest ( vw, vh ) camera =
    let
        defaultGameCoordToPixel =
            0.1

        svgW =
            toFloat vw * defaultGameCoordToPixel / camera.zoomFactor

        svgH =
            toFloat vh * defaultGameCoordToPixel / camera.zoomFactor

        svgX =
            Tuple.first camera.center - svgW / 2.0

        svgY =
            Tuple.second camera.center - svgH / 2.0

        -- Extra space around the camera that is also of interest
        extra =
            2
    in
    { xMin = svgX - extra
    , xMax = svgX + svgW + extra
    , yMin = svgY - extra
    , yMax = svgY + svgH + extra
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
    buildSvg viewportSize
        gs.camera
        ([ svgBoard ]
            ++ List.map svgAgent gs.agents
        )


buildSvg : ( Int, Int ) -> Camera -> List (Svg Msg) -> Html Msg
buildSvg ( vw, vh ) camera svgs =
    -- TODO: Clean this up.
    -- TODO: Factor out shared code with regionOfInterest.
    let
        defaultGameCoordToPixel =
            0.1

        ( centerX, centerY ) =
            camera.center

        svgW =
            toFloat vw * defaultGameCoordToPixel / camera.zoomFactor

        svgH =
            toFloat vh * defaultGameCoordToPixel / camera.zoomFactor

        svgMinX =
            centerX - svgW / 2.0

        svgMinY =
            centerY - svgH / 2.0
    in
    svg [ viewBox svgMinX svgMinY svgW svgH ] svgs


svgAgent : Agent -> Svg Msg
svgAgent agent =
    let
        ( x, y ) =
            agent.position

        color =
            Scale.convert (Scale.sequential Scale.Color.viridisInterpolator ( 0, 360 )) 135.0
    in
    circle [ InPx.cx x, InPx.cy y, InPx.r 0.5, fill <| Paint color ] []


svgBoard : Svg Msg
svgBoard =
    let
        color =
            Scale.convert (Scale.sequential Scale.Color.viridisInterpolator ( 0, 360 )) 270.0
    in
    TypedSvg.rect
        [ InPx.x 0.0
        , InPx.y 0.0
        , InPx.width 80.0
        , InPx.height 80.0
        , fill <| Paint color
        ]
        []


testAgents : List Agent
testAgents =
    [ { heading = -0.9453345, position = ( 0.0, 0.0 ) }
    , { heading = -0.9453345, position = ( 80.0, 80.0 ) }
    , { heading = -0.9453345, position = ( 80.0, 0.0 ) }
    , { heading = -0.9453345, position = ( 0.0, 80.0 ) }
    ]
