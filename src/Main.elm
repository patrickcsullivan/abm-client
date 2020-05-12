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
import Network.FromClient exposing (FromClient(..))
import Network.Port
import Network.ToClient exposing (ToClient)


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
    { pos : ( Float, Float )
    , heading : Float -- heading in radians
    }


type alias Cell =
    { pos : ( Int, Int )
    , growthAmt : Int
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
    , Network.Port.sendToServer (RegisterInterest interest)
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
            updateGameState (updateCells sMsg state.viewportSize) state


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


updateCells : ToClient -> ( Int, Int ) -> GameState -> ( GameState, Cmd Msg )
updateCells sMsg viewportSize gs =
    let
        interest =
            regionOfInterest viewportSize gs.camera

        updatedCells =
            sMsg.cellUpdates
                |> List.map
                    (\up ->
                        { pos = ( up.x, up.y )
                        , growthAmt = up.growthAmt
                        }
                    )
                -- Filter cells in case server sent back uniteresting ones.
                |> filterCellsInRegion interest
    in
    ( { gs
        | cells = updatedCells
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
            , Network.Port.sendToServer (RegisterInterest interest)
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
        (renderCells gs.cells)


renderCells : List Cell -> List Renderable
renderCells =
    List.map cellShape


cellShape : Cell -> Renderable
cellShape cell =
    Render.shape
        Render.rectangle
        { color = Color.green
        , position =
            ( Tuple.first cell.pos |> toFloat
            , Tuple.second cell.pos * 1 |> toFloat
            )
        , size = ( 1, 1 )
        }
