module Main exposing (Msg(..), main, update, view)

import Agent exposing (Agent)
import BoundingBox exposing (BoundingBox)
import Browser
import Browser.Events exposing (onResize)
import Cell exposing (Cell)
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
import SimUpdate exposing (SimUpdate(..))


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
    | Error String
    | Loaded a


type alias GameState =
    { time : Float -- time in ms
    , keys : List Keyboard.Key -- keys currently pressed
    , resources : Resources
    , cells : List Cell
    , agents : List Agent
    , camera : Camera
    }


init : ( Int, Int ) -> ( State, Cmd Msg )
init viewportSize =
    ( { serverUrl = "127.0.0.1:8888"
      , viewportSize = viewportSize
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

        ReceiveMsgFromServer rMsg ->
            -- TODO: Update, insert, and prune cells.
            let
                _ =
                    rMsg |> Debug.log "ReceiveMsgFromServer"
            in
            ( state, Cmd.none )


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
    { minX = camCenterX - camSizeX / 2 - extra
    , maxX = camCenterX + camSizeX / 2 + extra
    , minY = camCenterY - camSizeY / 2 - extra
    , maxY = camCenterY + camSizeY / 2 + extra
    }


{-| Checks if cell is within the region defined by the bounding box.
-}
isCellIn : BoundingBox -> Cell -> Bool
isCellIn box cell =
    let
        ( posX, posY ) =
            cell.pos |> Tuple.mapBoth toFloat toFloat
    in
    (posX <= box.maxX)
        && (posY <= box.maxY)
        && -- Add cell width, because if edge of cell is touching region it should be included.
           (posX + 1 >= box.minX)
        && (posY + 1 >= box.minY)



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

                Error e ->
                    statusMessageView ("Error occured: " ++ e)

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
