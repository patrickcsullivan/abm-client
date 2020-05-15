module Camera exposing (Camera, at, moveBy)


type alias Camera =
    { center : ( Float, Float )
    , zoomFactor : Float
    }


at : ( Float, Float ) -> Camera
at center =
    { center = center
    , zoomFactor = 1.0
    }


moveBy : ( Float, Float ) -> Camera -> Camera
moveBy ( dx, dy ) camera =
    let
        ( x, y ) =
            camera.center
    in
    { camera
        | center = ( x + dx, y + dy )
    }
