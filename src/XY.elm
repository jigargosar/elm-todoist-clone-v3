module XY exposing (..)

import Json.Decode as JD


type alias XY =
    { x : Float, y : Float }


type alias HasXY a =
    { a | x : Float, y : Float }


subtract : HasXY a -> HasXY b -> XY
subtract a b =
    XY (a.x - b.x) (a.y - b.y)


add : HasXY a -> HasXY b -> XY
add a b =
    XY (a.x + b.x) (a.y + b.y)


pageXYDecoder : JD.Decoder XY
pageXYDecoder =
    JD.map2 XY
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)
