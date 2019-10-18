module MouseMoveDelta exposing (..)

import Json.Decode as JD
import XY exposing (XY)


type MouseMoveDelta
    = MouseMoveDelta XY XY


pageXYDecoder : JD.Decoder XY
pageXYDecoder =
    JD.map2 XY
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


fromPageXYDecoder : JD.Decoder MouseMoveDelta
fromPageXYDecoder =
    JD.map (\xy -> MouseMoveDelta xy xy) pageXYDecoder
