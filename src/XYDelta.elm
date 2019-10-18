module XYDelta exposing (..)

import Json.Decode as JD
import XY exposing (XY)


type XYDelta
    = MouseMoveDelta XY XY


pageXYDecoder : JD.Decoder XY
pageXYDecoder =
    JD.map2 XY
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


fromPageXYDecoder : JD.Decoder XYDelta
fromPageXYDecoder =
    JD.map (\xy -> MouseMoveDelta xy xy) pageXYDecoder


moveToPageXYDecoder : XYDelta -> JD.Decoder XYDelta
moveToPageXYDecoder (MouseMoveDelta start _) =
    JD.map (\xy -> MouseMoveDelta start xy) pageXYDecoder
