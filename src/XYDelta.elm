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


moveTo : XY -> XYDelta -> XYDelta
moveTo xy (MouseMoveDelta start _) =
    MouseMoveDelta start xy


diff (MouseMoveDelta start current) =
    XY.subtract current start


init xy =
    MouseMoveDelta xy xy
