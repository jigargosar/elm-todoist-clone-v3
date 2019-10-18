module XYDelta exposing (..)

import Json.Decode as JD
import XY exposing (XY)


type XYDelta
    = MouseMoveDelta XY XY


moveTo : XY -> XYDelta -> XYDelta
moveTo xy (MouseMoveDelta start _) =
    MouseMoveDelta start xy


diff (MouseMoveDelta start current) =
    XY.subtract current start


init xy =
    MouseMoveDelta xy xy
