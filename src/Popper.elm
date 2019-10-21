module Popper exposing (..)

import Browser.Dom exposing (Element)
import XY exposing (XY)


type Popper
    = Popper (Maybe State)


type alias State =
    { startXY : XY
    , anchorId : String
    , anchorEl : Element
    , popupId : String
    , popupEl : Maybe Element
    }
