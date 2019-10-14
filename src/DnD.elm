module DnD exposing (..)

import Browser.Dom as Dom


type DnD
    = DnD Internal


type alias Internal =
    Maybe State


type alias Position =
    { x : Float, y : Float }


type alias State =
    { dragIndex : Int
    , dropIndex : Int
    , dragElement : Maybe Dom.Element
    , dropElement : Maybe Dom.Element
    , dragElementId : String
    , dropElementId : String
    }


initial =
    DnD Nothing
