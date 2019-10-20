module DrawerItem exposing (..)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import StyleAttrs as SA exposing (StyleAttrs)
import Styles exposing (..)


type alias DrawerItem msg =
    { tag : String, sa : StyleAttrs msg, children : List (Html msg) }


withSA : StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withSA =
    todo


todo =
    Debug.todo "implement"


tagged : String -> DrawerItem msg -> DrawerItem msg
tagged =
    todo
