module DrawerItem exposing (DrawerItem, init, withSA)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import StyleAttrs as SA exposing (StyleAttrs)
import Styles exposing (..)


type alias DrawerItem msg =
    { tag : String, sa : StyleAttrs msg, children : List (Html msg) }


type alias HtmlItem msg =
    { tag : String, sa : StyleAttrs msg, children : List (Html msg) }


init : DrawerItem msg
init =
    todo


withSA : StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withSA =
    todo


withPrimaryAction : HtmlItem msg -> DrawerItem msg -> DrawerItem msg
withPrimaryAction =
    todo


withContent : HtmlItem msg -> DrawerItem msg -> DrawerItem msg
withContent =
    todo


withSecondaryAction : HtmlItem msg -> DrawerItem msg -> DrawerItem msg
withSecondaryAction =
    todo


todo =
    Debug.todo "implement"
