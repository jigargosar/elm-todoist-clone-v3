module DrawerItem exposing (DrawerItem, init, withContent, withPrimaryAction, withSA, withSecondaryAction)

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


initLink : DrawerItem msg
initLink =
    todo


withSA : StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withSA =
    todo


withPrimaryAction : HtmlItem msg -> DrawerItem msg -> DrawerItem msg
withPrimaryAction =
    todo


withPrimaryIcon : String -> StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withPrimaryIcon =
    todo


withDraggablePrimaryIcon : String -> StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withDraggablePrimaryIcon =
    todo


withContentText : String -> DrawerItem msg -> DrawerItem msg
withContentText =
    todo


withContent : HtmlItem msg -> DrawerItem msg -> DrawerItem msg
withContent =
    todo


withSecondaryAction : HtmlItem msg -> DrawerItem msg -> DrawerItem msg
withSecondaryAction =
    todo


todo =
    Debug.todo "implement"
