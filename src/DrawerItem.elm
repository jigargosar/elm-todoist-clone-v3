module DrawerItem exposing (DrawerItem, init, initLink, render, withContentText, withDraggablePrimaryIcon, withLinkContent, withPrimaryIcon, withSecondaryMoreAction)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import StyleAttrs as SA exposing (StyleAttrs)
import Styles exposing (..)


type alias DrawerItem msg =
    { tag : String
    , sa : StyleAttrs msg
    , primary : Maybe (Html msg)
    , content : Maybe (Html msg)
    , secondary : Maybe (Html msg)
    }


init : StyleAttrs msg -> DrawerItem msg
init sa =
    DrawerItem "div" sa Nothing Nothing Nothing


initLink : StyleAttrs msg -> DrawerItem msg
initLink =
    todo


withPrimaryIcon : String -> StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withPrimaryIcon iconName sa model =
    { model | primary = Just <| primaryIcon iconName sa }


primaryIcon : String -> StyleAttrs msg -> Html msg
primaryIcon iconName sa =
    i (SA.toAttrsWithBase [] [] sa) [ text iconName ]


withDraggablePrimaryIcon : String -> StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withDraggablePrimaryIcon =
    todo


withContentText : String -> DrawerItem msg -> DrawerItem msg
withContentText =
    todo


withLinkContent : String -> StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withLinkContent =
    todo


withSecondaryMoreAction : StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withSecondaryMoreAction =
    todo


render : DrawerItem msg -> Html msg
render =
    todo


todo =
    Debug.todo "implement"
