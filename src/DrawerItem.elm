module DrawerItem exposing (DrawerItem, init, initLink, render, withContentText, withLinkContent, withPrimaryIcon, withSecondaryMoreAction)

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
    i (SA.toAttrsWithBase [ pv 2, ph 1 ] [ class "material-icons" ] sa) [ text iconName ]


withContentText : String -> DrawerItem msg -> DrawerItem msg
withContentText title model =
    { model | content = Just <| contentText title }


contentText : String -> Html msg
contentText title =
    div [ css [ flexGrow1, pv 2, ph 1, flex, itemsCenter ] ] [ text title ]


withLinkContent : String -> StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withLinkContent =
    todo


withSecondaryMoreAction : StyleAttrs msg -> DrawerItem msg -> DrawerItem msg
withSecondaryMoreAction =
    todo


render : DrawerItem msg -> Html msg
render { tag, sa, primary, content, secondary } =
    node tag
        (SA.toAttrsWithBase [ ph 1, flex ] [] sa)
        (List.filterMap identity [ primary, content, secondary, Just drawerScrollFix ])


drawerScrollFix =
    div [ css [ mr 3 ] ] []


todo =
    Debug.todo "implement"
