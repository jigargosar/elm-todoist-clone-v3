module DrawerUI exposing (..)

import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (class, css)
import Style
import Styles exposing (..)
import UI.Icon as Icon


item : List Style -> List (Attribute msg) -> List (Html msg) -> Html msg
item styles attrs =
    div (css [ Style.drawerItem, batch styles ] :: class "hover_parent" :: attrs)


dragHandle styles attrs icon =
    Icon.view2 icon ([ css [ Style.listItemIcon, cursorMove, batch styles ] ] ++ attrs)
