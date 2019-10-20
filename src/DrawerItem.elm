module DrawerItem exposing (..)

import Html.Styled exposing (div)
import StyleAttrs as SA
import Styles exposing (..)


view rootSA =
    div (SA.toAttrsWithBase baseRootStyle [] rootSA) []


baseRootStyle =
    [ ph 1, flex ]
