module DrawerItem exposing (..)

import Html.Styled exposing (div)
import Html.Styled.Attributes exposing (css)
import StyleAttrs as SA
import Styles exposing (..)


view rootSA children =
    div (SA.toAttrsWithBase baseRootStyles [] rootSA)
        (children ++ [ rightScrollMarginFixEl ])


rightScrollMarginFixEl =
    div [ css [ mr 3 ] ] []


baseRootStyles =
    [ ph 1, flex ]
