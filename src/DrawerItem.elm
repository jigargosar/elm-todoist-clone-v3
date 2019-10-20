module DrawerItem exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import StyleAttrs as SA
import Styles exposing (..)


view sa children =
    div (SA.toAttrsWithBase baseRootStyles [] sa)
        (children ++ [ rightScrollMarginFixEl ])


icon name sa =
    i (SA.toAttrsWithBase [] [ class "material-icons" ] sa) [ text name ]


rightScrollMarginFixEl =
    div [ css [ mr 3 ] ] []


baseRootStyles =
    [ ph 1, flex ]
