module DrawerItem exposing (view)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import StyleAttrs as SA
import Styles exposing (..)


view sa children =
    div (SA.toAttrsWithBase baseRootStyles [] sa)
        (children ++ [ rightScrollMarginFixEl ])


rightScrollMarginFixEl =
    div [ css [ mr 3 ] ] []


baseRootStyles =
    [ ph 1, flex ]


icon name sa =
    i (SA.toAttrsWithBase [] [ class "material-icons" ] sa) [ text name ]
