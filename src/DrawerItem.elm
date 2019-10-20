module DrawerItem exposing (icon, view)

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


icon : String -> SA.StyleAttrs msg -> Html msg
icon name sa =
    i
        (SA.toAttrsWithBase
            baseIconRootStyles
            [ class "material-icons" ]
            sa
        )
        [ text name ]


baseIconRootStyles =
    [ ph 2, pv 1 ]
