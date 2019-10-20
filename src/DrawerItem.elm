module DrawerItem exposing (contentAsTextLink, icon, view)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import StyleAttrs as SA exposing (StyleAttrs)
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


contentAsTextLink : StyleAttrs msg -> String -> Html msg
contentAsTextLink sa title =
    a
        (SA.toAttrsWithBase
            baseTextLinkStyles
            []
            sa
        )
        [ text title ]


baseTextLinkStyles =
    [ Css.textDecoration Css.none
    , Css.visited [ Css.color Css.inherit ]
    , Css.color Css.inherit
    , ph 2
    , pv 1
    , flex
    , flexGrow1
    , itemsCenter
    ]
