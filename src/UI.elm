module UI exposing (..)

import Html.Styled exposing (Attribute, Html, button, div, i, span, text)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Px
import Styles exposing (..)
import Theme


viewExpansionPanelHeader :
    { toggled : msg
    , title : String
    , collapsed : Bool
    , secondary : { iconName : String, action : msg }
    }
    -> Html msg
viewExpansionPanelHeader { toggled, collapsed, title, secondary } =
    div
        [ css
            [ Px.pl 4
            , Px.pr (4 + 16)
            , flex
            , itemsCenter
            , bo_b
            , boColor Theme.borderGray
            , hover [ bgGrayL 0.95 ]
            ]
        ]
        [ let
            iconName =
                if collapsed then
                    "chevron_right"

                else
                    "expand_more"
          in
          button
            [ css [ btnReset, pointer, flex, itemsCenter, flexGrow1, tal ], onClick toggled ]
            [ i [ css [ Px.pa 4 ], class "material-icons" ] [ text iconName ]
            , span [ css [ Px.p2 8 4, bold, flexGrow1 ] ] [ text title ]
            ]
        , button
            [ css [ btnReset, pointer, Px.pa 4, Px.m2 4 0, flex, itemsCenter, selfEnd ]
            , onClick secondary.action
            ]
            [ i [ class "material-icons" ] [ text secondary.iconName ] ]
        ]


viewExpansionPanel :
    { toggled : msg
    , title : String
    , collapsed : Bool
    , secondary : { iconName : String, action : msg }
    }
    -> (() -> List (Html msg))
    -> List (Html msg)
viewExpansionPanel config lazyContent =
    viewExpansionPanelHeader config
        :: (if config.collapsed then
                []

            else
                lazyContent ()
           )
