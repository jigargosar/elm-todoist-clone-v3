module DrawerUI exposing (..)

import Html.Styled exposing (Attribute, Html, button, div, i, span, text)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Px
import Styles exposing (..)
import Theme


viewExpansionPanelHeader : { toggle : msg, title : String, isExpanded : Bool } -> List (Html msg)
viewExpansionPanelHeader { toggle, isExpanded, title } =
    let
        iconName =
            if isExpanded then
                "expand_more"

            else
                "chevron_right"
    in
    [ div
        [ css [ Px.pl 4, Px.pr (4 + 16), flex, itemsCenter, bo_b, boc Theme.borderGray, hover [ bgGrayL 0.95 ] ] ]
        [ button
            [ css [ btnReset, pointer, flexGrow1, flex, itemsCenter, tal ], onClick toggle ]
            [ i [ css [ Px.pa 4 ], class "material-icons" ] [ text iconName ]
            , span [ css [ Px.p2 8 4, bold, flexGrow1 ] ] [ text "Projects" ]
            ]

        --        , secondaryActionAddIconBtn
        ]
    ]
