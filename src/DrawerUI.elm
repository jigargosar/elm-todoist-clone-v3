module DrawerUI exposing (viewExpansionPanelHeader)

import Html.Styled exposing (Attribute, Html, button, div, i, span, text)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Px
import Styles exposing (..)
import Theme


viewExpansionPanelHeader :
    { toggle : msg
    , title : String
    , isExpanded : Bool
    , secondary : Maybe { iconName : String, action : msg }
    , secondaryIcon : String
    , secondaryAction : msg
    }
    -> List (Html msg)
viewExpansionPanelHeader { toggle, isExpanded, title, secondary } =
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
        , Maybe.map secondaryActionIconBtn secondary |> Maybe.withDefault (text "")
        ]
    ]


secondaryActionIconBtn : { a | iconName : String, action : msg } -> Html msg
secondaryActionIconBtn { iconName, action } =
    button
        [ css [ btnReset, pointer, Px.pa 4, Px.m2 4 0, flex, itemsCenter, selfEnd ]
        , onClick action
        ]
        [ i [ class "material-icons" ] [ text iconName ] ]
