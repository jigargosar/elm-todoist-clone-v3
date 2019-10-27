module DrawerUI exposing (expansionToggleBtn, listItemStyle, secondaryActionIconBtnStyle, viewExpansionPanelHeader)

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
    }
    -> List (Html msg)
viewExpansionPanelHeader { toggle, isExpanded, title, secondary } =
    [ div
        [ css [ listItemStyle ] ]
        [ expansionToggleBtn toggle "Projects" isExpanded
        , Maybe.map secondaryActionIconBtn secondary |> Maybe.withDefault (text "")
        ]
    ]


expansionToggleBtn : msg -> String -> Bool -> Html msg
expansionToggleBtn toggle title isExpanded =
    let
        iconName =
            if isExpanded then
                "expand_more"

            else
                "chevron_right"
    in
    button
        [ css [ btnReset, pointer, flexGrow1, flex, itemsCenter, tal ], onClick toggle ]
        [ i [ css [ Px.pa 4 ], class "material-icons" ] [ text iconName ]
        , span [ css [ Px.p2 8 4, bold, flexGrow1 ] ] [ text title ]
        ]


secondaryActionIconBtn : { a | iconName : String, action : msg } -> Html msg
secondaryActionIconBtn { iconName, action } =
    button
        [ css [ secondaryActionIconBtnStyle ]
        , onClick action
        ]
        [ secondaryActionIcon iconName ]


secondaryActionIconBtnStyle : Style
secondaryActionIconBtnStyle =
    batch [ btnReset, pointer, Px.pa 4, Px.m2 4 0, flex, itemsCenter, selfEnd ]


secondaryActionIcon : String -> Html msg
secondaryActionIcon name =
    i [ class "material-icons" ] [ text name ]


listItemStyle : Style
listItemStyle =
    batch [ Px.pl 4, Px.pr (4 + 16), flex, itemsCenter, bo_b, boc Theme.borderGray, hover [ bgGrayL 0.95 ] ]
