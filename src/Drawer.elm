module Drawer exposing
    ( prefixNavItemsView
    , viewSimpleNavItem
    )

import DrawerItem as DI
import Html.Styled exposing (..)
import Route
import StyleAttrs as SA exposing (StyleAttrs)


prefixNavItemsView : List (Html msg)
prefixNavItemsView =
    [ viewSimpleNavItem Route.inboxHref "Inbox" "inbox"
    , viewSimpleNavItem Route.inboxHref "Today" "calendar_today"
    , viewSimpleNavItem Route.inboxHref "Next 7 Days" "view_week"
    ]


viewSimpleNavItem : Attribute msg -> String -> String -> Html msg
viewSimpleNavItem href title iconName =
    viewSimpleNavItemHelp (StyleAttrs [] [ href ]) { name = iconName, sa = SA.none } title


viewSimpleNavItemHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewSimpleNavItemHelp rootSA icon title =
    DI.initLink rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render
