module Drawer exposing
    ( Panel(..)
    , prefixNavItemsView
    , viewSimpleNavItem
    )

import DrawerItem as DI
import FilterId exposing (FilterId)
import Html.Styled exposing (..)
import Json.Decode as JD
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)
import Route
import StyleAttrs as SA exposing (StyleAttrs)
import Styles exposing (..)


prefixNavItemsView : List (Html msg)
prefixNavItemsView =
    [ viewSimpleNavItem (Route.href Route.Inbox) "Inbox" "inbox"
    , viewSimpleNavItem (Route.href Route.Inbox) "Today" "calendar_today"
    , viewSimpleNavItem (Route.href Route.Inbox) "Next 7 Days" "view_week"
    ]


type Panel
    = Projects
    | Labels
    | Filters


viewSimpleNavItem : Attribute msg -> String -> String -> Html msg
viewSimpleNavItem href title iconName =
    viewSimpleNavItemHelp (StyleAttrs [] [ href ]) { name = iconName, sa = SA.none } title


viewSimpleNavItemHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewSimpleNavItemHelp rootSA icon title =
    DI.initLink rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render
