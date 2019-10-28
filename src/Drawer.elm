module Drawer exposing
    ( Panel(..)
    , PanelItemConfig
    , PanelItemId(..)
    , PanelMsg(..)
    , prefixNavItemsView
    , viewSimpleNavItem
    )

import Drag exposing (Drag)
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


type PanelMsg
    = Add
    | More String PanelItemId


type PanelItemId
    = ProjectItemId ProjectId
    | LabelItemId LabelId
    | FilterItemId FilterId


type alias PanelItemConfig id item =
    { moreClicked : String -> id -> JD.Decoder PanelMsg
    , dragMsg : Drag.Msg -> PanelMsg
    , panelId : String
    , id : item -> id
    , idToString : id -> String
    , title : item -> String
    , route : item -> Route.Route
    , iconName : String
    , iconStyle : item -> Style
    }


viewSimpleNavItem : Attribute msg -> String -> String -> Html msg
viewSimpleNavItem href title iconName =
    viewSimpleNavItemHelp (StyleAttrs [] [ href ]) { name = iconName, sa = SA.none } title


viewSimpleNavItemHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewSimpleNavItemHelp rootSA icon title =
    DI.initLink rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render
