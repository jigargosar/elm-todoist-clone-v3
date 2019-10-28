module Drawer exposing
    ( Panel(..)
    , PanelItemConfig
    , PanelItemId(..)
    , PanelMsg(..)
    , filterPanelItemConfig
    , labelPanelItemConfig
    , prefixNavItemsView
    , projectPanelItemConfig
    , viewPanelItemGhost
    , viewSimpleNavItem
    )

import Drag exposing (Drag)
import DrawerItem as DI
import Filter exposing (Filter)
import FilterId exposing (FilterId)
import Html.Styled exposing (..)
import Json.Decode as JD
import Label exposing (Label)
import LabelId exposing (LabelId)
import Project exposing (Project)
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
    | DragMsg Drag.Msg
    | DragComplete Drag.Info
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


moreClickedDecoder : (id -> PanelItemId) -> String -> id -> JD.Decoder PanelMsg
moreClickedDecoder panelItemId anchorId id =
    let
        kind =
            panelItemId id

        msg =
            More anchorId kind
    in
    JD.succeed msg


projectPanelItemConfig : PanelItemConfig ProjectId Project
projectPanelItemConfig =
    { moreClicked = moreClickedDecoder ProjectItemId
    , dragMsg = DragMsg
    , panelId = "project"
    , iconName = "folder"
    , id = Project.id
    , idToString = ProjectId.toString
    , title = Project.title
    , route = Project.id >> Route.Project
    , iconStyle = Styles.c_ << Project.cssColor
    }


labelPanelItemConfig : PanelItemConfig LabelId Label
labelPanelItemConfig =
    { moreClicked = moreClickedDecoder LabelItemId
    , dragMsg = DragMsg
    , panelId = "label"
    , id = Label.id
    , idToString = LabelId.toString
    , title = Label.title
    , route = Label.id >> Route.Label
    , iconName = "label"
    , iconStyle = Styles.c_ << Label.cssColor
    }


filterPanelItemConfig : PanelItemConfig FilterId Filter
filterPanelItemConfig =
    { moreClicked = moreClickedDecoder FilterItemId
    , dragMsg = DragMsg
    , panelId = "filter"
    , id = Filter.id
    , idToString = FilterId.toString
    , title = Filter.title
    , route = Filter.id >> Route.Filter
    , iconName = "filter_list"
    , iconStyle = Styles.c_ << Filter.cssColor
    }


viewPanelItemGhost : PanelItemConfig id item -> List item -> Drag -> List (Html msg)
viewPanelItemGhost config items drag =
    Drag.ghostItemWithStyles items drag
        |> Maybe.map
            (\( ghostStyles, item ) ->
                let
                    icon =
                        { name = config.iconName, sa = SA.styles [ config.iconStyle item ] }

                    rootSA =
                        SA.styles ghostStyles

                    title =
                        config.title item
                in
                [ viewPanelItemGhostHelp rootSA icon title
                , node "style" [] [ text "body *{ cursor:move!important; }" ]
                ]
            )
        |> Maybe.withDefault []


viewSimpleNavItem : Attribute msg -> String -> String -> Html msg
viewSimpleNavItem href title iconName =
    viewSimpleNavItemHelp (StyleAttrs [] [ href ]) { name = iconName, sa = SA.none } title


viewSimpleNavItemHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewSimpleNavItemHelp rootSA icon title =
    DI.initLink rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render


viewPanelItemGhostHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewPanelItemGhostHelp rootSA icon title =
    DI.init rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render
