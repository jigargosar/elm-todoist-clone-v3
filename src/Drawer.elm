module Drawer exposing
    ( Config
    , Panel(..)
    , PanelItemId(..)
    , PanelLists
    , PanelState
    , initialPanelState
    , panelSubscriptions
    , toggleExpansionPanel
    , updatePanelDrag
    , view
    )

import Css
import Drag exposing (Drag)
import DrawerItem as DI
import ExpansionPanelUI
import Filter exposing (Filter)
import FilterId exposing (FilterId)
import Html.Styled exposing (..)
import Html.Styled.Attributes as A
import Html.Styled.Events exposing (onClick)
import Label exposing (Label)
import LabelId exposing (LabelId)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Route
import StyleAttrs as SA exposing (StyleAttrs)
import Styles exposing (..)
import View exposing (View)


type Panel
    = Projects
    | Labels
    | Filters


panelTypes =
    [ Projects, Labels, Filters ]


type alias SubState a =
    { projects : a
    , labels : a
    , filters : a
    }


type alias PanelState =
    { expanded : SubState Bool, drag : SubState Drag }


mapExpanded func panelState =
    { panelState | expanded = func panelState.expanded }


mapDrag func panelState =
    { panelState | drag = func panelState.drag }


initialPanelState : PanelState
initialPanelState =
    PanelState (initSubState True) (initSubState Drag.initial)


initSubState : a -> SubState a
initSubState a =
    SubState a a a


getSubState : Panel -> SubState a -> a
getSubState panel subState =
    case panel of
        Projects ->
            subState.projects

        Labels ->
            subState.labels

        Filters ->
            subState.filters


setSubState : Panel -> a -> SubState a -> SubState a
setSubState panel a subState =
    case panel of
        Projects ->
            { subState | projects = a }

        Labels ->
            { subState | labels = a }

        Filters ->
            { subState | filters = a }


mapSubState : Panel -> (a -> a) -> SubState a -> SubState a
mapSubState panel func subState =
    let
        get =
            getSubState panel

        set a =
            setSubState panel a subState
    in
    get subState |> func |> set


toggleExpansionPanel : Panel -> PanelState -> PanelState
toggleExpansionPanel panel =
    mapExpanded (mapSubState panel not)


updatePanelDrag :
    (Panel -> Drag.Msg -> msg)
    -> (Panel -> Drag.Info -> msg)
    -> Panel
    -> Drag.Msg
    -> PanelState
    -> ( PanelState, Cmd msg )
updatePanelDrag toMsg onComplete panel msg panelState =
    let
        drag =
            getSubState panel panelState.drag
    in
    Drag.update (toMsg panel) (onComplete panel) msg drag
        |> Tuple.mapFirst (\newDrag -> panelState |> mapDrag (mapSubState panel (always newDrag)))


panelDragSubscriptions : (Panel -> Drag.Msg -> msg) -> SubState Drag -> Sub msg
panelDragSubscriptions toMsg dragSubState =
    let
        dragSubscription panel =
            Drag.subscriptions (toMsg panel) (getSubState panel dragSubState)
    in
    Sub.batch (List.map dragSubscription panelTypes)


panelSubscriptions : (Panel -> Drag.Msg -> msg) -> PanelState -> Sub msg
panelSubscriptions toMsg panelState =
    panelDragSubscriptions toMsg panelState.drag


type alias Config msg =
    { onToggleExpansionPanel : Panel -> msg
    , onPanelItemMoreMenuClicked : PanelItemId -> msg
    , panelDragConfig : { toMsg : Panel -> Drag.Msg -> msg, onComplete : Panel -> Drag.Info -> msg }
    }


type alias PanelLists =
    { projects : List Project
    , labels : List Label
    , filters : List Filter
    }


view :
    Config msg
    -> PanelLists
    -> PanelState
    -> View (Html msg)
view config panelLists panelState =
    let
        prefixCP =
            View.content
                [ viewSimpleNavItem (Route.href Route.Inbox) "Inbox" "inbox"
                , viewSimpleNavItem (Route.href Route.Inbox) "Today" "calendar_today"
                , viewSimpleNavItem (Route.href Route.Inbox) "Next 7 Days" "view_week"
                ]

        panelConfig : PanelNavItemViewConfig id item -> List item -> Panel -> PanelConfig id item msg
        panelConfig itemConfig items panel =
            { togglePanel = config.onToggleExpansionPanel panel
            , domIdPrefix = "panel-nav-item__"
            , onMoreClicked = config.onPanelItemMoreMenuClicked
            , isExpanded = .expanded >> getSubState panel
            , drag = .drag >> getSubState panel
            , dragSystem = Drag.system (config.panelDragConfig.toMsg panel) (config.panelDragConfig.onComplete panel)
            , items = items
            , itemConfig = itemConfig
            }

        projectsCP =
            viewPanel (panelConfig projectNavItemViewConfig panelLists.projects Projects)
                "Projects"
                panelState

        labelsCP =
            viewPanel (panelConfig labelNavItemViewConfig panelLists.labels Labels)
                "Labels"
                panelState

        filtersCP =
            viewPanel (panelConfig filterNavItemViewConfig panelLists.filters Filters)
                "Filters"
                panelState
    in
    View.concat [ prefixCP, projectsCP, labelsCP, filtersCP ]


type PanelItemId
    = ProjectItemId ProjectId
    | LabelItemId LabelId
    | FilterItemId FilterId


viewPanel :
    PanelConfig id item msg
    -> String
    -> PanelState
    -> View (Html msg)
viewPanel pc title panelState =
    let
        isExpanded : Bool
        isExpanded =
            pc.isExpanded panelState

        drag : Drag
        drag =
            pc.drag panelState
    in
    View.concat
        [ View.content
            [ ExpansionPanelUI.viewHeader pc.togglePanel title isExpanded ]
        , if isExpanded then
            View.fromTuple
                ( pc.items
                    |> pc.dragSystem.rotate drag
                    |> List.indexedMap (viewPanelNavItem pc pc.itemConfig pc.dragSystem drag)
                , viewPanelNavItemGhost pc.dragSystem pc.itemConfig drag pc.items
                )

          else
            View.none
        ]


viewPanelNavItemGhost : Drag.System item msg -> PanelNavItemViewConfig id item -> Drag -> List item -> List (Html msg)
viewPanelNavItemGhost dragSystem itemConfig drag items =
    dragSystem.ghostItemWithStyles items drag
        |> Maybe.map
            (\( ghostStyles, item ) ->
                let
                    icon =
                        { name = itemConfig.iconName, sa = SA.styles [ itemConfig.iconStyle item ] }

                    rootSA =
                        SA.styles ghostStyles

                    title =
                        itemConfig.title item
                in
                [ viewPanelNavItemGhostHelp rootSA icon title
                , node "style" [] [ text "body *{ cursor:move!important; }" ]
                ]
            )
        |> Maybe.withDefault []


type alias PanelConfig id item msg =
    { togglePanel : msg
    , domIdPrefix : String
    , onMoreClicked : PanelItemId -> msg
    , isExpanded : PanelState -> Bool
    , drag : PanelState -> Drag
    , dragSystem : Drag.System item msg
    , items : List item
    , itemConfig : PanelNavItemViewConfig id item
    }


type alias PanelNavItemViewConfig id item =
    { id : item -> id
    , idToString : id -> String
    , panelItemId : id -> PanelItemId
    , title : item -> String
    , route : item -> Route.Route
    , iconName : String
    , iconStyle : item -> Style
    }


projectNavItemViewConfig : PanelNavItemViewConfig ProjectId Project
projectNavItemViewConfig =
    { id = Project.id
    , idToString = ProjectId.toString
    , panelItemId = ProjectItemId
    , title = Project.title
    , route = Project.id >> Route.Project
    , iconName = "folder"
    , iconStyle = c_ << Project.cssColor
    }


labelNavItemViewConfig : PanelNavItemViewConfig LabelId Label
labelNavItemViewConfig =
    { id = Label.id
    , idToString = LabelId.toString
    , panelItemId = LabelItemId
    , title = Label.title
    , route = Label.id >> Route.Label
    , iconName = "label"
    , iconStyle = c_ << Label.cssColor
    }


filterNavItemViewConfig : PanelNavItemViewConfig FilterId Filter
filterNavItemViewConfig =
    { id = Filter.id
    , idToString = FilterId.toString
    , panelItemId = FilterItemId
    , title = Filter.title
    , route = Filter.id >> Route.Filter
    , iconName = "filter_list"
    , iconStyle = c_ << Filter.cssColor
    }


viewPanelNavItem :
    PanelConfig id item msg
    -> PanelNavItemViewConfig id item
    -> Drag.System item msg
    -> Drag
    -> Int
    -> item
    -> Html msg
viewPanelNavItem config itemConfig dragSystem drag idx item =
    let
        { domIdPrefix, onMoreClicked } =
            config

        id =
            itemConfig.id item

        domId =
            domIdPrefix ++ "_" ++ itemConfig.idToString id

        rootSA =
            let
                dragOverStyle =
                    Styles.styleIf (dragSystem.eqDragOverIdx idx drag) [ Css.opacity <| Css.zero ]
            in
            StyleAttrs
                [ hover [ bgGrayL 0.9 ]
                , noSelection
                , dragOverStyle
                ]
                (A.id domId :: dragSystem.dropEvents idx drag)

        primaryIcon : { name : String, sa : StyleAttrs msg }
        primaryIcon =
            let
                dragEvents =
                    dragSystem.dragEvents idx domId drag
            in
            { name = itemConfig.iconName
            , sa = StyleAttrs [ Css.cursor Css.move, itemConfig.iconStyle item ] dragEvents
            }

        link : { title : String, sa : StyleAttrs msg }
        link =
            { title = itemConfig.title item, sa = StyleAttrs [] [ Route.href <| itemConfig.route item ] }

        moreSA : StyleAttrs msg
        moreSA =
            StyleAttrs [] [ onClick (onMoreClicked <| itemConfig.panelItemId id) ]
    in
    viewPanelNavItemHelp rootSA primaryIcon link moreSA


viewSimpleNavItem : Attribute msg -> String -> String -> Html msg
viewSimpleNavItem href title iconName =
    viewSimpleNavItemHelp (StyleAttrs [] [ href ]) { name = iconName, sa = SA.none } title


viewSimpleNavItemHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewSimpleNavItemHelp rootSA icon title =
    DI.initLink rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render


viewPanelNavItemHelp :
    StyleAttrs msg
    -> { a | name : String, sa : StyleAttrs msg }
    -> { b | title : String, sa : StyleAttrs msg }
    -> StyleAttrs msg
    -> Html msg
viewPanelNavItemHelp rootSA icon linkContent moreSA =
    DI.init rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentAsLink linkContent.title linkContent.sa
        |> DI.withSecondaryMoreAction moreSA
        |> DI.render


viewPanelNavItemGhostHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewPanelNavItemGhostHelp rootSA icon title =
    DI.init rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render
