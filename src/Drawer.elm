module Drawer exposing
    ( Config
    , Panel(..)
    , PanelConfig2
    , PanelItemId(..)
    , PanelLists
    , PanelModel
    , PanelNavItemViewConfig
    , PanelsConfig
    , PanelsState
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


type alias PanelsState =
    { expanded : SubState Bool, drag : SubState Drag }


mapExpanded func panelState =
    { panelState | expanded = func panelState.expanded }


mapDrag func panelState =
    { panelState | drag = func panelState.drag }


initialPanelState : PanelsState
initialPanelState =
    PanelsState (initSubState True) (initSubState Drag.initial)


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


toggleExpansionPanel : Panel -> PanelsState -> PanelsState
toggleExpansionPanel panel =
    mapExpanded (mapSubState panel not)


updatePanelDrag :
    (Panel -> Drag.Msg -> msg)
    -> (Panel -> Drag.Info -> msg)
    -> Panel
    -> Drag.Msg
    -> PanelsState
    -> ( PanelsState, Cmd msg )
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


panelSubscriptions : (Panel -> Drag.Msg -> msg) -> PanelsState -> Sub msg
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


type alias PanelsConfig msg =
    { projects : PanelConfig2 ProjectId Project msg
    , labels : PanelConfig2 LabelId Label msg
    , filters : PanelConfig2 FilterId Filter msg
    }


view :
    PanelsConfig msg
    -> Config msg
    -> PanelLists
    -> PanelsState
    -> View (Html msg)
view allPanelConfig config panelLists panelState =
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

        projectsCP2 =
            viewPanel2 allPanelConfig.projects
                { isPanelExpanded = panelState.expanded.projects
                , drag = panelState.drag.projects
                , items = panelLists.projects
                }

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


type alias PanelConfig2 id item msg =
    { onTogglePanelClicked : msg
    , onPanelItemMoreClicked : id -> msg
    , dragSystem : Drag.System item msg
    , panelTitle : String
    , panelItemDomIdPrefix : String
    , itemConfig : PanelNavItemViewConfig id item
    }


type alias PanelModel item =
    { isPanelExpanded : Bool
    , drag : Drag
    , items : List item
    }


viewPanel2 : PanelConfig2 id item msg -> PanelModel item -> View (Html msg)
viewPanel2 config model =
    Debug.todo "impl"


viewPanel :
    PanelConfig id item msg
    -> String
    -> PanelsState
    -> View (Html msg)
viewPanel pc title panelState =
    let
        isExpanded : Bool
        isExpanded =
            pc.isExpanded panelState

        drag : Drag
        drag =
            pc.drag panelState

        dragSystem =
            pc.dragSystem

        domIdPrefix =
            pc.domIdPrefix

        onMoreClicked =
            pc.onMoreClicked

        itemConfig =
            pc.itemConfig

        items =
            pc.items
    in
    View.concat
        [ View.content
            [ ExpansionPanelUI.viewHeader pc.togglePanel title isExpanded ]
        , if isExpanded then
            View.fromTuple
                ( items
                    |> dragSystem.rotate drag
                    |> List.indexedMap
                        (viewPanelNavItem { domIdPrefix = domIdPrefix, onMoreClicked = onMoreClicked }
                            dragSystem
                            itemConfig
                            drag
                        )
                , viewPanelNavItemGhost dragSystem itemConfig drag items
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
    , isExpanded : PanelsState -> Bool
    , drag : PanelsState -> Drag
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
    { domIdPrefix : String, onMoreClicked : PanelItemId -> msg }
    -> Drag.System item msg
    -> PanelNavItemViewConfig id item
    -> Drag
    -> Int
    -> item
    -> Html msg
viewPanelNavItem config dragSystem itemConfig drag idx item =
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
