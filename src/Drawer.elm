module Drawer exposing
    ( Config
    , ExpansionPanels
    , Panel(..)
    , PanelItemId(..)
    , PanelLists
    , PanelsDragState
    , initialExpansionPanels
    , initialPanelsDragState
    , panelDragSubscriptions
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


type alias PanelState =
    { expanded : SubStateExpanded, drag : SubStateDrag }


type alias SubState a =
    { projects : a
    , labels : a
    , filters : a
    }


type alias SubStateExpanded =
    { projects : Bool
    , labels : Bool
    , filters : Bool
    }


type alias SubStateDrag =
    { projects : Drag
    , labels : Drag
    , filters : Drag
    }


initSubState : a -> SubState a
initSubState a =
    SubState a a a


getPanelSubState : Panel -> SubState a -> a
getPanelSubState panel subState =
    case panel of
        Projects ->
            subState.projects

        Labels ->
            subState.labels

        Filters ->
            subState.filters


setPanelSubState : Panel -> a -> SubState a -> SubState a
setPanelSubState panel a subState =
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
            getPanelSubState panel

        set a =
            setPanelSubState panel a subState
    in
    get subState |> func |> set


type alias ExpansionPanels =
    SubState Bool


initialExpansionPanels : ExpansionPanels
initialExpansionPanels =
    initSubState True


toggleExpansionPanel : Panel -> ExpansionPanels -> ExpansionPanels
toggleExpansionPanel panel =
    mapSubState panel not


type alias PanelsDragState =
    SubState Drag


initialPanelsDragState : PanelsDragState
initialPanelsDragState =
    initSubState Drag.initial


updatePanelDrag :
    (Panel -> Drag.Msg -> msg)
    -> (Panel -> Drag.Info -> msg)
    -> Panel
    -> Drag.Msg
    -> PanelsDragState
    -> ( PanelsDragState, Cmd msg )
updatePanelDrag toMsg onComplete panel msg dragSubState =
    let
        drag =
            getPanelSubState panel dragSubState
    in
    Drag.update (toMsg panel) (onComplete panel) msg drag
        |> Tuple.mapFirst (\newDrag -> setPanelSubState panel newDrag dragSubState)


panelDragSubscriptions : (Panel -> Drag.Msg -> msg) -> PanelsDragState -> Sub msg
panelDragSubscriptions toMsg dragSubState =
    let
        dragSubscription panel =
            Drag.subscriptions (toMsg panel) (getPanelSubState panel dragSubState)
    in
    Sub.batch (List.map dragSubscription panelTypes)


type alias Config msg =
    { onToggleExpansionPanel : Panel -> msg
    , panelToDragMsg : Panel -> Drag.Msg -> msg
    , panelToDragCompleteMsg : Panel -> Drag.Info -> msg
    , onPanelItemMoreMenuClicked : PanelItemId -> msg
    }


type alias PanelLists =
    { projectList : List Project
    , labelList : List Label
    , filterList : List Filter
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

        panelConfig : Panel -> PanelConfig item msg
        panelConfig panel =
            { togglePanel = config.onToggleExpansionPanel panel
            , dragSystem = Drag.system (config.panelToDragMsg panel) (config.panelToDragCompleteMsg panel)
            , domIdPrefix = "panel-nav-item__"
            , onMoreClicked = config.onPanelItemMoreMenuClicked
            , isExpanded = .expanded >> getPanelSubState panel
            , drag = .drag >> getPanelSubState panel
            }

        projectsCP =
            viewPanel (panelConfig Projects)
                projectNavItemViewConfig
                "Projects"
                panelState
                panelLists.projectList

        labelsCP =
            viewPanel (panelConfig Labels)
                labelNavItemViewConfig
                "Labels"
                panelState
                panelLists.labelList

        filtersCP =
            viewPanel (panelConfig Filters)
                filterNavItemViewConfig
                "Filters"
                panelState
                panelLists.filterList
    in
    View.concat [ prefixCP, projectsCP, labelsCP, filtersCP ]


type PanelItemId
    = ProjectItemId ProjectId
    | LabelItemId LabelId
    | FilterItemId FilterId


viewPanel :
    PanelConfig item msg
    -> PanelNavItemViewConfig id item
    -> String
    -> PanelState
    -> List item
    -> View (Html msg)
viewPanel pc ic title panelState list =
    let
        isExpanded =
            pc.isExpanded panelState

        drag =
            pc.drag panelState
    in
    View.concat
        [ View.content
            [ ExpansionPanelUI.viewHeader pc.togglePanel title isExpanded ]
        , if isExpanded then
            View.fromTuple
                ( list
                    |> pc.dragSystem.rotate drag
                    |> List.indexedMap (viewPanelNavItem pc ic drag)
                , pc.dragSystem.ghostStyles drag
                    |> Maybe.andThen
                        (\( idx, styles ) ->
                            List.drop idx list |> List.head |> Maybe.map (Tuple.pair styles)
                        )
                    |> Maybe.map
                        (\( ghostStyle, item ) ->
                            let
                                iconSA =
                                    StyleAttrs [ ic.iconStyle item ] []

                                rootSA =
                                    StyleAttrs [ ghostStyle ] []
                            in
                            [ viewPanelNavItemGhost rootSA { name = ic.iconName, sa = iconSA } (ic.title item)
                            , node "style" [] [ text "body *{ cursor:move!important; }" ]
                            ]
                        )
                    |> Maybe.withDefault []
                )

          else
            View.none
        ]


type alias PanelConfig item msg =
    { togglePanel : msg
    , domIdPrefix : String
    , dragSystem : Drag.System item msg
    , onMoreClicked : PanelItemId -> msg
    , isExpanded : PanelState -> Bool
    , drag : PanelState -> Drag
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


viewPanelNavItem : PanelConfig item msg -> PanelNavItemViewConfig id item -> Drag -> Int -> item -> Html msg
viewPanelNavItem config itemConfig drag idx item =
    let
        { dragSystem, domIdPrefix, onMoreClicked } =
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


viewPanelNavItemGhost : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewPanelNavItemGhost rootSA icon title =
    DI.init rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render
