module Drawer exposing
    ( AllPanelsConfig
    , AllPanelsState
    , Panel(..)
    , PanelConfig
    , PanelItemConfig
    , PanelItemId(..)
    , PanelLists
    , initialPanelsState
    , panelsSubscriptions
    , togglePanelExpansion
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


type alias PanelState =
    { isExpanded : Bool
    , drag : Drag
    }


type alias AllPanelsState =
    { projects : PanelState
    , labels : PanelState
    , filters : PanelState
    }


initialPanelState : PanelState
initialPanelState =
    PanelState True Drag.initial


initialPanelsState : AllPanelsState
initialPanelsState =
    AllPanelsState initialPanelState initialPanelState initialPanelState


panelGetter : Panel -> { a | projects : c, labels : c, filters : c } -> c
panelGetter panel m =
    case panel of
        Projects ->
            m.projects

        Labels ->
            m.labels

        Filters ->
            m.filters


panelSetter :
    Panel
    -> b
    -> { c | projects : b, labels : b, filters : b }
    -> { c | projects : b, labels : b, filters : b }
panelSetter panel small big =
    case panel of
        Projects ->
            { big | projects = small }

        Labels ->
            { big | labels = small }

        Filters ->
            { big | filters = small }


panelMapper :
    Panel
    -> (a -> a)
    -> { c | projects : a, labels : a, filters : a }
    -> { c | projects : a, labels : a, filters : a }
panelMapper panel func big =
    let
        get =
            panelGetter panel

        set a =
            panelSetter panel a big
    in
    get big |> func |> set


togglePanelExpansion : Panel -> AllPanelsState -> AllPanelsState
togglePanelExpansion panel =
    panelMapper panel (\s -> { s | isExpanded = not s.isExpanded })


updatePanelDrag :
    (Panel -> Drag.Msg -> msg)
    -> (Panel -> Drag.Info -> msg)
    -> Panel
    -> Drag.Msg
    -> AllPanelsState
    -> ( AllPanelsState, Cmd msg )
updatePanelDrag toMsg onComplete panel msg model =
    let
        drag =
            (panelGetter panel model).drag
    in
    Drag.update (toMsg panel) (onComplete panel) msg drag
        |> Tuple.mapFirst (\newDrag -> panelMapper panel (\s -> { s | drag = newDrag }) model)


panelsSubscriptions : AllPanelsConfig msg -> AllPanelsState -> Sub msg
panelsSubscriptions c panelState =
    Sub.batch
        [ c.projects.dragSystem.subscriptions panelState.projects.drag
        ]


type alias PanelLists =
    { projects : List Project
    , labels : List Label
    , filters : List Filter
    }


type alias AllPanelsConfig msg =
    { projects : PanelConfig ProjectId Project msg
    , labels : PanelConfig LabelId Label msg
    , filters : PanelConfig FilterId Filter msg
    }


view :
    AllPanelsConfig msg
    -> PanelLists
    -> AllPanelsState
    -> View (Html msg)
view allPanelConfig panelLists panelState =
    let
        prefixCP =
            View.content
                [ viewSimpleNavItem (Route.href Route.Inbox) "Inbox" "inbox"
                , viewSimpleNavItem (Route.href Route.Inbox) "Today" "calendar_today"
                , viewSimpleNavItem (Route.href Route.Inbox) "Next 7 Days" "view_week"
                ]

        projectsCP =
            viewPanel allPanelConfig.projects
                panelLists.projects
                panelState.projects

        labelsCP =
            viewPanel allPanelConfig.labels
                panelLists.labels
                panelState.labels

        filtersCP =
            viewPanel allPanelConfig.filters
                panelLists.filters
                panelState.labels
    in
    View.concat [ prefixCP, projectsCP, labelsCP, filtersCP ]


type PanelItemId
    = ProjectItemId ProjectId
    | LabelItemId LabelId
    | FilterItemId FilterId


type alias PanelConfig id item msg =
    { toggleExpansionClicked : msg
    , itemMoreClicked : id -> msg
    , dragSystem : Drag.System item msg
    , panelTitle : String
    , panelItemDomIdPrefix : String
    , itemConfig : PanelItemConfig id item
    }


type alias PanelItemConfig id item =
    { id : item -> id
    , idToString : id -> String
    , panelItemId : id -> PanelItemId
    , title : item -> String
    , route : item -> Route.Route
    , iconName : String
    , iconStyle : item -> Style
    }


viewPanel : PanelConfig id item msg -> List item -> PanelState -> View (Html msg)
viewPanel config items state =
    let
        dragSystem =
            config.dragSystem

        domIdPrefix =
            config.panelItemDomIdPrefix

        onMoreClicked =
            config.itemMoreClicked

        itemConfig =
            config.itemConfig
    in
    View.concat
        [ View.content
            [ ExpansionPanelUI.viewHeader config.toggleExpansionClicked config.panelTitle state.isExpanded ]
        , if state.isExpanded then
            View.fromTuple
                ( items
                    |> dragSystem.rotate state.drag
                    |> List.indexedMap
                        (viewPanelItem { domIdPrefix = domIdPrefix, onMoreClicked = onMoreClicked }
                            dragSystem
                            itemConfig
                            state.drag
                        )
                , viewPanelItemGhost dragSystem itemConfig state.drag items
                )

          else
            View.none
        ]


viewPanelItemGhost : Drag.System item msg -> PanelItemConfig id item -> Drag -> List item -> List (Html msg)
viewPanelItemGhost dragSystem itemConfig drag items =
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
                [ viewPanelItemGhostHelp rootSA icon title
                , node "style" [] [ text "body *{ cursor:move!important; }" ]
                ]
            )
        |> Maybe.withDefault []


viewPanelItem :
    { domIdPrefix : String, onMoreClicked : id -> msg }
    -> Drag.System item msg
    -> PanelItemConfig id item
    -> Drag
    -> Int
    -> item
    -> Html msg
viewPanelItem config dragSystem itemConfig drag idx item =
    let
        { domIdPrefix, onMoreClicked } =
            config

        id =
            itemConfig.id item

        domId =
            domIdPrefix ++ itemConfig.idToString id

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
            StyleAttrs [] [ onClick (onMoreClicked <| id) ]
    in
    viewPanelItemHelp rootSA primaryIcon link moreSA


viewSimpleNavItem : Attribute msg -> String -> String -> Html msg
viewSimpleNavItem href title iconName =
    viewSimpleNavItemHelp (StyleAttrs [] [ href ]) { name = iconName, sa = SA.none } title


viewSimpleNavItemHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewSimpleNavItemHelp rootSA icon title =
    DI.initLink rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render


viewPanelItemHelp :
    StyleAttrs msg
    -> { a | name : String, sa : StyleAttrs msg }
    -> { b | title : String, sa : StyleAttrs msg }
    -> StyleAttrs msg
    -> Html msg
viewPanelItemHelp rootSA icon linkContent moreSA =
    DI.init rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentAsLink linkContent.title linkContent.sa
        |> DI.withSecondaryMoreAction moreSA
        |> DI.render


viewPanelItemGhostHelp : StyleAttrs msg -> { a | name : String, sa : StyleAttrs msg } -> String -> Html msg
viewPanelItemGhostHelp rootSA icon title =
    DI.init rootSA
        |> DI.withPrimaryIcon icon.name icon.sa
        |> DI.withContentText title
        |> DI.render
