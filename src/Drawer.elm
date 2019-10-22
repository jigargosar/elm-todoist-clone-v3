module Drawer exposing
    ( AllPanelsConfig
    , AllPanelsState
    , Panel(..)
    , PanelConfig
    , PanelItemConfig
    , PanelItemId(..)
    , PanelLists
    , getDragForPanel
    , initialPanelsState
    , togglePanelExpansion
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
import Html.Styled.Events as E
import Json.Decode as JD
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
    }


type TaggedPanelState id
    = TaggedPanelState PanelState


projectsPanelState : AllPanelsState -> TaggedPanelState ProjectId
projectsPanelState =
    .projects >> TaggedPanelState


labelsPanelState : AllPanelsState -> TaggedPanelState LabelId
labelsPanelState =
    .labels >> TaggedPanelState


filtersPanelState : AllPanelsState -> TaggedPanelState FilterId
filtersPanelState =
    .filters >> TaggedPanelState


type alias AllPanelsState =
    { projects : PanelState
    , labels : PanelState
    , filters : PanelState
    }


initialPanelState : PanelState
initialPanelState =
    PanelState True


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


getDragForPanel : a -> Maybe ( a, Drag ) -> Drag
getDragForPanel panel panelDrag =
    case panelDrag of
        Nothing ->
            Drag.initial

        Just ( panel_, drag ) ->
            if panel_ == panel then
                drag

            else
                Drag.initial


view :
    AllPanelsConfig msg
    -> PanelLists
    -> AllPanelsState
    -> Maybe ( Panel, Drag )
    -> View (Html msg)
view allPanelConfig panelLists state panelDrag =
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
                (projectsPanelState state)
                (getDragForPanel Projects panelDrag)

        labelsCP =
            viewPanel allPanelConfig.labels
                panelLists.labels
                (labelsPanelState state)
                (getDragForPanel Labels panelDrag)

        filtersCP =
            viewPanel allPanelConfig.filters
                panelLists.filters
                (filtersPanelState state)
                (getDragForPanel Filters panelDrag)
    in
    View.concat [ prefixCP, projectsCP, labelsCP, filtersCP ]


type PanelItemId
    = ProjectItemId ProjectId
    | LabelItemId LabelId
    | FilterItemId FilterId


type alias PanelConfig id item msg =
    { toggleExpansionClicked : msg
    , panelTitle : String
    , itemConfig : PanelItemConfig id item msg
    }


type alias PanelItemConfig id item msg =
    { moreClicked : String -> id -> JD.Decoder msg
    , dragSystem : Drag.System item msg
    , dragMsg : Drag.Msg -> msg
    , panelId : String
    , id : item -> id
    , idToString : id -> String
    , title : item -> String
    , route : item -> Route.Route
    , iconName : String
    , iconStyle : item -> Style
    }


viewPanel : PanelConfig id item msg -> List item -> TaggedPanelState id -> Drag -> View (Html msg)
viewPanel config items (TaggedPanelState state) drag =
    View.concat
        [ View.content
            [ ExpansionPanelUI.viewHeader
                config.toggleExpansionClicked
                config.panelTitle
                state.isExpanded
            ]
        , if state.isExpanded then
            viewPanelItems config.itemConfig items drag

          else
            View.none
        ]


viewPanelItems : PanelItemConfig id item msg -> List item -> Drag -> View (Html msg)
viewPanelItems config items drag =
    View.fromTuple
        ( items
            |> config.dragSystem.rotate drag
            |> List.indexedMap
                (viewPanelItem config drag)
        , viewPanelItemGhost config items drag
        )


panelItemDomId : PanelItemConfig id item msg -> id -> String
panelItemDomId config id =
    "drawer-panel__ " ++ config.panelId ++ "__item__" ++ config.idToString id


panelItemMoreBtnDomId : PanelItemConfig id item msg -> id -> String
panelItemMoreBtnDomId config id =
    panelItemDomId config id ++ "__more-btn"


viewPanelItem :
    PanelItemConfig id item msg
    -> Drag
    -> Int
    -> item
    -> Html msg
viewPanelItem config drag idx item =
    let
        id =
            config.id item

        dragSystem =
            config.dragSystem

        domId =
            panelItemDomId config id

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
            { name = config.iconName
            , sa = StyleAttrs [ Css.cursor Css.move, config.iconStyle item ] dragEvents
            }

        link : { title : String, sa : StyleAttrs msg }
        link =
            { title = config.title item, sa = StyleAttrs [] [ Route.href <| config.route item ] }

        moreDomId =
            panelItemMoreBtnDomId config id

        moreSA : StyleAttrs msg
        moreSA =
            StyleAttrs [] [ A.id moreDomId, E.on "click" (config.moreClicked moreDomId id) ]
    in
    viewPanelItemHelp rootSA primaryIcon link moreSA


viewPanelItemGhost : PanelItemConfig id item msg -> List item -> Drag -> List (Html msg)
viewPanelItemGhost config items drag =
    config.dragSystem.ghostItemWithStyles items drag
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
