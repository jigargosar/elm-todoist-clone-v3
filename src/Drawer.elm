module Drawer exposing
    ( Config
    , DragInfo
    , ExpansionPanels
    , Panel(..)
    , initialExpansionPanels
    , isPanelExpanded
    , toggleExpansionPanel
    , view
    )

import Css
import Drag
import ExpansionPanelUI
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (class, css)
import Project exposing (Project)
import ProjectId
import Styles exposing (..)


type alias ExpansionPanels =
    { projectsExpanded : Bool
    , labelsExpanded : Bool
    , filtersExpanded : Bool
    }


initialExpansionPanels : ExpansionPanels
initialExpansionPanels =
    ExpansionPanels True True True


toggleExpansionPanel : Panel -> ExpansionPanels -> ExpansionPanels
toggleExpansionPanel panel model =
    case panel of
        Projects ->
            { model | projectsExpanded = not model.projectsExpanded }

        Labels ->
            { model | labelsExpanded = not model.labelsExpanded }

        Filters ->
            { model | filtersExpanded = not model.filtersExpanded }


isPanelExpanded : Panel -> ExpansionPanels -> Bool
isPanelExpanded panel =
    case panel of
        Projects ->
            .projectsExpanded

        Labels ->
            .labelsExpanded

        Filters ->
            .filtersExpanded


labelList : List LabelView
labelList =
    [ LabelView "to read" 333
    , LabelView "medical" 93990
    , LabelView "quick-ref" 444
    ]


filterList : List FilterView
filterList =
    [ FilterView "Assigned to me" 933
    , FilterView "Assigned to others" 9354
    , FilterView "Priority 1" 93344
    , FilterView "Priority 2" 932323
    , FilterView "Priority 3" 932323
    , FilterView "View all" 932325
    , FilterView "No due date" 9355
    ]


type Panel
    = Projects
    | Labels
    | Filters


type alias LabelView =
    { title : String, hue : Float }


type alias FilterView =
    { title : String, hue : Float }


type alias DragInfo =
    Maybe
        { panel : Panel
        , dragIdx : Int
        , ghostStyles : Style
        , dropIdx : Int
        }


type alias Config msg =
    { onToggleExpansionPanel : Panel -> msg
    }


type alias PanelLists =
    { projectList : List Project
    , labelList : List LabelView
    , filterList : List FilterView
    }


view :
    Config msg
    -> List Project
    -> ExpansionPanels
    -> { content : List (Html msg), portal : List (Html msg) }
view config projectList expansionPanels =
    let
        panelLists =
            { projectList = projectList, labelList = labelList, filterList = filterList }
    in
    { content = [], portal = [] }


viewPanel togglePanel title isExpanded dragToMsg drag toNavItem list =
    ExpansionPanelUI.view togglePanel
        title
        (\_ ->
            let
                viewDnDNavItem idx navItem =
                    let
                        domId =
                            "panel-dnd-item__" ++ navItem.id

                        dragEvents =
                            Drag.dragEvents dragToMsg idx domId drag

                        dropEvents =
                            Drag.dropEvents dragToMsg idx drag

                        dragOverStyles =
                            Styles.styleIf (Drag.eqDragOverIdx idx drag) [ Css.opacity <| Css.zero ]

                        styles =
                            [ Styles.noSelection, dragOverStyles ]
                    in
                    viewNavItem (A.id domId :: dragEvents ++ dropEvents) styles navItem
            in
            list
                |> List.map toNavItem
                |> List.indexedMap viewDnDNavItem
        )
        isExpanded


onlyContent content =
    { content = content, portal = [] }


mergeContentPortal : List { content : List x, portal : List x } -> { content : List x, portal : List x }
mergeContentPortal =
    List.foldl
        (\cp acc -> { acc | content = acc.content ++ cp.content, portal = acc.portal ++ cp.portal })
        { content = [], portal = [] }


panelTitle : Panel -> String
panelTitle panel =
    case panel of
        Projects ->
            "Projects"

        Labels ->
            "Labels"

        Filters ->
            "Filters"



--getPanelContentPortal :
--    Config msg
--    -> Panel
--    -> { content : List (Html msg), portal : List (Html msg) }
--getPanelContentPortal config panel =
--    let
--        getCP =
--            getPanelContentPortalHelp config panel
--    in
--    case panel of
--        Projects ->
--            getCP projectToNavItem config.projectList
--
--        Labels ->
--            getCP labelToNavItem labelList
--
--        Filters ->
--            getCP filterToNavItem filterList
--
--getPanelContentPortalHelp :
--    Config msg
--    -> Panel
--    -> (a -> NavItemViewModel)
--    -> List a
--    -> { content : List (Html msg), portal : List (Html msg) }
--getPanelContentPortalHelp config panel toNavItem list =
--    let
--        viewDnDNavItem idx navItem =
--            let
--                domId =
--                    "panel-dnd-item__" ++ navItem.id
--
--                dragEvents =
--                    config.dragEvents panel idx domId
--
--                dropEvents =
--                    config.dropEvents panel idx domId
--
--                styles =
--                    config.dragInfo panel
--                        |> Maybe.andThen
--                            (\{ dropIdx } ->
--                                if idx == dropIdx then
--                                    Just [ Css.opacity <| Css.num 0 ]
--
--                                else
--                                    Nothing
--                            )
--                        |> Maybe.withDefault []
--            in
--            viewNavItem (A.id domId :: dragEvents ++ dropEvents) styles navItem
--    in
--    { content =
--        ExpansionPanelUI.view (config.onToggleExpansionPanel panel)
--            (panelTitle panel)
--            (\_ ->
--                list
--                    |> List.map toNavItem
--                    |> config.sort panel
--                    |> List.indexedMap (\idx -> viewDnDNavItem idx)
--            )
--            (config.isPanelExpanded panel)
--    , portal =
--        config.dragInfo panel
--            |> Maybe.andThen
--                (\{ dragIdx, ghostStyles } ->
--                    List.drop dragIdx list
--                        |> List.head
--                        |> Maybe.map (toNavItem >> viewNavItem [] [ ghostStyles ] >> List.singleton)
--                )
--            |> Maybe.withDefault []
--    }
--
--


type alias NavItemViewModel =
    { id : String
    , title : String
    , iconColor : Css.Color
    , icon : String
    }


projectToNavItem : Project -> NavItemViewModel
projectToNavItem project =
    { id = ProjectId.toString (Project.id project)
    , title = Project.title project
    , iconColor = Css.hsl (Project.hue project |> toFloat) 0.7 0.5
    , icon = "folder"
    }


labelToNavItem : LabelView -> NavItemViewModel
labelToNavItem { title, hue } =
    { id = title
    , title = title
    , iconColor = Css.hsl hue 0.7 0.5
    , icon = "label"
    }


filterToNavItem : FilterView -> NavItemViewModel
filterToNavItem { title, hue } =
    { id = title
    , title = title
    , iconColor = Css.hsl hue 0.7 0.5
    , icon = "filter_list"
    }


navTitleIconItem title icon =
    viewItem [] [] title Css.inherit icon


viewNavItem : List (Attribute msg) -> List Style -> NavItemViewModel -> Html msg
viewNavItem attrs styles { title, iconColor, icon } =
    viewItem attrs styles title iconColor icon


type alias ColorCompatible x =
    { x | value : String, color : Css.Compatible }


viewItem : List (Attribute msg) -> List Css.Style -> String -> ColorCompatible x -> String -> Html msg
viewItem attributes styles title iconColor iconName =
    div
        (css
            [ ph 1
            , pointer
            , flex
            , c_grayL 0.3
            , batch styles
            ]
            :: attributes
        )
        [ i
            [ css [ pv 2, ph 1, flex, itemsCenter, c_ iconColor ]
            , class "material-icons"
            ]
            [ text iconName ]
        , div
            [ css [ pv 2, ph 1, flex, itemsCenter, mr 3 ]
            ]
            [ text title ]
        ]
