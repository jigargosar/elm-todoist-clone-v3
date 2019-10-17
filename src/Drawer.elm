module Drawer exposing (ExpansionPanels, Panel(..), initialExpansionPanels, isPanelExpanded, toggleExpansionPanel, view)

import Css
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


view :
    { onToggleExpansionPanel : Panel -> msg
    , dragEvents : Panel -> Int -> String -> List (H.Attribute msg)
    , isPanelExpanded : Panel -> Bool
    }
    -> List Project
    -> Maybe { x | panel : Panel, dragIdx : Int, ghostStyles : Style }
    -> { content : List (Html msg), portal : List (Html msg) }
view config projectList dragInfo =
    let
        viewPanel_ : Panel -> List (Html msg)
        viewPanel_ panel =
            getPanelViewModel config projectList panel |> viewPanel config panel

        ghostItem : Maybe (Html msg)
        ghostItem =
            case dragInfo of
                Nothing ->
                    Nothing

                Just { dragIdx, panel, ghostStyles } ->
                    let
                        maybeGhostNavItem =
                            case panel of
                                Projects ->
                                    List.drop dragIdx projectList
                                        |> List.head
                                        |> Maybe.map projectToNavItem

                                Labels ->
                                    List.drop dragIdx labelList
                                        |> List.head
                                        |> Maybe.map labelToNavItem

                                Filters ->
                                    List.drop dragIdx filterList
                                        |> List.head
                                        |> Maybe.map filterToNavItem
                    in
                    maybeGhostNavItem |> Maybe.map (viewNavItem [] [ ghostStyles ])

        portal =
            case ghostItem of
                Nothing ->
                    []

                Just html_ ->
                    [ html_ ]
    in
    { content =
        [ navTitleIconItem "Inbox" "inbox"
        , navTitleIconItem "Today" "calendar_today"
        , navTitleIconItem "Next 7 Days" "view_week"
        ]
            ++ viewPanel_ Projects
            ++ viewPanel_ Labels
            ++ viewPanel_ Filters
    , portal = portal
    }


type alias PanelViewModel msg =
    { title : String
    , toggleExpansion : msg
    , isExpanded : Bool
    , lazyContent : () -> List (Html msg)
    }


getPanelViewModel :
    { onToggleExpansionPanel : Panel -> msg
    , dragEvents : Panel -> Int -> String -> List (H.Attribute msg)
    , isPanelExpanded : Panel -> Bool
    }
    -> List Project
    -> Panel
    -> PanelViewModel msg
getPanelViewModel config projectList panel =
    let
        toggleExpansion =
            config.onToggleExpansionPanel panel

        dragEvents =
            config.dragEvents panel

        isExpanded =
            config.isPanelExpanded panel

        lazyContentConfig =
            { dragEvents = dragEvents }
    in
    case panel of
        Projects ->
            PanelViewModel "Projects"
                toggleExpansion
                isExpanded
                (panelLazyContent lazyContentConfig projectToNavItem projectList)

        Labels ->
            PanelViewModel "Labels"
                toggleExpansion
                isExpanded
                (panelLazyContent lazyContentConfig labelToNavItem labelList)

        Filters ->
            PanelViewModel "Filters"
                toggleExpansion
                isExpanded
                (panelLazyContent lazyContentConfig filterToNavItem filterList)


panelLazyContent :
    { dragEvents : Int -> String -> List (H.Attribute msg) }
    -> (a -> NavItemViewModel)
    -> List a
    -> ()
    -> List (Html msg)
panelLazyContent config func list _ =
    List.indexedMap
        (\idx ->
            func
                >> (\navItem ->
                        let
                            domId =
                                "panel-dnd-item__" ++ navItem.id
                        in
                        viewNavItem (A.id domId :: config.dragEvents idx domId) [] navItem
                   )
        )
        list


viewPanel :
    { onToggleExpansionPanel : Panel -> msg
    , dragEvents : Panel -> Int -> String -> List (H.Attribute msg)
    , isPanelExpanded : Panel -> Bool
    }
    -> Panel
    -> PanelViewModel msg
    -> List (Html msg)
viewPanel config panel { title, lazyContent } =
    ExpansionPanelUI.view (config.onToggleExpansionPanel panel)
        title
        lazyContent
        (config.isPanelExpanded panel)


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
