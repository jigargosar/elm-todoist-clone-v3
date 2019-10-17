module Drawer exposing (ExpansionPanels, Panel(..), initialExpansionPanels, toggleExpansionPanel, view)

import Css
import ExpansionPanelUI
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css)
import Project exposing (Project)
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
    { onToggleExpansionPanel : Panel -> msg }
    -> List Project
    -> ExpansionPanels
    -> { content : List (Html msg), portal : List (Html msg) }
view { onToggleExpansionPanel } projectList eps =
    { content =
        [ navTitleIconItem "Inbox" "inbox"
        , navTitleIconItem "Today" "calendar_today"
        , navTitleIconItem "Next 7 Days" "view_week"
        ]
            ++ ExpansionPanelUI.view (onToggleExpansionPanel Projects)
                "Projects"
                (\_ ->
                    projectList |> List.map (projectToNavItem >> viewNavItem)
                )
                eps.projectsExpanded
            ++ ExpansionPanelUI.view (onToggleExpansionPanel Labels)
                "Labels"
                (\_ ->
                    labelList |> List.map (labelToNavItem >> viewNavItem)
                )
                eps.labelsExpanded
            ++ ExpansionPanelUI.view (onToggleExpansionPanel Filters)
                "Filters"
                (\_ ->
                    filterList |> List.map (filterToNavItem >> viewNavItem)
                )
                eps.filtersExpanded
    , portal = []
    }


type alias PanelViewModel msg =
    { title : String
    , toggleExpansion : msg
    , isExpanded : Bool
    , lazyContent : () -> List (Html msg)
    }


getPanelViewModel panel projectList onToggleExpansionPanel expansionPanels =
    let
        toggleExpansion =
            onToggleExpansionPanel panel

        isExpanded =
            isPanelExpanded panel expansionPanels

        lazyContent : (a -> NavItemViewModel) -> List a -> () -> List (Html msg)
        lazyContent func list _ =
            List.map (func >> viewNavItem) list
    in
    case panel of
        Projects ->
            PanelViewModel "Projects"
                toggleExpansion
                isExpanded
                (lazyContent projectToNavItem projectList)

        Labels ->
            PanelViewModel "Labels"
                toggleExpansion
                isExpanded
                (lazyContent labelToNavItem labelList)

        Filters ->
            PanelViewModel "Filters"
                toggleExpansion
                isExpanded
                (lazyContent filterToNavItem filterList)


type alias NavItemViewModel =
    { title : String
    , iconColor : Css.Color
    , icon : String
    }


projectToNavItem : Project -> NavItemViewModel
projectToNavItem project =
    { title = Project.title project
    , iconColor = Css.hsl (Project.hue project |> toFloat) 0.7 0.5
    , icon = "folder"
    }


labelToNavItem : LabelView -> NavItemViewModel
labelToNavItem { title, hue } =
    { title = title
    , iconColor = Css.hsl hue 0.7 0.5
    , icon = "label"
    }


filterToNavItem : FilterView -> NavItemViewModel
filterToNavItem { title, hue } =
    { title = title
    , iconColor = Css.hsl hue 0.7 0.5
    , icon = "filter_list"
    }


navTitleIconItem title icon =
    viewItem [] [] title Css.inherit icon


viewNavItem : NavItemViewModel -> Html msg
viewNavItem { title, iconColor, icon } =
    viewItem [] [] title iconColor icon


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
