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


toggleExpansionPanel panel model =
    case panel of
        Projects ->
            { model | projectsExpanded = not model.projectsExpanded }

        Labels ->
            { model | labelsExpanded = not model.labelsExpanded }

        Filters ->
            { model | filtersExpanded = not model.filtersExpanded }


type alias Internal =
    { labelList : List LabelView
    , filterList : List FilterView
    }


labelList =
    [ LabelView "to read" 333
    , LabelView "medical" 93990
    , LabelView "quick-ref" 444
    ]


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
                    projectList |> List.map navProjectItem
                )
                eps.projectsExpanded
            ++ ExpansionPanelUI.view (onToggleExpansionPanel Labels)
                "Labels"
                (\_ ->
                    labelList |> List.map navLabelItem
                )
                eps.labelsExpanded
            ++ ExpansionPanelUI.view (onToggleExpansionPanel Filters)
                "Filters"
                (\_ ->
                    filterList |> List.map navFilterItem
                )
                eps.filtersExpanded
    , portal = []
    }


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


navProjectItem : Project -> Html msg
navProjectItem project =
    let
        title =
            Project.title project

        iconColor =
            Css.hsl (Project.hue project |> toFloat) 0.7 0.5
    in
    viewItem [] [] title iconColor "folder"


navLabelItem : LabelView -> Html msg
navLabelItem { title, hue } =
    viewItem [] [] title (Css.hsl hue 0.7 0.5) "label"


navFilterItem : FilterView -> Html msg
navFilterItem { title, hue } =
    viewItem [] [] title (Css.hsl hue 0.7 0.5) "filter_list"
