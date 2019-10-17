module Drawer exposing (ExpansionPanels, Panel(..), initialExpansionPanels, toggleExpansionPanel, view)

import Css
import ExpansionPanelUI
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events as E
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
    }
    -> List Project
    -> ExpansionPanels
    -> Maybe { x | panel : Panel, dragIdx : Int, ghostStyles : Style }
    -> { content : List (Html msg), portal : List (Html msg) }
view config projectList eps dragInfo =
    let
        viewPanel_ : Panel -> List (Html msg)
        viewPanel_ =
            getPanelViewModel config projectList eps >> viewPanel

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
                    maybeGhostNavItem |> Maybe.map (viewNavItemWithAttrsAndStyles [] [ ghostStyles ])

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
    }
    -> List Project
    -> ExpansionPanels
    -> Panel
    -> PanelViewModel msg
getPanelViewModel config projectList expansionPanels panel =
    let
        toggleExpansion =
            config.onToggleExpansionPanel panel

        dragEvents =
            config.dragEvents panel

        isExpanded =
            isPanelExpanded panel expansionPanels

        lazyContent : (a -> NavItemViewModel) -> List a -> () -> List (Html msg)
        lazyContent func list _ =
            List.indexedMap
                (\idx ->
                    func
                        >> (\navItem ->
                                let
                                    domId =
                                        "panel-dnd-item__" ++ navItem.id
                                in
                                viewNavItemWithAttrs (A.id domId :: dragEvents idx domId) navItem
                           )
                )
                list
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


viewPanel : PanelViewModel msg -> List (Html msg)
viewPanel { title, toggleExpansion, isExpanded, lazyContent } =
    ExpansionPanelUI.view toggleExpansion
        title
        lazyContent
        isExpanded


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


viewNavItemWithAttrs : List (Attribute msg) -> NavItemViewModel -> Html msg
viewNavItemWithAttrs attrs { title, iconColor, icon } =
    viewItem attrs [] title iconColor icon


viewNavItemWithAttrsAndStyles : List (Attribute msg) -> List Style -> NavItemViewModel -> Html msg
viewNavItemWithAttrsAndStyles attrs styles { title, iconColor, icon } =
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
