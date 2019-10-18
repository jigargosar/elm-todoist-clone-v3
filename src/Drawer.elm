module Drawer exposing
    ( Config
    , ExpansionPanels
    , FilterView
    , LabelView
    , Panel(..)
    , PanelLists
    , PanelsDragState
    , filterList
    , initialExpansionPanels
    , initialPanelsDragState
    , labelList
    , panelDragSubscriptions
    , toggleExpansionPanel
    , updatePanelDrag
    , view
    )

import Css
import Drag exposing (Drag)
import ExpansionPanelUI
import Html.Styled exposing (..)
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


type alias Config msg =
    { onToggleExpansionPanel : Panel -> msg
    , panelToDragMsg : Panel -> Drag.Msg -> msg
    , panelToDragChangeMsg : Panel -> Drag.Info -> msg
    }


type alias PanelLists =
    { projectList : List Project
    , labelList : List LabelView
    , filterList : List FilterView
    }


type alias PanelsDragState =
    { projectsDrag : Drag
    , labelsDrag : Drag
    , filtersDrag : Drag
    }


initialPanelsDragState : PanelsDragState
initialPanelsDragState =
    { projectsDrag = Drag.initial
    , labelsDrag = Drag.initial
    , filtersDrag = Drag.initial
    }


updatePanelDrag :
    (Panel -> Drag.Msg -> msg)
    -> (Panel -> Drag.Info -> msg)
    -> Panel
    -> Drag.Msg
    -> PanelsDragState
    -> ( PanelsDragState, Cmd msg )
updatePanelDrag toMsg onChange panel msg model =
    let
        updateHelp =
            Drag.update (toMsg panel) (onChange panel) msg
    in
    case panel of
        Projects ->
            updateHelp model.projectsDrag
                |> Tuple.mapFirst (\drag -> { model | projectsDrag = drag })

        Labels ->
            updateHelp model.labelsDrag
                |> Tuple.mapFirst (\drag -> { model | labelsDrag = drag })

        Filters ->
            updateHelp model.filtersDrag
                |> Tuple.mapFirst (\drag -> { model | filtersDrag = drag })


panelDragSubscriptions : (Panel -> Drag.Msg -> msg) -> PanelsDragState -> Sub msg
panelDragSubscriptions toMsg model =
    Sub.batch
        [ Drag.subscriptions (toMsg Projects) model.projectsDrag
        , Drag.subscriptions (toMsg Labels) model.labelsDrag
        , Drag.subscriptions (toMsg Filters) model.filtersDrag
        ]


view :
    Config msg
    -> PanelLists
    -> ExpansionPanels
    -> PanelsDragState
    -> ContentPortal msg
view config panelLists expansionPanels panelsDragState =
    let
        prefixCP =
            onlyContent
                [ navTitleIconItem "Inbox" "inbox"
                , navTitleIconItem "Today" "calendar_today"
                , navTitleIconItem "Next 7 Days" "view_week"
                ]

        projectsCP =
            viewPanel (config.onToggleExpansionPanel Projects)
                "Projects"
                expansionPanels.projectsExpanded
                (Drag.system (config.panelToDragMsg Projects) (config.panelToDragChangeMsg Projects))
                panelsDragState.projectsDrag
                projectToNavItem
                panelLists.projectList

        labelsCP =
            viewPanel (config.onToggleExpansionPanel Labels)
                "Labels"
                expansionPanels.labelsExpanded
                (Drag.system (config.panelToDragMsg Labels) (config.panelToDragChangeMsg Labels))
                panelsDragState.labelsDrag
                labelToNavItem
                panelLists.labelList

        filtersCP =
            viewPanel (config.onToggleExpansionPanel Filters)
                "Filters"
                expansionPanels.filtersExpanded
                (Drag.system (config.panelToDragMsg Filters) (config.panelToDragChangeMsg Filters))
                panelsDragState.filtersDrag
                filterToNavItem
                panelLists.filterList
    in
    [ prefixCP, projectsCP, labelsCP, filtersCP ]
        |> mergeContentPortal


type alias ContentPortal msg =
    { content : List (Html msg), portal : List (Html msg) }


viewPanel :
    msg
    -> String
    -> Bool
    -> Drag.System a msg
    -> Drag
    -> (a -> NavItemViewModel)
    -> List a
    -> ContentPortal msg
viewPanel togglePanel title isExpanded dragSystem drag toNavItem list =
    let
        ghostItem =
            dragSystem.ghostStyles drag
                |> Maybe.andThen
                    (\( idx, styles ) ->
                        List.drop idx list |> List.head |> Maybe.map (toNavItem >> Tuple.pair styles)
                    )
                |> Maybe.map
                    (\( styles, navItem ) ->
                        viewNavItem [] [ styles ] navItem
                    )
                |> Maybe.withDefault (text "")
    in
    { content =
        ExpansionPanelUI.view togglePanel
            title
            (\_ ->
                let
                    viewDnDNavItem idx navItem =
                        let
                            domId =
                                String.toLower title ++ "-panel-drag-item__" ++ navItem.id

                            dragEvents =
                                dragSystem.dragEvents idx domId drag

                            dropEvents =
                                dragSystem.dropEvents idx drag

                            dragOverStyles =
                                Styles.styleIf (dragSystem.eqDragOverIdx idx drag) [ Css.opacity <| Css.zero ]

                            styles =
                                [ Styles.noSelection, dragOverStyles ]
                        in
                        viewNavItem (A.id domId :: dragEvents ++ dropEvents) styles navItem
                in
                list
                    |> dragSystem.rotate drag
                    |> List.map toNavItem
                    |> List.indexedMap viewDnDNavItem
            )
            isExpanded
    , portal = [ ghostItem ]
    }


onlyContent content =
    { content = content, portal = [] }


mergeContentPortal : List { content : List x, portal : List x } -> { content : List x, portal : List x }
mergeContentPortal =
    List.foldl
        (\cp acc -> { acc | content = acc.content ++ cp.content, portal = acc.portal ++ cp.portal })
        { content = [], portal = [] }


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
