module Drawer exposing
    ( Config
    , ExpansionPanels
    , FilterView
    , LabelView
    , Panel(..)
    , PanelItemId(..)
    , PanelLists
    , PanelsDragState
    , filterList
    , initialExpansionPanels
    , initialPanelsDragState
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
import Label exposing (Label)
import LabelId exposing (LabelId)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Route
import StyleAttrs as SA exposing (StyleAttrs)
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


filterList : List FilterView
filterList =
    [ FilterView "Assigned to me" 933
    , FilterView "Assigned to others" 9354
    , FilterView "Priority 1" 93344
    , FilterView "Priority 2" 932323
    , FilterView "Priority 3" 932323
    , FilterView "View all" 932325
    , FilterView "No due date" 93555
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
    , panelToDragCompleteMsg : Panel -> Drag.Info -> msg
    , onPanelItemMoreMenuClicked : PanelItemId -> msg
    }


type alias PanelLists =
    { projectList : List Project
    , labelList : List Label
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
updatePanelDrag toMsg onComplete panel msg model =
    let
        updateHelp =
            Drag.update (toMsg panel) (onComplete panel) msg
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
                [ navTitleIconItem (Route.href Route.Inbox) "Inbox" "inbox"
                , navTitleIconItem (Route.href Route.Inbox) "Today" "calendar_today"
                , navTitleIconItem (Route.href Route.Inbox) "Next 7 Days" "view_week"
                ]

        panelConfig panel =
            { togglePanel = config.onToggleExpansionPanel panel
            , dragSystem = Drag.system (config.panelToDragMsg panel) (config.panelToDragCompleteMsg panel)
            }

        projectsCP =
            viewPanel (panelConfig Projects)
                "Projects"
                expansionPanels.projectsExpanded
                panelsDragState.projectsDrag
                projectToNavItem
                panelLists.projectList

        labelsCP =
            viewPanel (panelConfig Labels)
                "Labels"
                expansionPanels.labelsExpanded
                panelsDragState.labelsDrag
                labelToNavItem
                panelLists.labelList

        filtersCP =
            viewPanel (panelConfig Filters)
                "Filters"
                expansionPanels.filtersExpanded
                panelsDragState.filtersDrag
                filterToNavItem
                panelLists.filterList
    in
    [ prefixCP, projectsCP, labelsCP, filtersCP ]
        |> mergeContentPortal


type alias ContentPortal msg =
    { content : List (Html msg), portal : List (Html msg) }


type PanelItem
    = ProjectItem Project
    | LabelItem Label
    | FilterItem FilterView


type PanelItemId
    = ProjectItemId ProjectId
    | LabelItemId LabelId
    | FilterItemId String
    | OtherItemId


viewPanel :
    { togglePanel : msg, dragSystem : Drag.System a msg }
    -> String
    -> Bool
    -> Drag
    -> (a -> NavItemViewModel id msg)
    -> List a
    -> ContentPortal msg
viewPanel { togglePanel, dragSystem } title isExpanded drag toNavItem list =
    let
        ghostItem =
            dragSystem.ghostStyles drag
                |> Maybe.andThen
                    (\( idx, styles ) ->
                        List.drop idx list |> List.head |> Maybe.map (toNavItem >> Tuple.pair styles)
                    )
                |> Maybe.map
                    (\( styles, navItem ) ->
                        [ viewNavItem (StyleAttrs [ styles ] []) navItem
                        , node "style" [] [ text "body *{ cursor:move!important; }" ]
                        ]
                    )
                |> Maybe.withDefault []
    in
    { content =
        ExpansionPanelUI.view togglePanel
            title
            (\_ ->
                let
                    viewDnDNavItem : Int -> NavItemViewModel id msg -> Html msg
                    viewDnDNavItem idx navItem =
                        let
                            domId =
                                String.toLower title ++ "-panel-drag-item__" ++ navItem.idToString navItem.id

                            dragEvents =
                                dragSystem.dragEvents idx domId drag

                            dropEvents =
                                dragSystem.dropEvents idx drag

                            dragOverStyle =
                                Styles.styleIf (dragSystem.eqDragOverIdx idx drag) [ Css.opacity <| Css.zero ]

                            rootSA =
                                StyleAttrs [ dragOverStyle ] (A.id domId :: dropEvents)

                            iconSA =
                                StyleAttrs [ Css.cursor Css.move ] dragEvents

                            newNavItem =
                                { navItem | iconSA = SA.concat navItem.iconSA iconSA }
                        in
                        viewNavItem rootSA newNavItem
                in
                list
                    |> dragSystem.rotate drag
                    |> List.map toNavItem
                    |> List.indexedMap viewDnDNavItem
            )
            isExpanded
    , portal = ghostItem
    }


onlyContent content =
    { content = content, portal = [] }


mergeContentPortal : List { content : List x, portal : List x } -> { content : List x, portal : List x }
mergeContentPortal =
    List.foldl
        (\cp acc -> { acc | content = acc.content ++ cp.content, portal = acc.portal ++ cp.portal })
        { content = [], portal = [] }


type alias NavItemViewModel id msg =
    { id : id
    , panelItemId : PanelItemId
    , idToString : id -> String
    , title : String
    , href : Attribute msg
    , iconName : String
    , iconSA : StyleAttrs msg
    }


projectToNavItem : Project -> NavItemViewModel ProjectId msg
projectToNavItem project =
    let
        projectId =
            Project.id project
    in
    { id = projectId
    , panelItemId = ProjectItemId projectId
    , idToString = ProjectId.toString
    , title = Project.title project
    , href = Route.href (Route.Project projectId)
    , iconName = "folder"
    , iconSA = StyleAttrs [ c_ <| Project.cssColor project ] []
    }


labelToNavItem : Label -> NavItemViewModel LabelId msg
labelToNavItem label =
    { id = Label.id label
    , panelItemId = LabelItemId (Label.id label)
    , idToString = LabelId.toString
    , title = Label.title label
    , href = Route.href Route.Root
    , iconName = "label"
    , iconSA = StyleAttrs [ c_ <| Css.hsl (Label.hue label |> toFloat) 0.7 0.5 ] []
    }


filterToNavItem : FilterView -> NavItemViewModel String msg
filterToNavItem { title, hue } =
    { id = title
    , panelItemId = FilterItemId title
    , idToString = identity
    , title = title
    , href = Route.href Route.Root
    , iconName = "filter_list"
    , iconSA = StyleAttrs [ c_ <| Css.hsl hue 0.7 0.5 ] []
    }


navTitleIconItem : Attribute msg -> String -> String -> Html msg
navTitleIconItem href title iconName =
    viewNavItem SA.none
        { id = title
        , panelItemId = OtherItemId
        , idToString = identity
        , title = title
        , href = href
        , iconName = iconName
        , iconSA = StyleAttrs [ c_inherit ] []
        }


viewNavItem : StyleAttrs msg -> NavItemViewModel id msg -> Html msg
viewNavItem rootSA { title, iconName, iconSA, href } =
    div
        (SA.toAttrsWithBase
            [ ph 1
            , pointer
            , flex
            , c_grayL 0.3
            , hover [ bgGrayL 0.9 ]
            , noSelection
            ]
            [ class "hover_parent" ]
            rootSA
        )
        [ i
            (SA.toAttrsWithBase
                [ pv 2, ph 1, flex, itemsCenter ]
                [ class "material-icons" ]
                iconSA
            )
            [ text iconName ]
        , a
            [ css
                [ Css.textDecoration Css.none
                , Css.visited [ Css.color Css.inherit ]
                , Css.color Css.inherit
                , pv 2
                , ph 1
                , flex
                , flexGrow1
                , itemsCenter
                ]
            , href
            ]
            [ text title ]
        , i
            [ css [ pv 2, ph 1, mr 3 ]
            , class "show_on_parent_hover"
            , class "material-icons"
            ]
            [ text "more_horiz" ]
        ]
