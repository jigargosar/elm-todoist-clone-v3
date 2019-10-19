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
import LabelId
import Project exposing (Project)
import ProjectId
import Route
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
    , panelToDragCompleteMsg : Panel -> Drag.Info -> msg
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

        projectsCP =
            viewPanel (config.onToggleExpansionPanel Projects)
                "Projects"
                expansionPanels.projectsExpanded
                (Drag.system (config.panelToDragMsg Projects) (config.panelToDragCompleteMsg Projects))
                panelsDragState.projectsDrag
                projectToNavItem
                panelLists.projectList

        labelsCP =
            viewPanel (config.onToggleExpansionPanel Labels)
                "Labels"
                expansionPanels.labelsExpanded
                (Drag.system (config.panelToDragMsg Labels) (config.panelToDragCompleteMsg Labels))
                panelsDragState.labelsDrag
                labelToNavItem
                panelLists.labelList

        filtersCP =
            viewPanel (config.onToggleExpansionPanel Filters)
                "Filters"
                expansionPanels.filtersExpanded
                (Drag.system (config.panelToDragMsg Filters) (config.panelToDragCompleteMsg Filters))
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
    -> (a -> NavItemViewModel msg)
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
                        viewNavItem (StyleAttrs [ styles ] []) navItem
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

                            rootSA =
                                StyleAttrs styles (A.id domId :: dropEvents)

                            iconSA =
                                StyleAttrs [] dragEvents

                            newNavItem =
                                { navItem | iconSA = concatStyleAttributes navItem.iconSA iconSA }
                        in
                        viewNavItem rootSA newNavItem
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


type alias NavItemViewModel msg =
    { id : String
    , title : String
    , iconName : String
    , iconSA : StyleAttrs msg
    , href : Attribute msg
    }


projectToNavItem : Project -> NavItemViewModel msg
projectToNavItem project =
    let
        projectId =
            Project.id project
    in
    { id = ProjectId.toString projectId
    , title = Project.title project
    , iconName = "folder"
    , iconSA = StyleAttrs [ c_ <| Project.cssColor project ] []
    , href = Route.href (Route.Project projectId)
    }


labelToNavItem : Label -> NavItemViewModel msg
labelToNavItem label =
    { id = LabelId.toString (Label.id label)
    , title = Label.title label
    , iconName = "label"
    , iconSA = StyleAttrs [ c_ <| Css.hsl (Label.hue label |> toFloat) 0.7 0.5 ] []
    , href = Route.href Route.Root
    }


filterToNavItem : FilterView -> NavItemViewModel msg
filterToNavItem { title, hue } =
    { id = title
    , title = title
    , iconName = "filter_list"
    , iconSA = StyleAttrs [ c_ <| Css.hsl hue 0.7 0.5 ] []
    , href = Route.href Route.Root
    }


navTitleIconItem href title iconName =
    viewItem (StyleAttrs [] []) href title iconName (StyleAttrs [ c_inherit ] [])


viewNavItem : StyleAttrs msg -> NavItemViewModel msg -> Html msg
viewNavItem rootSA { title, iconName, iconSA, href } =
    viewItem rootSA href title iconName iconSA


concatStyleAttributes : StyleAttrs msg -> StyleAttrs msg -> StyleAttrs msg
concatStyleAttributes sa1 sa2 =
    StyleAttrs (sa1.styles ++ sa2.styles) (sa1.attrs ++ sa2.attrs)


type alias ColorCompatible x =
    { x | value : String, color : Css.Compatible }


type alias StyleAttrs msg =
    { styles : List Style, attrs : List (Attribute msg) }


type alias IconView msg =
    { name : String
    , styles : List Style
    , attrs : List (Attribute msg)
    }


viewItem : StyleAttrs msg -> Attribute msg -> String -> String -> StyleAttrs msg -> Html msg
viewItem rootSA href title iconName iconSA =
    div
        (css
            [ ph 1
            , pointer
            , flex
            , c_grayL 0.3
            , hover [ bgGrayL 0.9 ]
            , batch rootSA.styles
            ]
            :: rootSA.attrs
        )
        [ i
            ([ css [ pv 2, ph 1, flex, itemsCenter, batch iconSA.styles ]
             , class "material-icons"
             ]
                ++ iconSA.attrs
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
                , mr 3
                ]
            , href
            ]
            [ text title ]
        ]
