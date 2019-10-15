module Drawer exposing (Drawer, Msg, System, system)

import Css
import DnD exposing (DnD)
import ExpansionPanel exposing (ExpansionPanel)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (class, css)
import Lens exposing (Lens)
import Project exposing (Project)
import ProjectId
import Return
import Styles exposing (..)
import Task


type alias System msg big =
    { initial : Drawer
    , update : Msg -> big -> ( big, Cmd msg )
    , view : big -> { content : List (Html msg), portal : List (Html msg) }
    , subscriptions : big -> Sub msg
    }


system :
    (Msg -> msg)
    -> { onProjectListSorted : List Project -> msg }
    -> (big -> List Project)
    -> Lens Drawer big
    -> System msg big
system toMsg { onProjectListSorted } getProjectList bigL =
    { initial = initial
    , update =
        \msg big ->
            Lens.update bigL (update toMsg onProjectListSorted (getProjectList big) msg) big
    , view = \big -> view toMsg (getProjectList big) (bigL.get big)
    , subscriptions = bigL.get >> subscriptions toMsg
    }



--


type Drawer
    = Drawer Internal


type alias ExpansionPanels =
    { projects : ExpansionPanel
    , labels : ExpansionPanel
    , filters : ExpansionPanel
    }


type alias DndPanels =
    { projects : DnD
    , labels : DnD
    , filters : DnD
    }


type alias Internal =
    { expansionPanels : ExpansionPanels
    , dndPanels : DndPanels
    , labelList : List LabelView
    , filterList : List FilterView
    }


initial : Drawer
initial =
    Internal
        { projects = projectsEPS.initial
        , labels = labelsEPS.initial
        , filters = filtersEPS.initial
        }
        { projects = projectsDnDSystem.initial
        , labels = labelsDnDSystem.initial
        , filters = filtersDnDSystem.initial
        }
        [ LabelView "to read" 333
        , LabelView "medical" 93990
        , LabelView "quick-ref" 444
        ]
        [ FilterView "Assigned to me" 933
        , FilterView "Assigned to others" 9354
        , FilterView "Priority 1" 93344
        , FilterView "Priority 2" 932323
        , FilterView "Priority 3" 932323
        , FilterView "View all" 932325
        , FilterView "No due date" 9355
        ]
        |> Drawer


type Panel
    = Projects
    | Labels
    | Filters


type Msg
    = ExpansionPanel Panel ExpansionPanel.Msg
    | DndPanel Panel DnD.Msg
    | DnDCommit Panel DnD.Info


internalLens : Lens Internal Drawer
internalLens =
    Lens (\(Drawer internal) -> internal) (\s _ -> Drawer s)


labelsLens : Lens (List LabelView) Drawer
labelsLens =
    Lens.compose internalLens
        (Lens .labelList (\s b -> { b | labelList = s }))


filtersLens =
    Lens.compose internalLens
        (Lens .filterList (\s b -> { b | filterList = s }))


expansionPanelsLens : Lens ExpansionPanels Drawer
expansionPanelsLens =
    Lens.compose internalLens (Lens .expansionPanels (\s b -> { b | expansionPanels = s }))


expansionPanelLens : Panel -> Lens ExpansionPanel Drawer
expansionPanelLens panel =
    case panel of
        Projects ->
            Lens.compose expansionPanelsLens (Lens .projects (\s b -> { b | projects = s }))

        Labels ->
            Lens.compose expansionPanelsLens (Lens .labels (\s b -> { b | labels = s }))

        Filters ->
            Lens.compose expansionPanelsLens (Lens .filters (\s b -> { b | filters = s }))


expansionPanelSystem : Panel -> ExpansionPanel.System Msg Drawer
expansionPanelSystem panel =
    ExpansionPanel.system (ExpansionPanel panel) (expansionPanelLens panel)


dndPanelsLens : Lens DndPanels Drawer
dndPanelsLens =
    Lens.compose internalLens (Lens .dndPanels (\s b -> { b | dndPanels = s }))


dndPanelLens : Panel -> Lens DnD Drawer
dndPanelLens panel =
    case panel of
        Projects ->
            Lens.compose dndPanelsLens (Lens .projects (\s b -> { b | projects = s }))

        Labels ->
            Lens.compose dndPanelsLens (Lens .labels (\s b -> { b | labels = s }))

        Filters ->
            Lens.compose dndPanelsLens (Lens .filters (\s b -> { b | filters = s }))


dndPanelSystem : Panel -> DnD.System a Msg Drawer
dndPanelSystem panel =
    DnD.create (DndPanel panel) { onCommit = DnDCommit panel } (dndPanelLens panel)


labelsDnDSystem : DnD.System LabelView Msg Drawer
labelsDnDSystem =
    dndPanelSystem Labels


projectsDnDSystem : DnD.System Project Msg Drawer
projectsDnDSystem =
    dndPanelSystem Projects


filtersDnDSystem : DnD.System FilterView Msg Drawer
filtersDnDSystem =
    dndPanelSystem Filters


projectsEPS : ExpansionPanel.System Msg Drawer
projectsEPS =
    expansionPanelSystem Projects


labelsEPS : ExpansionPanel.System Msg Drawer
labelsEPS =
    expansionPanelSystem Labels


filtersEPS : ExpansionPanel.System Msg Drawer
filtersEPS =
    expansionPanelSystem Filters


subscriptions : (Msg -> msg) -> Drawer -> Sub msg
subscriptions toMsg model =
    Sub.batch
        [ projectsDnDSystem.subscriptions model
        , labelsDnDSystem.subscriptions model
        , filtersDnDSystem.subscriptions model
        ]
        |> Sub.map toMsg


update : (Msg -> msg) -> (List Project -> msg) -> List Project -> Msg -> Drawer -> ( Drawer, Cmd msg )
update toMsg updateProjectListOrder projectList message =
    case message of
        ExpansionPanel panel msg ->
            (expansionPanelSystem panel).update msg
                >> Return.mapCmd toMsg

        DndPanel panel msg ->
            (dndPanelSystem panel).update msg >> Return.mapCmd toMsg

        DnDCommit panel info ->
            case panel of
                Projects ->
                    Return.singleton
                        >> Return.command (updateProjectListOrder (DnD.rotateFromInfo info projectList) |> perform)

                Labels ->
                    (\m -> labelsLens.set (DnD.rotateFromInfo info (labelsLens.get m)) m)
                        >> Return.singleton

                Filters ->
                    (\m -> filtersLens.set (DnD.rotateFromInfo info (filtersLens.get m)) m)
                        >> Return.singleton


perform =
    Task.succeed >> Task.perform identity


type alias LabelView =
    { title : String, hue : Float }


type alias FilterView =
    { title : String, hue : Float }


view : (Msg -> msg) -> List Project -> Drawer -> { content : List (Html msg), portal : List (Html msg) }
view toMsg projectList model =
    { content =
        [ navIconItem "Inbox" "inbox"
        , navIconItem "Today" "calendar_today"
        , navIconItem "Next 7 Days" "view_week"
        , projectsEPS.view
            "Projects"
            (projectList
                |> projectsDnDSystem.rotate model
                |> List.indexedMap (navProjectItem model)
            )
            model
        , labelsEPS.view
            "Labels"
            (labelsLens.get model
                |> labelsDnDSystem.rotate model
                |> List.indexedMap (navLabelItem model)
            )
            model
        , filtersEPS.view
            "Filters"
            (filtersLens.get model
                |> filtersDnDSystem.rotate model
                |> List.indexedMap (navFilterItem model)
            )
            model
        ]
            |> List.map (H.map toMsg)
    , portal =
        navProjectGhostItem projectList model
            ++ navLabelGhostItem (labelsLens.get model) model
            ++ navFilterGhostItem (filtersLens.get model) model
            |> List.map (H.map toMsg)
    }


navItem title iconColor iconName =
    div [ css [ ph 1, pointer, flex, c_grayL 0.3 ] ]
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


navIconItem title icon =
    navItem title Css.inherit icon


viewItem2 attributes styles title iconColor iconName =
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


navProjectItem : Drawer -> Int -> Project -> Html Msg
navProjectItem model idx project =
    let
        info =
            projectsDnDSystem.info model

        domId =
            "project-dnd-element__" ++ (Project.id project |> ProjectId.toString)

        ( attrs, styles ) =
            case info of
                Nothing ->
                    ( projectsDnDSystem.dragEvents idx domId, [] )

                Just { drop } ->
                    ( projectsDnDSystem.dropEvents idx domId
                    , if drop.index == idx then
                        [ Css.opacity <| Css.num 0 ]

                      else
                        []
                    )

        title =
            Project.title project

        iconColor =
            Css.hsl (Project.hue project |> toFloat) 0.7 0.5
    in
    viewItem2 (A.id domId :: attrs) styles title iconColor "folder"


navLabelItem : Drawer -> Int -> LabelView -> Html Msg
navLabelItem model idx { title, hue } =
    let
        info =
            labelsDnDSystem.info model

        domId =
            "label-dnd-element__" ++ title ++ "__" ++ String.fromInt idx

        ( attrs, styles ) =
            case info of
                Nothing ->
                    ( labelsDnDSystem.dragEvents idx domId, [] )

                Just { drop } ->
                    ( labelsDnDSystem.dropEvents idx domId
                    , if drop.index == idx then
                        [ Css.opacity <| Css.num 0 ]

                      else
                        []
                    )
    in
    viewItem2 (A.id domId :: attrs) styles title (Css.hsl hue 0.7 0.5) "label"


navFilterItem : Drawer -> Int -> FilterView -> Html Msg
navFilterItem model idx { title, hue } =
    let
        info =
            filtersDnDSystem.info model

        domId =
            "filter-dnd-element__" ++ title ++ "__" ++ String.fromInt idx

        ( attrs, styles ) =
            case info of
                Nothing ->
                    ( filtersDnDSystem.dragEvents idx domId, [] )

                Just { drop } ->
                    ( filtersDnDSystem.dropEvents idx domId
                    , if drop.index == idx then
                        [ Css.opacity <| Css.num 0 ]

                      else
                        []
                    )
    in
    viewItem2 (A.id domId :: attrs) styles title (Css.hsl hue 0.7 0.5) "filter_list"


maybeDrag2Item dnd2Sys model items =
    dnd2Sys.info model
        |> Maybe.andThen
            (\{ drag } ->
                items
                    |> List.drop drag.index
                    |> List.head
            )


navProjectGhostItem projectList model =
    maybeDrag2Item projectsDnDSystem model projectList
        |> Maybe.map
            (\project ->
                [ let
                    attrs =
                        [ css [ projectsDnDSystem.ghostStyles model ] ]

                    title =
                        Project.title project

                    iconColor =
                        Css.hsl (Project.hue project |> toFloat) 0.7 0.5
                  in
                  viewItem2 attrs [] title iconColor "label"
                ]
            )
        |> Maybe.withDefault []


navLabelGhostItem labels model =
    maybeDrag2Item labelsDnDSystem model labels
        |> Maybe.map
            (\{ title, hue } ->
                [ let
                    attrs =
                        [ css [ labelsDnDSystem.ghostStyles model ] ]
                  in
                  viewItem2 attrs [] title (Css.hsl hue 0.7 0.5) "label"
                ]
            )
        |> Maybe.withDefault []


navFilterGhostItem filters model =
    maybeDrag2Item filtersDnDSystem model filters
        |> Maybe.map
            (\{ title, hue } ->
                [ let
                    attrs =
                        [ css [ filtersDnDSystem.ghostStyles model ] ]
                  in
                  viewItem2 attrs [] title (Css.hsl hue 0.7 0.5) "filter_list"
                ]
            )
        |> Maybe.withDefault []
