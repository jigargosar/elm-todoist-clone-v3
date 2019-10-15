module Drawer exposing (Drawer, Msg, System, system)

import Basics.More exposing (flip)
import Css
import DnD exposing (DnD)
import DnDList
import ExpansionPanel exposing (ExpansionPanel)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (class, css)
import Lens exposing (Lens)
import Project exposing (Project)
import ProjectId
import Return
import SelectList
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


dndSystem : DnDList.System a Msg
dndSystem =
    DnDList.create
        { beforeUpdate = \_ _ list -> list
        , movement = DnDList.Vertical
        , listen = DnDList.OnDrop
        , operation = DnDList.Rotate
        }
        (Dnd Projects)


type Drawer
    = Drawer Internal


type alias Internal =
    { projects : ExpansionPanel
    , labels : ExpansionPanel
    , filters : ExpansionPanel
    , dnd : DnDList.Model
    , dndProjects : DnD
    , dndLabels : DnD
    , dndFilters : DnD
    }


initial : Drawer
initial =
    Internal projectsEPS.initial
        labelsEPS.initial
        filtersEPS.initial
        dndSystem.model
        dndLabelsSystem.initial
        dndProjectsSystem.initial
        dndFiltersSystem.initial
        |> Drawer


type Panel
    = Projects
    | Labels
    | Filters


type Msg
    = ExpansionPanel Panel ExpansionPanel.Msg
    | Dnd Panel DnDList.Msg
    | DndMsg Panel DnD.Msg
    | DnDCommit Panel DnD.Info


internalLens =
    Lens (\(Drawer internal) -> internal) (\s _ -> Drawer s)


expansionPanelLens panel =
    case panel of
        Projects ->
            Lens.compose internalLens (Lens .projects (\s b -> { b | projects = s }))

        Labels ->
            Lens.compose internalLens (Lens .labels (\s b -> { b | labels = s }))

        Filters ->
            Lens.compose internalLens (Lens .filters (\s b -> { b | filters = s }))


expansionPanelSystem panel =
    ExpansionPanel.system (ExpansionPanel panel) (expansionPanelLens panel)


dndLens : Lens DnDList.Model Drawer
dndLens =
    Lens.compose internalLens (Lens .dnd (\s b -> { b | dnd = s }))


dnd2Lens : Panel -> Lens DnD Drawer
dnd2Lens panel =
    case panel of
        Projects ->
            Lens.compose internalLens (Lens .dndProjects (\s b -> { b | dndProjects = s }))

        Labels ->
            Lens.compose internalLens (Lens .dndLabels (\s b -> { b | dndLabels = s }))

        Filters ->
            Lens.compose internalLens (Lens .dndFilters (\s b -> { b | dndFilters = s }))


dnd2System panel =
    DnD.create (DndMsg panel) { onCommit = DnDCommit panel } (dnd2Lens panel)


dndLabelsSystem : DnD.System Msg Drawer
dndLabelsSystem =
    dnd2System Labels


dndProjectsSystem : DnD.System Msg Drawer
dndProjectsSystem =
    dnd2System Projects


dndFiltersSystem : DnD.System Msg Drawer
dndFiltersSystem =
    dnd2System Filters


projectsEPS : ExpansionPanel.System Msg Drawer
projectsEPS =
    expansionPanelSystem Projects


labelsEPS : ExpansionPanel.System Msg Drawer
labelsEPS =
    expansionPanelSystem Labels


filtersEPS : ExpansionPanel.System Msg Drawer
filtersEPS =
    expansionPanelSystem Filters


updateDnd toMsg onListOrderChanged list msg model =
    let
        oldDnd =
            dndLens.get model

        ( dnd, newList ) =
            dndSystem.update msg oldDnd list
    in
    ( dndLens.set dnd model
    , Cmd.batch
        [ dndSystem.commands oldDnd |> Cmd.map toMsg
        , onListOrderChanged newList |> Task.succeed |> Task.perform identity
        ]
    )


subscriptions : (Msg -> msg) -> Drawer -> Sub msg
subscriptions toMsg model =
    Sub.batch
        [ dndSystem.subscriptions (dndLens.get model)
        , dndLabelsSystem.subscriptions model
        ]
        |> Sub.map toMsg


update : (Msg -> msg) -> (List Project -> msg) -> List Project -> Msg -> Drawer -> ( Drawer, Cmd msg )
update toMsg updateProjectListOrder projectList message =
    case message of
        ExpansionPanel panel msg ->
            (expansionPanelSystem panel).update msg
                >> Return.mapCmd toMsg

        Dnd panel msg ->
            case panel of
                Projects ->
                    updateDnd toMsg updateProjectListOrder projectList msg

                _ ->
                    Return.singleton

        DndMsg panel msg ->
            (dnd2System panel).update msg >> Return.mapCmd toMsg

        DnDCommit panel info ->
            Return.singleton
                >> Return.command
                    (case panel of
                        Projects ->
                            updateProjectListOrder (DnD.rotate info projectList) |> perform

                        _ ->
                            Cmd.none
                    )


perform =
    Task.succeed >> Task.perform identity


type alias LabelView =
    { title : String, hue : Float }


labelList =
    [ LabelView "to read" 333
    , LabelView "medical" 93990
    , LabelView "quick-ref" 444
    ]


view : (Msg -> msg) -> List Project -> Drawer -> { content : List (Html msg), portal : List (Html msg) }
view toMsg projectList model =
    { content =
        [ navIconItem "Inbox" "inbox"
        , navIconItem "Today" "calendar_today"
        , navIconItem "Next 7 Days" "view_week"
        , viewProjectsExpansionPanel projectList model
        , labelsEPS.view
            "Projects"
            (projectList
                |> (dndProjectsSystem.info model |> Maybe.map DnD.rotate |> Maybe.withDefault identity)
                |> List.indexedMap (navProject2Item model)
            )
            model
        , labelsEPS.view
            "Labels"
            (labelList
                |> (dndLabelsSystem.info model |> Maybe.map DnD.rotate |> Maybe.withDefault identity)
                |> List.indexedMap (navLabelItem model)
            )
            model
        , filtersEPS.view
            "Filters"
            [ navFilterItem "Assigned to me" 933
            , navFilterItem "Assigned to others" 9354
            , navFilterItem "Priority 1" 93344
            , navFilterItem "Priority 2" 932323
            , navFilterItem "Priority 3" 932323
            , navFilterItem "View all" 932325
            , navFilterItem "No due date" 9355
            ]
            model
        ]
            |> List.map (H.map toMsg)
    , portal =
        navLabelGhostItem labelList model
            ++ [ viewProjectGhostItem projectList model ]
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


rotateDragged dnd list =
    case dndSystem.info dnd of
        Just { dragIndex, dropIndex } ->
            SelectList.fromList list
                |> Maybe.andThen (SelectList.selectBy dragIndex)
                |> Maybe.map (SelectList.moveBy (dropIndex - dragIndex) >> SelectList.toList)
                |> Maybe.withDefault list

        Nothing ->
            list


viewProjectsExpansionPanel projectList model =
    let
        dnd =
            dndLens.get model
    in
    projectsEPS.view
        "Projects"
        (rotateDragged dnd projectList
            |> List.indexedMap (navProjectItem dnd)
        )
        model


maybeDragItem : DnDList.Model -> List a -> Maybe a
maybeDragItem dnd items =
    dndSystem.info dnd
        |> Maybe.andThen
            (\{ dragIndex } ->
                items
                    |> List.drop dragIndex
                    |> List.head
            )


viewProjectGhostItem : List Project -> Drawer -> Html Msg
viewProjectGhostItem projectList model =
    let
        dnd =
            dndLens.get model
    in
    case maybeDragItem dnd projectList of
        Just project ->
            let
                iconColor =
                    Css.hsl (Project.hue project |> toFloat) 0.7 0.5

                title =
                    Project.title project

                ghostStyles =
                    dndSystem.ghostStyles dnd |> List.map A.fromUnstyled
            in
            viewItem2 ghostStyles [] title iconColor "folder"

        Nothing ->
            text ""


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


navProjectItem dnd sortIdx project =
    let
        domId =
            String.fromInt sortIdx

        dragEvents =
            dndSystem.dragEvents sortIdx domId
                |> List.map A.fromUnstyled

        dropEvents =
            dndSystem.dropEvents sortIdx domId
                |> List.map A.fromUnstyled

        info =
            dndSystem.info dnd

        styles =
            info
                |> Maybe.andThen
                    (\i ->
                        if sortIdx == i.dropIndex then
                            Just [ Css.opacity <| Css.num 0 ]

                        else
                            Nothing
                    )
                |> Maybe.withDefault []

        attributes =
            A.id domId
                :: (case info of
                        Just _ ->
                            dropEvents

                        Nothing ->
                            dragEvents
                   )

        title =
            Project.title project

        iconColor =
            Css.hsl (Project.hue project |> toFloat) 0.7 0.5
    in
    viewItem2 attributes styles title iconColor "folder"


navLabelItem : Drawer -> Int -> LabelView -> Html Msg
navLabelItem model idx { title, hue } =
    let
        info =
            dndLabelsSystem.info model

        domId =
            "label-dnd-element__" ++ title ++ "__" ++ String.fromInt idx

        ( attrs, styles ) =
            case info of
                Nothing ->
                    ( dndLabelsSystem.dragEvents idx domId, [] )

                Just { drop } ->
                    ( dndLabelsSystem.dropEvents idx domId
                    , if drop.index == idx then
                        [ Css.opacity <| Css.num 0 ]

                      else
                        []
                    )
    in
    viewItem2 (A.id domId :: attrs) styles title (Css.hsl hue 0.7 0.5) "label"


navProject2Item : Drawer -> Int -> Project -> Html Msg
navProject2Item model idx project =
    let
        info =
            dndProjectsSystem.info model

        domId =
            "project-dnd-element__" ++ (Project.id project |> ProjectId.toString)

        ( attrs, styles ) =
            case info of
                Nothing ->
                    ( dndProjectsSystem.dragEvents idx domId, [] )

                Just { drop } ->
                    ( dndProjectsSystem.dropEvents idx domId
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


maybeDrag2Item drawer items =
    dndLabelsSystem.info drawer
        |> Maybe.andThen
            (\{ drag } ->
                items
                    |> List.drop drag.index
                    |> List.head
            )


navLabelGhostItem labels model =
    maybeDrag2Item model labels
        |> Maybe.map
            (\{ title, hue } ->
                [ let
                    attrs =
                        [ css [ dndLabelsSystem.ghostStyles model ] ]
                  in
                  viewItem2 attrs [] title (Css.hsl hue 0.7 0.5) "label"
                ]
            )
        |> Maybe.withDefault []


navFilterItem title hue =
    navItem title (Css.hsl hue 0.7 0.5) "filter_list"
