module DrawerOldS exposing (Drawer, Msg, System, system)

import Css
import DnD exposing (DnD)
import Drag exposing (Drag)
import ExpansionPanelUI
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


type Drawer
    = Drawer Internal


type alias DndPanels =
    { projects : DnD
    , labels : DnD
    , filters : DnD
    }


type alias ExpansionPanelsState =
    { projects : Bool
    , labels : Bool
    , filters : Bool
    }


type alias Internal =
    { expansionPanelsState : ExpansionPanelsState
    , dndPanels : DndPanels
    , labelList : List LabelView
    , filterList : List FilterView
    , drag : ( Panel, Drag )
    }


initial : Drawer
initial =
    Internal
        (ExpansionPanelsState True True True)
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
        ( Projects, Drag.initial )
        |> Drawer


type Panel
    = Projects
    | Labels
    | Filters


type Msg
    = ToggleExpansionPanel Panel Bool
    | DndPanel Panel DnD.Msg
    | DnDCommit Panel DnD.Info
    | Drag Panel Drag.Msg


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


subscriptions : (Msg -> msg) -> Drawer -> Sub msg
subscriptions toMsg model =
    Sub.batch
        [ projectsDnDSystem.subscriptions model
        , labelsDnDSystem.subscriptions model
        , filtersDnDSystem.subscriptions model
        , case unwrap model |> .drag of
            ( p, d ) ->
                Drag.subscriptions d |> Sub.map (Drag p)
        ]
        |> Sub.map toMsg


update : (Msg -> msg) -> (List Project -> msg) -> List Project -> Msg -> Drawer -> ( Drawer, Cmd msg )
update toMsg updateProjectListOrder projectList message ((Drawer internal) as model) =
    case message of
        DndPanel panel msg ->
            (dndPanelSystem panel).update msg model
                |> Return.mapCmd toMsg

        DnDCommit panel info ->
            case panel of
                Projects ->
                    ( model
                    , updateProjectListOrder (DnD.rotateFromInfo info projectList) |> perform
                    )

                Labels ->
                    ( labelsLens.set (DnD.rotateFromInfo info (labelsLens.get model)) model
                    , Cmd.none
                    )

                Filters ->
                    ( filtersLens.set (DnD.rotateFromInfo info (filtersLens.get model)) model
                    , Cmd.none
                    )

        ToggleExpansionPanel panel bool ->
            ( mapExpansionPanelsState ((panelSystem panel).set bool) model
            , Cmd.none
            )

        Drag panel msg ->
            let
                prevDrag =
                    case internal.drag of
                        ( prevPanel, prevDrag_ ) ->
                            if prevPanel == panel then
                                prevDrag_

                            else
                                Drag.initial

                ( nextDrag, dragCmd ) =
                    Drag.update (Drag panel) msg prevDrag
            in
            ( { internal | drag = ( panel, nextDrag ) } |> Drawer
            , dragCmd |> Cmd.map toMsg
            )


perform =
    Task.succeed >> Task.perform identity


type alias LabelView =
    { title : String, hue : Float }


type alias FilterView =
    { title : String, hue : Float }


unwrap (Drawer internal) =
    internal


map : (Internal -> Internal) -> Drawer -> Drawer
map func =
    unwrap >> func >> Drawer


mapExpansionPanelsState : (ExpansionPanelsState -> ExpansionPanelsState) -> Drawer -> Drawer
mapExpansionPanelsState func =
    map (\i -> { i | expansionPanelsState = func i.expansionPanelsState })


type alias PanelSystem =
    { get : ExpansionPanelsState -> Bool
    , set :
        Bool
        -> ExpansionPanelsState
        -> ExpansionPanelsState
    , title : String
    }


panelSystem : Panel -> PanelSystem
panelSystem panel =
    case panel of
        Projects ->
            PanelSystem .projects (\s b -> { b | projects = s }) "Projects"

        Labels ->
            PanelSystem .labels (\s b -> { b | labels = s }) "Labels"

        Filters ->
            PanelSystem .filters (\s b -> { b | filters = s }) "Filters"


viewExpansionPanel panel lazyContent expansionPanelsState =
    let
        { get, title } =
            panelSystem panel
    in
    ExpansionPanelUI.view (ToggleExpansionPanel panel)
        title
        lazyContent
        (get expansionPanelsState)


view : (Msg -> msg) -> List Project -> Drawer -> { content : List (Html msg), portal : List (Html msg) }
view toMsg projectList ((Drawer internal) as model) =
    { content =
        [ navIconItem "Inbox" "inbox"
        , navIconItem "Today" "calendar_today"
        , navIconItem "Next 7 Days" "view_week"
        ]
            ++ viewExpansionPanel
                Projects
                (\_ ->
                    projectList
                        |> projectsDnDSystem.rotate model
                        |> List.indexedMap (navProjectItem model)
                )
                internal.expansionPanelsState
            ++ viewExpansionPanel
                Labels
                (\_ ->
                    labelsLens.get model
                        |> labelsDnDSystem.rotate model
                        |> List.indexedMap (navLabelItem model)
                )
                internal.expansionPanelsState
            ++ viewExpansionPanel Filters
                (\_ ->
                    filtersLens.get model
                        |> filtersDnDSystem.rotate model
                        |> List.indexedMap (navFilterItem model)
                )
                internal.expansionPanelsState
            ++ viewExpansionPanel Filters
                (\_ ->
                    viewFiltersContent (getDragFor Filters internal.drag) internal.filterList
                )
                internal.expansionPanelsState
            |> List.map (H.map toMsg)
    , portal =
        navProjectGhostItem projectList model
            ++ navLabelGhostItem (labelsLens.get model) model
            ++ navFilterGhostItem (filtersLens.get model) model
            |> List.map (H.map toMsg)
    }


viewFilters panelTitle filterList internal =
    let
        isExpanded =
            internal.expansionPanelsState.filters

        toggleExpanded =
            ToggleExpansionPanel Filters

        drag =
            internal.drag
                |> (\( panel, drag_ ) ->
                        if panel == Filters then
                            drag_

                        else
                            Drag.initial
                   )

        viewFilterItem : Int -> FilterView -> Html Msg
        viewFilterItem idx { title, hue } =
            let
                styles =
                    if Drag.dropIdxEq idx drag then
                        [ Css.opacity Css.zero ]

                    else
                        []

                domId =
                    "filter-item-drag-el__" ++ title
            in
            viewItem
                (A.id domId
                    :: Drag.dragEvents (Drag Filters) idx domId drag
                    ++ Drag.dropEvents (Drag Filters) idx domId drag
                )
                styles
                title
                (Css.hsl hue 0.7 0.5)
                "filter_list"

        ghostItem =
            maybeDrag2Item drag filterList
                |> Maybe.map
                    (\{ title, hue } ->
                        [ let
                            attrs =
                                [ css [ Drag.ghostStyles drag ] ]
                          in
                          viewItem attrs [] title (Css.hsl hue 0.7 0.5) "filter_list"
                        ]
                    )
                |> Maybe.withDefault []
    in
    { content =
        ExpansionPanelUI.view toggleExpanded
            panelTitle
            (\_ ->
                filterList
                    |> sort drag
                    |> List.indexedMap viewFilterItem
            )
            isExpanded
    , portal = ghostItem
    }


viewFiltersContent drag filterList =
    filterList
        |> sort drag
        |> List.indexedMap (navFilterItem2 drag)


getDragFor : a -> ( a, Drag ) -> Drag
getDragFor panel ( currentPanel, drag ) =
    if panel == currentPanel then
        drag

    else
        Drag.initial


sort drag list =
    case Drag.dragIdxInfo drag of
        Nothing ->
            list

        Just { dragIdx, dropIdx } ->
            let
                newList =
                    SelectList.fromList list
                        |> Maybe.andThen (SelectList.selectBy dragIdx)
                        |> Maybe.map (SelectList.moveBy (dropIdx - dragIdx) >> SelectList.toList)
                        |> Maybe.withDefault list
            in
            newList


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
    viewItem (A.id domId :: attrs) styles title iconColor "folder"


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
    viewItem (A.id domId :: attrs) styles title (Css.hsl hue 0.7 0.5) "label"


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
    viewItem (A.id domId :: attrs) styles title (Css.hsl hue 0.7 0.5) "filter_list"


navFilterItem2 : Drag -> Int -> FilterView -> Html Msg
navFilterItem2 drag idx { title, hue } =
    let
        styles =
            case Drag.dragIdxInfo drag of
                Nothing ->
                    []

                Just { dropIdx } ->
                    if dropIdx == idx then
                        [ Css.opacity Css.zero ]

                    else
                        []

        domId =
            "filter-item-drag-el__" ++ title
    in
    viewItem
        (A.id domId
            :: Drag.dragEvents (Drag Filters) idx domId drag
            ++ Drag.dropEvents (Drag Filters) idx domId drag
        )
        styles
        title
        (Css.hsl hue 0.7 0.5)
        "filter_list"


maybeDragItem dndSys model items =
    dndSys.info model
        |> Maybe.andThen
            (\{ drag } ->
                items
                    |> List.drop drag.index
                    |> List.head
            )


navProjectGhostItem projectList model =
    maybeDragItem projectsDnDSystem model projectList
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
                  viewItem attrs [] title iconColor "label"
                ]
            )
        |> Maybe.withDefault []


navLabelGhostItem labels model =
    maybeDragItem labelsDnDSystem model labels
        |> Maybe.map
            (\{ title, hue } ->
                [ let
                    attrs =
                        [ css [ labelsDnDSystem.ghostStyles model ] ]
                  in
                  viewItem attrs [] title (Css.hsl hue 0.7 0.5) "label"
                ]
            )
        |> Maybe.withDefault []


navFilterGhostItem filters model =
    maybeDragItem filtersDnDSystem model filters
        |> Maybe.map
            (\{ title, hue } ->
                [ let
                    attrs =
                        [ css [ filtersDnDSystem.ghostStyles model ] ]
                  in
                  viewItem attrs [] title (Css.hsl hue 0.7 0.5) "filter_list"
                ]
            )
        |> Maybe.withDefault []


maybeDrag2Item drag items =
    Drag.dragIdxInfo drag
        |> Maybe.andThen
            (\{ dragIdx } ->
                items
                    |> List.drop dragIdx
                    |> List.head
            )


navFilter2GhostItem drag filterList =
    maybeDrag2Item drag filterList
        |> Maybe.map
            (\{ title, hue } ->
                [ let
                    attrs =
                        [ css [ Drag.ghostStyles drag ] ]
                  in
                  viewItem attrs [] title (Css.hsl hue 0.7 0.5) "filter_list"
                ]
            )
        |> Maybe.withDefault []