module Drawer exposing (Drawer, Msg, System, system)

import Css
import DnD exposing (DnD)
import DnDList
import ExpansionPanel exposing (ExpansionPanel)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (class, css)
import Lens exposing (Lens)
import Project exposing (Project)
import Return
import SelectList
import Styles exposing (..)
import Task


type alias System msg big =
    { initial : Drawer
    , update : Msg -> big -> ( big, Cmd msg )
    , view : big -> List (Html msg)
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
    , dnd2 : DnD
    }


initial : Drawer
initial =
    Internal projectsEPS.initial
        labelsEPS.initial
        filtersEPS.initial
        dndSystem.model
        dnd2System.initial
        |> Drawer


type Panel
    = Projects
    | Labels
    | Filters


type Msg
    = ExpansionPanel Panel ExpansionPanel.Msg
    | Dnd Panel DnDList.Msg
    | DndMsg DnD.Msg


internalLens =
    Lens (\(Drawer internal) -> internal) (\s _ -> Drawer s)


expansionPanelLens panel =
    case panel of
        Projects ->
            Lens.compose internalLens (Lens .projects (\s b -> { b | projects = s }))

        Labels ->
            Lens.compose internalLens (Lens .labels (\s b -> { b | labels = s }))

        Filters ->
            Lens.compose internalLens (Lens .projects (\s b -> { b | projects = s }))


expansionPanelSystem panel =
    ExpansionPanel.system (ExpansionPanel panel) (expansionPanelLens panel)


dndLens : Lens DnDList.Model Drawer
dndLens =
    Lens.compose internalLens (Lens .dnd (\s b -> { b | dnd = s }))


dnd2Lens : Lens DnD Drawer
dnd2Lens =
    Lens.compose internalLens (Lens .dnd2 (\s b -> { b | dnd2 = s }))


dnd2System : DnD.System Msg Drawer
dnd2System =
    DnD.create DndMsg dnd2Lens


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
        , dnd2System.subscriptions model
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

        DndMsg msg ->
            dnd2System.update msg
                >> Return.mapCmd toMsg


type alias LabelView =
    { title : String, hue : Float }


labelList =
    [ LabelView "to read" 333
    , LabelView "medical" 93990
    , LabelView "quick-ref" 444
    ]


view : (Msg -> msg) -> List Project -> Drawer -> List (Html msg)
view toMsg projectList model =
    [ navIconItem "Inbox" "inbox"
    , navIconItem "Today" "calendar_today"
    , navIconItem "Next 7 Days" "view_week"
    , viewProjectsExpansionPanel projectList model
    , labelsEPS.view
        "Labels"
        (List.indexedMap navLabelItem labelList
            ++ navLabelGhostItem labelList model
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
        ((rotateDragged dnd projectList
            |> List.indexedMap (navProjectItem dnd)
         )
            ++ [ viewProjectGhostItem projectList model ]
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


navLabelItem : Int -> LabelView -> Html Msg
navLabelItem idx { title, hue } =
    let
        domId =
            "label-dnd-element__" ++ String.fromInt idx

        attrs =
            A.id domId
                :: dnd2System.dragEvents domId
                ++ dnd2System.dropEvents domId
    in
    viewItem2 attrs [] title (Css.hsl hue 0.7 0.5) "label"


maybeDrag2Item drawer items =
    dnd2System.info drawer
        |> Maybe.andThen
            (\{ dragElementId } ->
                String.replace "label-dnd-element__" "" dragElementId
                    |> String.toInt
                    |> Maybe.andThen
                        (\dragIndex ->
                            items
                                |> List.drop dragIndex
                                |> List.head
                        )
            )


navLabelGhostItem labels model =
    maybeDrag2Item model labels
        |> Maybe.map (\_ -> text "")
        |> Maybe.withDefault (text "")


navFilterItem title hue =
    navItem title (Css.hsl hue 0.7 0.5) "filter_list"
