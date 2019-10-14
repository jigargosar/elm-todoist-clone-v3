module Drawer exposing (Drawer, Msg, System, system)

import Css
import DnDList
import ExpansionPanel exposing (ExpansionPanel)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes as A exposing (class, css)
import Lens exposing (Lens)
import Project exposing (Project)
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
    , subscriptions = bigL.get >> subscriptions >> Sub.map toMsg
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
    }


initial : Drawer
initial =
    Internal projectsEPS.initial
        labelsEPS.initial
        filtersEPS.initial
        dndSystem.model
        |> Drawer


type Panel
    = Projects
    | Labels
    | Filters


type Msg
    = ExpansionPanel Panel ExpansionPanel.Msg
    | Dnd Panel DnDList.Msg


internalInDrawer =
    Lens (\(Drawer internal) -> internal) (\s _ -> Drawer s)


projectsEPInDrawer =
    Lens.compose internalInDrawer (Lens .projects (\s b -> { b | projects = s }))


labelsEPInDrawer =
    Lens.compose internalInDrawer (Lens .labels (\s b -> { b | labels = s }))


filtersEPInDrawer =
    Lens.compose internalInDrawer (Lens .projects (\s b -> { b | projects = s }))


dndL : Lens DnDList.Model Drawer
dndL =
    Lens.compose internalInDrawer { get = .dnd, set = \s b -> { b | dnd = s } }


projectsEPS : ExpansionPanel.System Msg Drawer
projectsEPS =
    ExpansionPanel.system (ExpansionPanel Projects) projectsEPInDrawer


labelsEPS : ExpansionPanel.System Msg Drawer
labelsEPS =
    ExpansionPanel.system (ExpansionPanel Labels) labelsEPInDrawer


filtersEPS : ExpansionPanel.System Msg Drawer
filtersEPS =
    let
        filtersLens : Lens ExpansionPanel Drawer
        filtersLens =
            Lens.compose internalInDrawer { get = .filters, set = \s b -> { b | filters = s } }
    in
    ExpansionPanel.system (ExpansionPanel Filters) filtersLens


updatePanel : Panel -> ExpansionPanel.Msg -> Drawer -> ( Drawer, Cmd Msg )
updatePanel panel =
    case panel of
        Projects ->
            projectsEPS.update

        Labels ->
            labelsEPS.update

        Filters ->
            filtersEPS.update


updateDnd toMsg onListOrderChanged list msg model =
    let
        oldDnd =
            dndL.get model

        ( dnd, newList ) =
            dndSystem.update msg oldDnd list
    in
    ( dndL.set dnd model
    , Cmd.batch
        [ dndSystem.commands oldDnd |> Cmd.map toMsg
        , onListOrderChanged newList |> Task.succeed |> Task.perform identity
        ]
    )


subscriptions : Drawer -> Sub Msg
subscriptions model =
    Sub.batch [ dndSystem.subscriptions (dndL.get model) ]


update : (Msg -> msg) -> (List Project -> msg) -> List Project -> Msg -> Drawer -> ( Drawer, Cmd msg )
update toMsg updateProjectListOrder projectList message model =
    case message of
        ExpansionPanel panel msg ->
            updatePanel panel msg model
                |> Tuple.mapSecond (Cmd.map toMsg)

        Dnd panel msg ->
            case panel of
                Projects ->
                    updateDnd toMsg updateProjectListOrder projectList msg model

                _ ->
                    ( model, Cmd.none )


view : (Msg -> msg) -> List Project -> Drawer -> List (Html msg)
view toMsg projectList model =
    [ navIconItem "Inbox" "inbox"
    , navIconItem "Today" "calendar_today"
    , navIconItem "Next 7 Days" "view_week"
    , viewProjectsExpansionPanel projectList model
    , labelsEPS.view
        "Labels"
        [ navLabelItem "to read" 333
        , navLabelItem "medical" 93990
        , navLabelItem "quick-ref" 444
        ]
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
            dndL.get model
    in
    projectsEPS.view
        "Projects"
        ((rotateDragged dnd projectList
            |> List.indexedMap (navProjectItem dnd)
         )
            ++ [ viewGhostItem projectList model ]
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


viewGhostItem : List Project -> Drawer -> Html Msg
viewGhostItem projectList model =
    let
        dnd =
            dndL.get model
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


navLabelItem title hue =
    navItem title (Css.hsl hue 0.7 0.5) "label"


navFilterItem title hue =
    navItem title (Css.hsl hue 0.7 0.5) "filter_list"
