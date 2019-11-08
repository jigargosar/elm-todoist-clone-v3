module Main exposing (main)

import Appbar
import Basics.More exposing (flip)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import DB exposing (DB)
import Dialog exposing (Dialog)
import Dialog.AddProject
import Dialog.EditProject
import Drawer
import Filter exposing (Filter)
import FilterCollection as FC exposing (FilterCollection)
import FilterId exposing (FilterId)
import FilterPanel exposing (FilterPanel)
import Html.Styled as H exposing (Attribute, Html, div, text, toUnstyled)
import InboxOrProject exposing (InboxOrProject)
import Json.Encode exposing (Value)
import Label exposing (Label)
import LabelCollection as LC exposing (LabelCollection)
import LabelId exposing (LabelId)
import LabelPanel exposing (LabelPanel)
import Layout
import Log exposing (logDecodeError, logError)
import Page.NotFound
import Popper exposing (Popper)
import PopupView
import Project exposing (Project)
import ProjectCollection as PC exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import ProjectPanel exposing (ProjectPanel)
import Random
import Return as Ret
import Route exposing (Route)
import Task
import Time
import Timestamp exposing (Timestamp)
import Todo exposing (Todo)
import TodoCollection as TC exposing (TodoCollection)
import TodoId exposing (TodoId)
import TodoProject
import TodoUI
import Url exposing (Url)



-- DIALOG


dialog :
    { openAddProject : Int -> { a | dialog : Dialog } -> ( { a | dialog : Dialog }, Cmd Msg )
    , openEditProject : Project -> { a | dialog : Dialog } -> ( { a | dialog : Dialog }, Cmd Msg )
    , close : { a | dialog : Dialog } -> ( { a | dialog : Dialog }, Cmd Msg )
    , update : Dialog.Msg -> { a | dialog : Dialog } -> ( { a | dialog : Dialog }, Cmd Msg )
    , view : { a | dialog : Dialog } -> List (Html Msg)
    , subscriptions : { a | dialog : Dialog } -> Sub Msg
    }
dialog =
    let
        dialogConfig : Dialog.Config Msg
        dialogConfig =
            Dialog.createConfig
                { toMsg = DialogMsg
                , projectAdded = AddProjectDialogSaved
                , projectEdited = EditProjectDialogSaved
                }

        updateDialog : Dialog.Msg -> { a | dialog : Dialog } -> ( { a | dialog : Dialog }, Cmd Msg )
        updateDialog msg model =
            Dialog.update dialogConfig msg model.dialog
                |> Tuple.mapFirst (\dialog_ -> { model | dialog = dialog_ })
    in
    { openAddProject = \idx -> updateDialog (Dialog.openAddProject idx)
    , openEditProject = \project -> updateDialog (Dialog.openEditProject project)
    , close = updateDialog Dialog.close
    , update = updateDialog
    , view = .dialog >> Dialog.view dialogConfig
    , subscriptions = .dialog >> Dialog.subscriptions dialogConfig
    }



-- POPUP


type Popup
    = ProjectMoreMenu ProjectId
    | LabelMoreMenu LabelId
    | FilterMoreMenu FilterId


type PopupMsg
    = ProjectMoreMenuMsg PopupView.ProjectMenuItem
    | LabelMoreMenuMsg PopupView.LabelMenuItem
    | FilterMoreMenuMsg PopupView.FilterMenuItem



-- PANELS


projectPanelSystem : ProjectPanel.System Msg
projectPanelSystem =
    ProjectPanel.system
        { toMsg = ProjectPanel
        , addClicked = AddProjectClicked
        , moreClicked = ProjectMoreMenu >> PopupTriggered
        , sorted = ProjectOrderChanged
        }


labelPanelConfig : LabelPanel.Config Msg
labelPanelConfig =
    LabelPanel.createConfig
        { toMsg = LabelPanel
        , addClicked = AddLabelClicked
        , moreClicked = LabelMoreMenu >> PopupTriggered
        , sorted = LabelOrderChanged
        }


filterPanelConfig : FilterPanel.Config Msg
filterPanelConfig =
    FilterPanel.createConfig
        { toMsg = FilterPanel
        , addClicked = AddFilterClicked
        , moreClicked = FilterMoreMenu >> PopupTriggered
        , sorted = FilterOrderChanged
        }


updateProjectPanel :
    ProjectPanel.Msg
    -> { a | projectPanel : ProjectPanel }
    -> ( { a | projectPanel : ProjectPanel }, Cmd Msg )
updateProjectPanel msg model =
    projectPanelSystem.update msg model.projectPanel
        |> Tuple.mapFirst (\projectPanel -> { model | projectPanel = projectPanel })


updateLabelPanel :
    LabelPanel.Msg
    -> { a | labelPanel : LabelPanel }
    -> ( { a | labelPanel : LabelPanel }, Cmd Msg )
updateLabelPanel msg model =
    LabelPanel.update labelPanelConfig msg model.labelPanel
        |> Tuple.mapFirst (\labelPanel -> { model | labelPanel = labelPanel })


updateFilterPanel :
    FilterPanel.Msg
    -> { a | filterPanel : FilterPanel }
    -> ( { a | filterPanel : FilterPanel }, Cmd Msg )
updateFilterPanel msg model =
    FilterPanel.update filterPanelConfig msg model.filterPanel
        |> Tuple.mapFirst (\filterPanel -> { model | filterPanel = filterPanel })



-- COLLECTIONS


projectById : ProjectId -> { a | projectCollection : ProjectCollection } -> Maybe Project
projectById projectId model =
    PC.byId projectId model.projectCollection



-- Flags


type alias Flags =
    { now : Int
    , todoList : Value
    , projectList : Value
    , labelList : Value
    , filterList : Value
    }



-- MODEL


type alias Model =
    { url : Url
    , navKey : Nav.Key
    , seed : Random.Seed
    , todoCollection : TodoCollection
    , projectCollection : ProjectCollection
    , labelCollection : LabelCollection
    , filterCollection : FilterCollection
    , isDrawerModalOpen : Bool
    , popup : Maybe ( Popup, Popper )
    , dialog : Dialog
    , projectPanel : ProjectPanel
    , labelPanel : LabelPanel
    , filterPanel : FilterPanel
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        initial : Model
        initial =
            { url = url
            , navKey = navKey
            , seed = Random.initialSeed flags.now
            , todoCollection = TC.initial
            , projectCollection = PC.initial
            , labelCollection = LC.initial
            , filterCollection = FC.initial
            , isDrawerModalOpen = False
            , popup = Nothing
            , dialog = Dialog.initial
            , projectPanel = ProjectPanel.initial
            , labelPanel = LabelPanel.initial
            , filterPanel = FilterPanel.initial
            }
    in
    Ret.singleton initial
        |> Ret.andThen (initCollections flags)
        |> Ret.andThen (onUrlChanged url)


initCollections : Flags -> DB a -> ( DB a, Cmd msg )
initCollections flags model =
    DB.init flags model
        |> Tuple.mapSecond (List.map logDecodeError >> Cmd.batch)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.popup of
            Just ( _, popper ) ->
                Popper.subscriptions Popper popper

            Nothing ->
                Sub.none
        , projectPanelSystem.subscriptions model.projectPanel
        , LabelPanel.subscriptions labelPanelConfig model.labelPanel
        , FilterPanel.subscriptions filterPanelConfig model.filterPanel
        , dialog.subscriptions model
        ]



-- UPDATE


type Msg
    = NoOp
    | LogError String
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ToggleTodoCompleted TodoId
    | OpenDrawerModal
    | CloseDrawerModal
    | PopupTriggered Popup String
    | Popper Popper.Msg
    | ClosePopup
    | PopupMsg PopupMsg
    | DialogMsg Dialog.Msg
    | DialogCanceled
    | AddProjectDialogSaved Dialog.AddProject.SavedWith
    | AddProjectWithTS Dialog.AddProject.SavedWith Timestamp
    | EditProjectDialogSaved Dialog.EditProject.SavedWith
    | EditProjectWithTS Dialog.EditProject.SavedWith Timestamp
    | AddProjectClicked
    | EditProjectClicked ProjectId
    | AddLabelClicked
    | AddFilterClicked
    | ProjectPanel ProjectPanel.Msg
    | LabelPanel LabelPanel.Msg
    | FilterPanel FilterPanel.Msg
    | ProjectOrderChanged (List Project)
    | LabelOrderChanged (List Label)
    | FilterOrderChanged (List Filter)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        ret =
            ( model, Cmd.none )
    in
    case message of
        NoOp ->
            ( model, Cmd.none )

        LogError error ->
            ( model, logError error )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        urlChanged =
                            url /= model.url
                    in
                    ( model
                    , if urlChanged then
                        Nav.pushUrl model.navKey (Url.toString url)

                      else
                        Cmd.none
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        OnUrlChange url ->
            onUrlChanged url model

        ToggleTodoCompleted todoId ->
            let
                newTodoCollection =
                    TC.toggleCompleted todoId model.todoCollection
            in
            ( { model | todoCollection = newTodoCollection }, Cmd.none )

        OpenDrawerModal ->
            ( { model | isDrawerModalOpen = True }, Cmd.none )

        CloseDrawerModal ->
            ( { model | isDrawerModalOpen = False }, Cmd.none )

        Popper msg ->
            case model.popup of
                Just ( kind, popper ) ->
                    let
                        ( newPopper, cmd ) =
                            Popper.update Popper msg popper
                    in
                    ( { model | popup = Just ( kind, newPopper ) }, cmd )

                Nothing ->
                    ( model, Cmd.none )

        PopupTriggered kind anchorId ->
            Popper.init Popper anchorId "rootPopup"
                |> Tuple.mapFirst (\popper -> { model | popup = Just ( kind, popper ) })

        ClosePopup ->
            ( closePopup model, Cmd.none )

        PopupMsg msg ->
            updateWithPopupKind (updatePopup msg) model

        DialogCanceled ->
            dialog.close model

        DialogMsg msg ->
            dialog.update msg model

        AddProjectDialogSaved savedWith ->
            dialog.close model
                |> Ret.command (Time.now |> Task.perform (AddProjectWithTS savedWith))

        EditProjectDialogSaved savedWith ->
            dialog.close model
                |> Ret.command (Time.now |> Task.perform (EditProjectWithTS savedWith))

        AddProjectWithTS { title, cColor, idx } ts ->
            let
                ( newProject, newModel ) =
                    stepRandom (Project.generator title idx cColor ts) model
            in
            ( DB.mapPC (PC.put newProject) newModel, Cmd.none )

        EditProjectWithTS { projectId, title, cColor } ts ->
            let
                updateProject =
                    Project.setTitle title
                        >> Project.setCColor cColor
                        >> Project.setModifiedAt ts

                newModel =
                    case projectById projectId model of
                        Just project ->
                            DB.mapPC
                                (updateProject project
                                    |> PC.put
                                )
                                model

                        Nothing ->
                            model
            in
            ( newModel, Cmd.none )

        ProjectPanel msg ->
            updateProjectPanel msg model

        LabelPanel msg ->
            updateLabelPanel msg model

        FilterPanel msg ->
            updateFilterPanel msg model

        AddProjectClicked ->
            dialog.openAddProject 0 model

        EditProjectClicked id ->
            projectById id model
                |> Maybe.map (flip dialog.openEditProject model)
                |> Maybe.withDefault ret

        AddLabelClicked ->
            ( model, Cmd.none )

        AddFilterClicked ->
            ( model, Cmd.none )

        ProjectOrderChanged projectList ->
            ( DB.mapPC (PC.updateSortOrder projectList) model
            , Cmd.none
            )

        LabelOrderChanged labelList ->
            ( DB.mapLC (LC.updateSortOrder labelList) model
            , Cmd.none
            )

        FilterOrderChanged filterList ->
            ( DB.mapFC (FC.updateSortOrder filterList) model
            , Cmd.none
            )


stepRandom : Random.Generator a -> { b | seed : Random.Seed } -> ( a, { b | seed : Random.Seed } )
stepRandom generator model =
    let
        ( generated, newSeed ) =
            Random.step generator model.seed
    in
    ( generated, { model | seed = newSeed } )


updateWithPopupKind : (Popup -> Model -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
updateWithPopupKind func model =
    case model.popup of
        Just ( popup, _ ) ->
            func popup model

        Nothing ->
            ( model, Cmd.none )


updatePopup : PopupMsg -> Popup -> Model -> ( Model, Cmd Msg )
updatePopup message popup model =
    case ( popup, message ) of
        ( ProjectMoreMenu projectId, ProjectMoreMenuMsg action ) ->
            updateProjectPopup projectId action model

        ( LabelMoreMenu labelId, LabelMoreMenuMsg action ) ->
            updateLabelPopup labelId action model

        ( FilterMoreMenu filterId, FilterMoreMenuMsg action ) ->
            updateFilterPopup filterId action model

        _ ->
            ( model, Cmd.none )


updateProjectPopup : ProjectId -> PopupView.ProjectMenuItem -> Model -> ( Model, Cmd Msg )
updateProjectPopup projectId action model =
    let
        maybeProject =
            PC.byId projectId model.projectCollection

        projectIdxWithOffset offset =
            maybeProject
                |> Maybe.map (Project.idx >> (+) offset)
                |> Debug.log "idx"
                |> Maybe.withDefault 0
    in
    case action of
        PopupView.AddProjectBelow ->
            dialog.openAddProject (projectIdxWithOffset 1) model
                |> Tuple.mapFirst closePopup

        PopupView.AddProjectAbove ->
            dialog.openAddProject (projectIdxWithOffset 0) model
                |> Tuple.mapFirst closePopup

        PopupView.EditProject ->
            (case maybeProject of
                Just project ->
                    dialog.openEditProject project model

                Nothing ->
                    ( model, Cmd.none )
            )
                |> Tuple.mapFirst closePopup

        _ ->
            ( model, Cmd.none )
                |> Tuple.mapFirst closePopup


updateLabelPopup : LabelId -> PopupView.LabelMenuItem -> Model -> ( Model, Cmd Msg )
updateLabelPopup _ action model =
    case action of
        PopupView.EditLabel ->
            ( model, Cmd.none )
                |> Tuple.mapFirst closePopup


updateFilterPopup : FilterId -> PopupView.FilterMenuItem -> Model -> ( Model, Cmd Msg )
updateFilterPopup _ action model =
    case action of
        PopupView.EditFilter ->
            ( model, Cmd.none )
                |> Tuple.mapFirst closePopup


closePopup : { a | popup : Maybe b } -> { a | popup : Maybe b }
closePopup model =
    { model | popup = Nothing }


onUrlChanged : Url -> Model -> ( Model, Cmd Msg )
onUrlChanged url model =
    ( { model | url = url }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        projectPanelView =
            projectPanelSystem.view
                (PC.sorted model.projectCollection)
                model.projectPanel

        labelPanelView =
            LabelPanel.view labelPanelConfig
                (LC.sorted model.labelCollection)
                model.labelPanel

        filterPanelView =
            FilterPanel.view filterPanelConfig
                (FC.sorted model.filterCollection)
                model.filterPanel
    in
    Layout.view { closeDrawerModal = CloseDrawerModal }
        { appbar = Appbar.view { menuClicked = OpenDrawerModal }
        , drawer =
            Drawer.prefixNavItemsView
                ++ projectPanelView
                ++ labelPanelView
                ++ filterPanelView
        , main = viewRoute (Route.fromUrl model.url) model
        , modal =
            popupView model
                ++ dialog.view model
                ++ projectPanelSystem.viewGhost model.projectPanel
                ++ LabelPanel.viewGhost model.labelPanel
                ++ FilterPanel.viewGhost model.filterPanel
        }
        model.isDrawerModalOpen


viewRoute : Route -> Model -> List (Html Msg)
viewRoute route model =
    case route of
        Route.NotFound url ->
            Page.NotFound.view url

        Route.Root ->
            viewRoute Route.Inbox model

        Route.Inbox ->
            inboxTodoListView
                model.labelCollection
                model.todoCollection

        Route.Project projectId ->
            case projectById projectId model of
                Just project ->
                    projectTodoListView project
                        model.labelCollection
                        model.todoCollection

                Nothing ->
                    viewRoute (Route.NotFound model.url) model

        Route.Label labelId ->
            todoListByLabelIdView
                labelId
                model.projectCollection
                model.labelCollection
                model.todoCollection

        Route.Filter filterId ->
            todoListByFilterIdView
                filterId
                model.projectCollection
                model.labelCollection
                model.todoCollection


todoLabelList : LabelCollection -> Todo -> List Label
todoLabelList lc todo =
    List.filterMap
        (\lid ->
            LC.byId lid lc
        )
        (Todo.labelIdList todo)


viewTodoListHelp : ProjectCollection -> LabelCollection -> List Todo -> List (Html Msg)
viewTodoListHelp pc lc todoList =
    let
        todoProjectFromTodo : Todo -> Maybe TodoProject.TodoProject
        todoProjectFromTodo =
            Todo.projectRef
                >> InboxOrProject.filterMap
                    (\id ->
                        PC.byId id pc
                    )

        viewTodoHelp : Todo -> Maybe (Html Msg)
        viewTodoHelp todo =
            todo
                |> todoProjectFromTodo
                |> Maybe.map
                    (\todoProject ->
                        TodoUI.view { toggle = ToggleTodoCompleted }
                            { viewProject = \_ -> TodoProject.view todoProject }
                            (todoLabelList lc todo)
                            todo
                    )
    in
    List.filterMap viewTodoHelp todoList


projectTodoListView :
    Project
    -> LabelCollection
    -> TodoCollection
    -> List (Html Msg)
projectTodoListView project lc todoCollection =
    let
        projectId =
            Project.id project

        todoList =
            TC.withProjectId projectId todoCollection

        viewTodo todo =
            TodoUI.view
                { toggle = ToggleTodoCompleted }
                { viewProject = \_ -> text "" }
                (todoLabelList lc todo)
                todo
    in
    TodoProject.viewProjectTitle
        { editClicked = EditProjectClicked
        , noOp = NoOp
        }
        project
        :: List.map viewTodo todoList


inboxTodoListView :
    LabelCollection
    -> TodoCollection
    -> List (Html Msg)
inboxTodoListView lc todoCollection =
    let
        todoList =
            TC.inInbox todoCollection

        viewTodo todo =
            TodoUI.view
                { toggle = ToggleTodoCompleted }
                { viewProject = \_ -> text "" }
                (todoLabelList lc todo)
                todo
    in
    TodoProject.viewInboxTitle { noOp = NoOp }
        :: List.map viewTodo todoList


todoListByLabelIdView : LabelId -> ProjectCollection -> LabelCollection -> TodoCollection -> List (Html Msg)
todoListByLabelIdView id pc lc tc =
    div [] [ text "label: ", text <| LabelId.toString id ]
        :: viewTodoListHelp pc lc (TC.withLabelId id tc)


todoListByFilterIdView : FilterId -> ProjectCollection -> LabelCollection -> TodoCollection -> List (Html Msg)
todoListByFilterIdView _ pc lc todoCollection =
    viewTodoListHelp pc lc (TC.sortedByIdx todoCollection)


popupView : Model -> List (Html Msg)
popupView model =
    case model.popup of
        Nothing ->
            []

        Just ( popup, popper ) ->
            let
                viewHelp : List (Html msg) -> (msg -> PopupMsg) -> List (Html Msg)
                viewHelp content toMsg =
                    PopupView.container
                        { onClose = ClosePopup
                        , noOp = NoOp
                        }
                        (content |> List.map (H.map (toMsg >> PopupMsg)))
                        popper
            in
            case popup of
                ProjectMoreMenu _ ->
                    viewHelp PopupView.projectContent ProjectMoreMenuMsg

                LabelMoreMenu _ ->
                    viewHelp PopupView.labelContent LabelMoreMenuMsg

                FilterMoreMenu _ ->
                    viewHelp PopupView.filterContent FilterMoreMenuMsg



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view >> toUnstyled >> List.singleton >> Browser.Document "Todoist Clone"
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
