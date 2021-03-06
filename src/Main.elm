module Main exposing (main)

import Appbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Cmds
import Dialog exposing (Dialog)
import Dialog.AddProject as AddProject
import Dialog.EditProject as EditProject
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
import Lens exposing (Lens)
import Log exposing (logDecodeError, logError)
import Optional exposing (Optional)
import Popper exposing (Popper)
import PopupView
import Project exposing (Project)
import ProjectCollection as PC exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import ProjectPanel exposing (ProjectPanel)
import Random
import Ret exposing (Ret, RetF)
import Return
import Route exposing (Route)
import Timestamp exposing (Timestamp)
import Todo exposing (Todo)
import TodoCollection as TC exposing (TodoCollection)
import TodoId exposing (TodoId)
import TodoProject
import TodoUI
import Tuple2
import Url exposing (Url)



-- DIALOG


dialogSystem : Dialog.System Msg
dialogSystem =
    Dialog.system
        { toMsg = SubMsg << Dialog
        , projectAdded = AddProjectDialogSaved
        , projectEdited = EditProjectDialogSaved
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


projectPanelSys : ProjectPanel.System Msg
projectPanelSys =
    ProjectPanel.system
        { toMsg = SubMsg << ProjectPanel
        , addClicked = AddProjectClicked
        , moreClicked = ProjectMoreMenu >> PopupTriggered
        , sorted = ProjectOrderChanged
        }


labelPanelConfig : LabelPanel.Config Msg
labelPanelConfig =
    LabelPanel.createConfig
        { toMsg = SubMsg << LabelPanel
        , addClicked = AddLabelClicked
        , moreClicked = LabelMoreMenu >> PopupTriggered
        , sorted = LabelOrderChanged
        }


filterPanelConfig : FilterPanel.Config Msg
filterPanelConfig =
    FilterPanel.createConfig
        { toMsg = SubMsg << FilterPanel
        , addClicked = AddFilterClicked
        , moreClicked = PopupTriggered << FilterMoreMenu
        , sorted = FilterOrderChanged
        }



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


fields =
    { tc = Lens.fromTuple ( .todoCollection, \s b -> { b | todoCollection = s } )
    , pc = Lens.fromTuple ( .projectCollection, \s b -> { b | projectCollection = s } )
    , lc = Lens.fromTuple ( .labelCollection, \s b -> { b | labelCollection = s } )
    , fc = Lens.fromTuple ( .filterCollection, \s b -> { b | filterCollection = s } )
    , isDrawerModalOpen = Lens.fromTuple ( .isDrawerModalOpen, \s b -> { b | isDrawerModalOpen = s } )
    , dialog = Lens.fromTuple ( .dialog, \s b -> { b | dialog = s } )
    , projectPanel = Lens.fromTuple ( .projectPanel, \s b -> { b | projectPanel = s } )
    , labelPanel = Lens.fromTuple ( .labelPanel, \s b -> { b | labelPanel = s } )
    , filterPanel = Lens.fromTuple ( .filterPanel, \s b -> { b | filterPanel = s } )
    , popper =
        Optional.fromTuple
            ( .popup >> Maybe.map Tuple.second
            , \s b -> { b | popup = Maybe.map (Tuple.mapSecond (always s)) b.popup }
            )
    , popup = Optional.fromTuple ( .popup, \s b -> { b | popup = Just s } )
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
            , dialog = dialogSystem.initial
            , projectPanel = ProjectPanel.initial
            , labelPanel = LabelPanel.initial
            , filterPanel = FilterPanel.initial
            }
    in
    Return.singleton initial
        |> Return.andThen (initCollections flags)
        |> Return.andThen (onUrlChanged url)


initCollections : Flags -> Model -> Ret Model Msg
initCollections flags =
    let
        handleDecodeResult lens result ( big, errors ) =
            case result of
                Ok small ->
                    ( lens.set small big, errors )

                Err error ->
                    ( big, error :: errors )
    in
    Tuple2.pairTo []
        >> handleDecodeResult fields.tc (TC.fromEncodedList flags.todoList)
        >> handleDecodeResult fields.pc (PC.fromEncodedList flags.projectList)
        >> handleDecodeResult fields.lc (LC.fromEncodedList flags.labelList)
        >> handleDecodeResult fields.fc (FC.fromEncodedList flags.filterList)
        >> Tuple.mapSecond (List.map logDecodeError >> Cmd.batch)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.popup of
            Just ( _, popper ) ->
                Popper.subscriptions popperConfig popper

            Nothing ->
                Sub.none
        , projectPanelSys.subscriptions model.projectPanel
        , LabelPanel.subscriptions labelPanelConfig model.labelPanel
        , FilterPanel.subscriptions filterPanelConfig model.filterPanel
        , dialogSystem.subscriptions model.dialog
        ]



-- UPDATE


type SubMsg
    = ProjectPanel ProjectPanel.Msg
    | LabelPanel LabelPanel.Msg
    | FilterPanel FilterPanel.Msg
    | Dialog Dialog.Msg
    | Popper Popper.Msg


type Msg
    = NoOp
    | LogError String
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ToggleTodoCompleted TodoId
    | OpenDrawerModal
    | CloseDrawerModal
    | PopupTriggered Popup String
    | ClosePopup
    | PopupMsg PopupMsg
    | AddProjectDialogSaved AddProject.SavedWith
    | AddProjectWithTS AddProject.SavedWith Timestamp
    | EditProjectDialogSaved EditProject.SavedWith
    | EditProjectWithTS EditProject.SavedWith Timestamp
    | AddProjectClicked
    | EditProjectClicked ProjectId
    | AddLabelClicked
    | AddFilterClicked
    | SubMsg SubMsg
    | ProjectOrderChanged (List Project)
    | LabelOrderChanged (List Label)
    | FilterOrderChanged (List Filter)


update : Msg -> Model -> Ret Model Msg
update message model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case message of
        NoOp ->
            noOp

        LogError error ->
            ( model, logError error )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        urlChanged =
                            url /= model.url
                    in
                    if urlChanged then
                        ( model, Nav.pushUrl model.navKey (Url.toString url) )

                    else
                        noOp

                Browser.External href ->
                    ( model, Nav.load href )

        OnUrlChange url ->
            onUrlChanged url model

        ToggleTodoCompleted todoId ->
            ( Lens.over fields.tc (TC.toggleCompleted todoId) model, Cmd.none )

        OpenDrawerModal ->
            ( fields.isDrawerModalOpen.set True model, Cmd.none )

        CloseDrawerModal ->
            ( fields.isDrawerModalOpen.set False model, Cmd.none )

        PopupTriggered kind anchorId ->
            Popper.init popperConfig anchorId "rootPopup"
                |> Tuple.mapFirst (\popper -> fields.popup.set ( kind, popper ) model)

        ClosePopup ->
            ( closePopup model, Cmd.none )

        PopupMsg msg ->
            updateWithPopupKind (updatePopup msg) model

        AddProjectDialogSaved savedWith ->
            noOp |> Ret.getNow (AddProjectWithTS savedWith)

        EditProjectDialogSaved savedWith ->
            noOp |> Ret.getNow (EditProjectWithTS savedWith)

        AddProjectWithTS { title, cColor, idx } ts ->
            let
                ( newProject, newModel ) =
                    stepRandom (Project.generator title idx cColor ts) model
            in
            ( Lens.over fields.pc (PC.put newProject) newModel, Cmd.none )

        EditProjectWithTS { projectId, title, cColor } ts ->
            let
                updateProject =
                    Project.setTitle title
                        >> Project.setCColor cColor
                        >> Project.setModifiedAt ts
            in
            ( Lens.over fields.pc (PC.update projectId updateProject) model, Cmd.none )

        SubMsg subMsg ->
            updateSub subMsg model

        AddProjectClicked ->
            openAddProjectDialog 0 model

        EditProjectClicked id ->
            openEditProjectDialog id model

        AddLabelClicked ->
            noOp

        AddFilterClicked ->
            noOp

        ProjectOrderChanged projectList ->
            ( Lens.over fields.pc (PC.updateSortOrder projectList) model, Cmd.none )

        LabelOrderChanged labelList ->
            ( Lens.over fields.lc (LC.updateSortOrder labelList) model, Cmd.none )

        FilterOrderChanged filterList ->
            ( Lens.over fields.fc (FC.updateSortOrder filterList) model, Cmd.none )


openAddProjectDialog : Int -> Model -> Ret Model Msg
openAddProjectDialog i model =
    ( model, Cmds.fromMsg (dialogSystem.openAddProject i) )


openEditProjectDialog : ProjectId -> Model -> Ret Model Msg
openEditProjectDialog id model =
    ( model
    , projectById id model
        |> Maybe.map dialogSystem.openEditProject
        |> Cmds.fromMaybeMsg
    )


type alias Return =
    Ret Model Msg


popperConfig : Popper.Msg -> Msg
popperConfig =
    SubMsg << Popper


updateSub : SubMsg -> Model -> Ret Model Msg
updateSub message =
    case message of
        ProjectPanel msg ->
            Ret.updateSub fields.projectPanel projectPanelSys.update msg

        LabelPanel msg ->
            Ret.updateSub fields.labelPanel (LabelPanel.update labelPanelConfig) msg

        FilterPanel msg ->
            Ret.updateSub fields.filterPanel (FilterPanel.update filterPanelConfig) msg

        Dialog msg ->
            Ret.updateSub fields.dialog dialogSystem.update msg

        Popper msg ->
            Ret.updateOptional fields.popper (Popper.update popperConfig) msg


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

        ret =
            Ret.only model
    in
    case action of
        PopupView.AddProjectBelow ->
            openAddProjectDialog (projectIdxWithOffset 1) model
                |> Ret.map closePopup

        PopupView.AddProjectAbove ->
            openAddProjectDialog (projectIdxWithOffset 0) model
                |> Ret.map closePopup

        PopupView.EditProject ->
            openEditProjectDialog projectId model
                |> Ret.map closePopup

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


type alias PanelView =
    { content : List (Html Msg), ghost : List (Html Msg) }


viewProjectPanel : ProjectCollection -> ProjectPanel -> PanelView
viewProjectPanel pc panel =
    { content = projectPanelSys.view (PC.sorted pc) panel
    , ghost = projectPanelSys.viewGhost panel
    }


viewLabelPanel : LabelCollection -> LabelPanel -> PanelView
viewLabelPanel lc panel =
    { content = LabelPanel.view labelPanelConfig (LC.sorted lc) panel
    , ghost = LabelPanel.viewGhost panel
    }


viewFilterPanel : FilterCollection -> FilterPanel -> PanelView
viewFilterPanel fc panel =
    { content = FilterPanel.view filterPanelConfig (FC.sorted fc) panel
    , ghost = FilterPanel.viewGhost panel
    }


view : Model -> Html Msg
view model =
    let
        projectPanelView =
            viewProjectPanel model.projectCollection model.projectPanel

        labelPanelView =
            viewLabelPanel model.labelCollection model.labelPanel

        filterPanelView =
            viewFilterPanel model.filterCollection model.filterPanel
    in
    Layout.view { closeDrawerModal = CloseDrawerModal }
        { appbar = Appbar.view { menuClicked = OpenDrawerModal }
        , drawer =
            Drawer.prefixNavItemsView
                ++ projectPanelView.content
                ++ labelPanelView.content
                ++ filterPanelView.content
        , main = viewRoute (Route.fromUrl model.url) model
        , modal =
            popupView model
                ++ [ dialogSystem.view model.dialog ]
                ++ projectPanelView.ghost
                ++ labelPanelView.ghost
                ++ filterPanelView.ghost
        }
        model.isDrawerModalOpen


viewRoute : Route -> Model -> List (Html Msg)
viewRoute route model =
    case route of
        Route.NotFound url ->
            [ div [] [ text <| "NotFound: " ++ Url.toString url ] ]

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


todoListByLabelIdView :
    LabelId
    -> ProjectCollection
    -> LabelCollection
    -> TodoCollection
    -> List (Html Msg)
todoListByLabelIdView id pc lc tc =
    div [] [ text "label: ", text <| LabelId.toString id ]
        :: viewTodoListHelp pc lc (TC.withLabelId id tc)


todoListByFilterIdView :
    FilterId
    -> ProjectCollection
    -> LabelCollection
    -> TodoCollection
    -> List (Html Msg)
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
