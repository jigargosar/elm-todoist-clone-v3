module Main exposing (main)

import Appbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Color
import Css
import DNDList
import Dialog exposing (Dialog)
import Dialog.AddProject
import Dialog.EditProject
import Drawer
import Emoji
import Filter exposing (Filter)
import FilterCollection exposing (FilterCollection)
import FilterId exposing (FilterId)
import FilterPanel exposing (FilterPanel)
import Html.Styled as H exposing (Attribute, Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css)
import Json.Decode as JD exposing (Decoder)
import Json.Encode exposing (Value)
import Label exposing (Label)
import LabelCollection exposing (LabelCollection)
import LabelId exposing (LabelId)
import LabelPanel exposing (LabelPanel)
import Layout
import Log exposing (logError)
import Page exposing (Page)
import Page.NotFound
import Popper exposing (Popper)
import PopupView
import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import ProjectPanel exposing (ProjectPanel)
import ProjectRef exposing (ProjectRef)
import Random
import Return
import Styles exposing (..)
import Task
import Time
import Timestamp exposing (Timestamp)
import Todo exposing (Todo)
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)
import TodoProject exposing (TodoProject)
import TodoView
import UI
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



-- PROJECT COLLECTION


projectById : ProjectId -> { a | projectCollection : ProjectCollection } -> Maybe Project
projectById projectId model =
    ProjectCollection.byId projectId model.projectCollection



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
    { page : Page
    , navKey : Nav.Key
    , seed : Random.Seed
    , todoDict : TodoDict
    , projectCollection : ProjectCollection
    , labelCollection : LabelCollection
    , filterCollection : FilterCollection
    , isDrawerModalOpen : Bool
    , popup : Maybe ( Popup, Popper )
    , dialog : Dialog
    , projectPanel : ProjectPanel
    , labelPanel : LabelPanel
    , filterPanel : FilterPanel
    , projectPanelCollapsed : Bool
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        initial : Model
        initial =
            { page = Page.pageFromUrl url
            , navKey = navKey
            , seed = Random.initialSeed flags.now
            , todoDict = TodoDict.initial
            , projectCollection = ProjectCollection.initial
            , labelCollection = LabelCollection.initial
            , filterCollection = FilterCollection.initial
            , isDrawerModalOpen = False
            , popup = Nothing
            , dialog = Dialog.initial
            , projectPanel = ProjectPanel.initial
            , labelPanel = LabelPanel.initial
            , filterPanel = FilterPanel.initial
            , projectPanelCollapsed = False
            }
    in
    Return.singleton initial
        |> Return.andThen
            (Return.pipelK
                [ initTodoDict flags.todoList
                , initProjectCollection flags.projectList
                , initLabelCollection flags.labelList
                , initFilterCollection flags.filterList
                , onUrlChanged url
                ]
            )


initProjectCollection :
    JD.Value
    -> { a | projectCollection : ProjectCollection }
    -> ( { a | projectCollection : ProjectCollection }, Cmd msg )
initProjectCollection encodedProjectList model =
    let
        ( newProjectCollection, cmd ) =
            case ProjectCollection.fromEncodedList encodedProjectList of
                Ok new ->
                    ( new, Cmd.none )

                Err e ->
                    ( model.projectCollection, logError <| JD.errorToString e )
    in
    ( { model | projectCollection = newProjectCollection }, cmd )


initLabelCollection :
    JD.Value
    -> { a | labelCollection : LabelCollection }
    -> ( { a | labelCollection : LabelCollection }, Cmd msg )
initLabelCollection encodedLabelList model =
    let
        ( newLabelCollection, cmd ) =
            case LabelCollection.fromEncodedList encodedLabelList of
                Ok new ->
                    ( new, Cmd.none )

                Err e ->
                    ( model.labelCollection, logError <| JD.errorToString e )
    in
    ( { model | labelCollection = newLabelCollection }, cmd )


initFilterCollection :
    JD.Value
    -> { a | filterCollection : FilterCollection }
    -> ( { a | filterCollection : FilterCollection }, Cmd msg )
initFilterCollection encodedFilterList model =
    let
        ( newFilterCollection, cmd ) =
            case FilterCollection.fromEncodedList encodedFilterList of
                Ok new ->
                    ( new, Cmd.none )

                Err e ->
                    ( model.filterCollection, logError <| JD.errorToString e )
    in
    ( { model | filterCollection = newFilterCollection }, cmd )


initTodoDict :
    JD.Value
    -> { a | todoDict : TodoDict }
    -> ( { a | todoDict : TodoDict }, Cmd msg )
initTodoDict encodedTodoList model =
    let
        ( newTodoDict, cmd ) =
            case TodoDict.fromEncodedList encodedTodoList of
                Ok new ->
                    ( new, Cmd.none )

                Err e ->
                    ( model.todoDict, logError <| JD.errorToString e )
    in
    ( { model | todoDict = newTodoDict }, cmd )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.popup of
            Just ( _, popper ) ->
                Popper.subscriptions Popper popper

            Nothing ->
                Sub.none
        , ProjectPanel.subscriptions projectPanelConfig model.projectPanel
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
    | AddLabelClicked
    | AddFilterClicked
    | ToggleProjectPanel
    | ToggleLabelPanel
    | ToggleFilterPanel
    | ProjectPanelDNDListMsg (DNDList.Msg Project)
    | LabelPanelDNDListMsg (DNDList.Msg Label)
    | FilterPanelDNDListMsg (DNDList.Msg Filter)
    | ProjectOrderChanged (List Project)
    | LabelOrderChanged (List Label)
    | FilterOrderChanged (List Filter)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LogError error ->
            ( model, logError error )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        OnUrlChange url ->
            onUrlChanged url model

        ToggleTodoCompleted todoId ->
            let
                newTodoDict =
                    TodoDict.toggleCompleted todoId model.todoDict
            in
            ( { model | todoDict = newTodoDict }, Cmd.none )

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
                |> Return.command (Time.now |> Task.perform (AddProjectWithTS savedWith))

        EditProjectDialogSaved savedWith ->
            dialog.close model
                |> Return.command (Time.now |> Task.perform (EditProjectWithTS savedWith))

        AddProjectWithTS { title, cColor, idx } ts ->
            let
                ( newProject, newModel ) =
                    stepRandom (Project.generator title idx cColor ts) model
            in
            ( mapProjectCollection (ProjectCollection.put newProject) newModel, Cmd.none )

        EditProjectWithTS { projectId, title, cColor } ts ->
            let
                updateProject =
                    Project.setTitle title
                        >> Project.setCColor cColor
                        >> Project.setModifiedAt ts

                newModel =
                    case projectById projectId model of
                        Just project ->
                            mapProjectCollection
                                (updateProject project
                                    |> ProjectCollection.put
                                )
                                model

                        Nothing ->
                            model
            in
            ( newModel, Cmd.none )

        ToggleProjectPanel ->
            ( { model | projectPanelCollapsed = not model.projectPanelCollapsed }, Cmd.none )

        ToggleLabelPanel ->
            ( mapLabelPanel LabelPanel.onToggle model, Cmd.none )

        ToggleFilterPanel ->
            ( mapFilterPanel FilterPanel.onToggle model, Cmd.none )

        AddProjectClicked ->
            dialog.openAddProject 0 model

        AddLabelClicked ->
            ( model, Cmd.none )

        AddFilterClicked ->
            ( model, Cmd.none )

        ProjectPanelDNDListMsg msg ->
            ProjectPanel.onDNDMsg projectPanelConfig msg model.projectPanel
                |> Tuple.mapFirst (\projectPanel -> mapProjectPanel (always projectPanel) model)

        LabelPanelDNDListMsg msg ->
            LabelPanel.onDNDMsg labelPanelConfig msg model.labelPanel
                |> Tuple.mapFirst (\labelPanel -> mapLabelPanel (always labelPanel) model)

        FilterPanelDNDListMsg msg ->
            FilterPanel.onDNDMsg filterPanelConfig msg model.filterPanel
                |> Tuple.mapFirst (\filterPanel -> mapFilterPanel (always filterPanel) model)

        ProjectOrderChanged projectList ->
            updateProjectSortOrder projectList model

        LabelOrderChanged labelList ->
            updateLabelSortOrder labelList model

        FilterOrderChanged filterList ->
            updateFilterSortOrder filterList model


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


mapProjectPanel : (b -> b) -> { a | projectPanel : b } -> { a | projectPanel : b }
mapProjectPanel func model =
    { model | projectPanel = func model.projectPanel }


projectPanelConfig : ProjectPanel.Config Msg
projectPanelConfig =
    { toggled = ToggleProjectPanel
    , addClicked = AddProjectClicked
    , moreClicked = ProjectMoreMenu >> PopupTriggered
    , dndConfig = { toMsg = ProjectPanelDNDListMsg, sorted = ProjectOrderChanged }
    }


mapProjectCollection : (b -> b) -> { a | projectCollection : b } -> { a | projectCollection : b }
mapProjectCollection func model =
    { model | projectCollection = func model.projectCollection }


updateProjectSortOrder projectList model =
    ( mapProjectCollection (ProjectCollection.updateSortOrder projectList) model
    , Cmd.none
    )


mapLabelPanel : (b -> b) -> { a | labelPanel : b } -> { a | labelPanel : b }
mapLabelPanel func model =
    { model | labelPanel = func model.labelPanel }


labelPanelConfig : LabelPanel.Config Msg
labelPanelConfig =
    { toggled = ToggleLabelPanel
    , addClicked = AddLabelClicked
    , moreClicked = LabelMoreMenu >> PopupTriggered
    , dndConfig = { toMsg = LabelPanelDNDListMsg, sorted = LabelOrderChanged }
    }


mapLabelCollection func model =
    { model | labelCollection = func model.labelCollection }


updateLabelSortOrder labelList model =
    ( mapLabelCollection (LabelCollection.updateSortOrder labelList) model
    , Cmd.none
    )


mapFilterPanel : (b -> b) -> { a | filterPanel : b } -> { a | filterPanel : b }
mapFilterPanel func model =
    { model | filterPanel = func model.filterPanel }


filterPanelConfig : FilterPanel.Config Msg
filterPanelConfig =
    { toggled = ToggleFilterPanel
    , addClicked = AddFilterClicked
    , moreClicked = FilterMoreMenu >> PopupTriggered
    , dndConfig = { toMsg = FilterPanelDNDListMsg, sorted = FilterOrderChanged }
    }


mapFilterCollection func model =
    { model | filterCollection = func model.filterCollection }


updateFilterSortOrder filterList model =
    ( mapFilterCollection (FilterCollection.updateSortOrder filterList) model
    , Cmd.none
    )


updateProjectPopup : ProjectId -> PopupView.ProjectMenuItem -> Model -> ( Model, Cmd Msg )
updateProjectPopup projectId action model =
    let
        maybeProject =
            ProjectCollection.byId projectId model.projectCollection

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
    let
        page =
            Page.pageFromUrl url
    in
    if page /= model.page then
        ( { model | page = page }, Cmd.none )

    else
        ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        projectPanelView =
            UI.viewExpansionPanel
                { toggled = ToggleProjectPanel
                , title = "Projects"
                , isExpanded = not model.projectPanelCollapsed
                , secondary = { iconName = "add", action = AddProjectClicked }
                }
                (\_ ->
                    ProjectPanel.viewItems projectPanelConfig
                        (ProjectCollection.sorted model.projectCollection)
                        model.projectPanel.dnd
                )

        labelPanelView =
            LabelPanel.view labelPanelConfig
                (LabelCollection.sorted model.labelCollection)
                model.labelPanel

        filterPanelView =
            FilterPanel.view filterPanelConfig
                (FilterCollection.sorted model.filterCollection)
                model.filterPanel
    in
    Layout.view { closeDrawerModal = CloseDrawerModal }
        { appbar = Appbar.view { menuClicked = OpenDrawerModal }
        , drawer =
            Drawer.prefixNavItemsView
                ++ projectPanelView
                ++ labelPanelView
                ++ filterPanelView
        , main = viewPage model
        , modal =
            popupView model
                ++ dialog.view model
                ++ ProjectPanel.viewGhost model.projectPanel
                ++ LabelPanel.viewGhost model.labelPanel
                ++ FilterPanel.viewGhost model.filterPanel
        }
        model.isDrawerModalOpen


viewPage : Model -> List (Html Msg)
viewPage model =
    case model.page of
        Page.NotFound url ->
            Page.NotFound.view url

        Page.TodoListByProjectRef projectRef ->
            projectRefTodoListView projectRef
                model.projectCollection
                model.labelCollection
                model.todoDict

        Page.TodoListByLabelId labelId ->
            todoListByLabelIdView
                labelId
                model.projectCollection
                model.labelCollection
                model.todoDict

        Page.TodoListByFilterId filterId ->
            todoListByFilterIdView
                filterId
                model.projectCollection
                model.labelCollection
                model.todoDict


projectRefTodoListView : ProjectRef -> ProjectCollection -> LabelCollection -> TodoDict -> List (Html Msg)
projectRefTodoListView ref pc lc todoDict =
    let
        todoList =
            TodoDict.withProjectRef ref todoDict

        config =
            { toggle = ToggleTodoCompleted }

        viewIsCompleted todo =
            let
                emoji =
                    if Todo.isCompleted todo then
                        Emoji.heavy_check_mark

                    else
                        Emoji.heavy_large_circle

                toggleMsg =
                    config.toggle <| Todo.id todo
            in
            Emoji.button toggleMsg emoji

        viewProject : Todo -> Html msg
        viewProject todo =
            let
                todoProject : TodoProject
                todoProject =
                    TodoProject.fromTodo pc todo
            in
            div
                [ css
                    [ ph 1
                    , lh 1.5
                    , Css.fontSize Css.small
                    , bg (toCssColor todoProject.color)
                    , c_ (toCssColor <| Color.highContrast todoProject.color)
                    , bor 2
                    , hover [ underline, pointer ]
                    ]
                ]
                [ text todoProject.title ]

        viewLabels todo =
            List.filterMap (\lid -> LabelCollection.byId lid lc |> Maybe.map viewLabel) (Todo.labelIdList todo)

        viewLabel label =
            div
                [ css
                    [ ph 1
                    , Css.fontSize Css.small
                    , c_ (Label.color label |> Color.blacken 15 |> toCssColor)
                    , hover [ underline, pointer ]
                    ]
                ]
                [ text <| Label.title label ]

        projectInfo : TodoProject
        projectInfo =
            TodoProject.fromProjectRef pc ref

        viewTodo todo =
            div [ class "ph2 pv1 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]
                [ div [ css [ flex, itemsCenter ] ]
                    [ viewIsCompleted todo
                    , div [ class "pa2 flex-grow-1" ] [ text <| Todo.title todo ]
                    , viewProject todo
                    ]
                , div [ css [ flex ] ] (viewLabels todo)
                ]

        viewProjectTitle =
            div [] [ text projectInfo.title ]
    in
    viewProjectTitle :: List.map viewTodo todoList


todoListByLabelIdView : LabelId -> ProjectCollection -> LabelCollection -> TodoDict -> List (Html Msg)
todoListByLabelIdView labelId pc lc todoDict =
    [ TodoView.viewList { toggle = ToggleTodoCompleted } pc lc (TodoDict.withLabelId labelId todoDict)
    ]


todoListByFilterIdView : FilterId -> ProjectCollection -> LabelCollection -> TodoDict -> List (Html Msg)
todoListByFilterIdView _ pc lc todoDict =
    [ TodoView.viewList { toggle = ToggleTodoCompleted } pc lc (TodoDict.sortedByIdx todoDict)
    ]


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
