module Main exposing (main)

import Appbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import DNDList
import Dialog exposing (Dialog)
import Drawer
import Filter exposing (Filter)
import FilterCollection exposing (FilterCollection)
import FilterId exposing (FilterId)
import FilterPanel exposing (FilterPanel)
import Html.Styled as H exposing (Attribute, Html, toUnstyled)
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
import Return
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)
import TodoView
import Url exposing (Url)



-- POPUP


type alias PopupKind =
    Drawer.PanelItemId


type PopupMsg
    = ProjectMoreMenuMsg PopupView.ProjectMenuItem
    | LabelMoreMenuMsg PopupView.LabelMenuItem
    | FilterMoreMenuMsg PopupView.FilterMenuItem



-- Flags


type alias Flags =
    { todoList : Value
    , projectList : Value
    , labelList : Value
    , filterList : Value
    }



-- MODEL


type alias Model =
    { page : Page
    , navKey : Nav.Key
    , todoDict : TodoDict
    , projectCollection : ProjectCollection
    , labelCollection : LabelCollection
    , filterCollection : FilterCollection
    , isDrawerModalOpen : Bool
    , popup : Maybe ( PopupKind, Popper )
    , dialog : Maybe Dialog
    , projectPanel : ProjectPanel
    , labelPanel : LabelPanel
    , filterPanel : FilterPanel
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        initial : Model
        initial =
            { page = Page.pageFromUrl url
            , navKey = navKey
            , todoDict = TodoDict.initial
            , projectCollection = ProjectCollection.initial
            , labelCollection = LabelCollection.initial
            , filterCollection = FilterCollection.initial
            , isDrawerModalOpen = False
            , popup = Nothing
            , dialog = Nothing
            , projectPanel = ProjectPanel.initial
            , labelPanel = LabelPanel.initial
            , filterPanel = FilterPanel.initial
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
    | PanelAddClicked Drawer.Panel
    | PopupTriggered PopupKind String
    | Popper Popper.Msg
    | ClosePopup
    | PopupMsg PopupMsg
    | OpenDialog Dialog
    | CloseDialog
    | ToggleProjectsPanel
    | ProjectPanelDNDListMsg (DNDList.Msg Project)
    | ProjectOrderChanged (List Project)
    | ToggleLabelsPanel
    | LabelPanelDNDListMsg (DNDList.Msg Label)
    | LabelOrderChanged (List Label)
    | ToggleFiltersPanel
    | FilterPanelDNDListMsg (DNDList.Msg Filter)
    | FilterOrderChanged (List Filter)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            Return.singleton model

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

        PanelAddClicked panel ->
            case panel of
                Drawer.Projects ->
                    ( { model | dialog = Just Dialog.initAddProject }, Cmd.none )

                Drawer.Labels ->
                    ( { model | dialog = Just Dialog.AddLabel }, Cmd.none )

                Drawer.Filters ->
                    ( { model | dialog = Just Dialog.AddFilter }, Cmd.none )

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
            let
                handlePopupMsg popKind =
                    case ( popKind, msg ) of
                        ( Drawer.ProjectItemId projectId, ProjectMoreMenuMsg action ) ->
                            onProjectMoreMenuAction projectId action model

                        ( Drawer.LabelItemId labelId, LabelMoreMenuMsg action ) ->
                            onLabelMoreMenuAction labelId action model

                        ( Drawer.FilterItemId filterId, FilterMoreMenuMsg action ) ->
                            onFilterMoreMenuAction filterId action model

                        _ ->
                            ( model, Cmd.none )
            in
            model.popup
                |> Maybe.map (Tuple.first >> handlePopupMsg)
                |> Maybe.withDefault ( model, Cmd.none )

        OpenDialog dialog ->
            ( { model | dialog = Just dialog }, Cmd.none )

        CloseDialog ->
            ( { model | dialog = Nothing }, Cmd.none )

        ToggleProjectsPanel ->
            ( mapProjectPanel ProjectPanel.onToggle model, Cmd.none )

        ProjectPanelDNDListMsg msg ->
            ProjectPanel.onDNDMsg projectPanelConfig msg model.projectPanel
                |> Tuple.mapFirst (\projectPanel -> mapProjectPanel (always projectPanel) model)

        ProjectOrderChanged projectList ->
            updateProjectSortOrder projectList model

        ToggleLabelsPanel ->
            ( mapLabelPanel LabelPanel.onToggle model, Cmd.none )

        LabelPanelDNDListMsg msg ->
            LabelPanel.onDNDMsg labelPanelConfig msg model.labelPanel
                |> Tuple.mapFirst (\labelPanel -> mapLabelPanel (always labelPanel) model)

        LabelOrderChanged labelList ->
            updateLabelSortOrder labelList model

        ToggleFiltersPanel ->
            ( mapFilterPanel FilterPanel.onToggle model, Cmd.none )

        FilterPanelDNDListMsg msg ->
            FilterPanel.onDNDMsg filterPanelConfig msg model.filterPanel
                |> Tuple.mapFirst (\filterPanel -> mapFilterPanel (always filterPanel) model)

        FilterOrderChanged filterList ->
            updateFilterSortOrder filterList model


mapProjectPanel : (b -> b) -> { a | projectPanel : b } -> { a | projectPanel : b }
mapProjectPanel func model =
    { model | projectPanel = func model.projectPanel }


projectPanelConfig : ProjectPanel.Config Msg
projectPanelConfig =
    { toggled = ToggleProjectsPanel
    , addClicked = PanelAddClicked Drawer.Projects
    , moreClicked = Drawer.ProjectItemId >> PopupTriggered
    , dndConfig = { toMsg = ProjectPanelDNDListMsg, sorted = ProjectOrderChanged }
    }


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
    { toggled = ToggleLabelsPanel
    , addClicked = PanelAddClicked Drawer.Labels
    , moreClicked = Drawer.LabelItemId >> PopupTriggered
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
    { toggled = ToggleFiltersPanel
    , addClicked = PanelAddClicked Drawer.Filters
    , moreClicked = Drawer.FilterItemId >> PopupTriggered
    , dndConfig = { toMsg = FilterPanelDNDListMsg, sorted = FilterOrderChanged }
    }


mapFilterCollection func model =
    { model | filterCollection = func model.filterCollection }


updateFilterSortOrder filterList model =
    ( mapFilterCollection (FilterCollection.updateSortOrder filterList) model
    , Cmd.none
    )


onProjectMoreMenuAction : ProjectId -> PopupView.ProjectMenuItem -> Model -> ( Model, Cmd Msg )
onProjectMoreMenuAction projectId action model =
    case action of
        PopupView.EditProject ->
            ( { model | dialog = Dialog.EditProject projectId |> Just }
            , Cmd.none
            )
                |> Return.map closePopup

        _ ->
            ( model, Cmd.none )
                |> Return.map closePopup


onLabelMoreMenuAction : LabelId -> PopupView.LabelMenuItem -> Model -> ( Model, Cmd Msg )
onLabelMoreMenuAction labelId action model =
    case action of
        PopupView.EditLabel ->
            ( { model | dialog = Dialog.EditLabel labelId |> Just }
            , Cmd.none
            )
                |> Return.map closePopup


onFilterMoreMenuAction : FilterId -> PopupView.FilterMenuItem -> Model -> ( Model, Cmd Msg )
onFilterMoreMenuAction filterId action model =
    case action of
        PopupView.EditFilter ->
            ( { model | dialog = Dialog.EditFilter filterId |> Just }
            , Cmd.none
            )
                |> Return.map closePopup


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
            ProjectPanel.view projectPanelConfig
                (ProjectCollection.sorted model.projectCollection)
                model.projectPanel

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
        , main = pageView model
        , modal =
            popupView model
                ++ dialogView model
                ++ ProjectPanel.viewGhost model.projectPanel
                ++ LabelPanel.viewGhost model.labelPanel
                ++ FilterPanel.viewGhost model.filterPanel
        }
        model.isDrawerModalOpen


pageView : Model -> List (Html Msg)
pageView model =
    case model.page of
        Page.NotFound url ->
            Page.NotFound.view url

        Page.TodoListByProjectRef projectRef ->
            mainView projectRef
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


mainView : ProjectRef -> ProjectCollection -> LabelCollection -> TodoDict -> List (Html Msg)
mainView ref pc lc todoDict =
    [ TodoView.viewList { toggle = ToggleTodoCompleted } pc lc (TodoDict.withProjectRef ref todoDict)
    ]


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

        Just ( kind, popper ) ->
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
            case kind of
                Drawer.ProjectItemId _ ->
                    viewHelp PopupView.projectContent ProjectMoreMenuMsg

                Drawer.LabelItemId _ ->
                    viewHelp PopupView.labelContent LabelMoreMenuMsg

                Drawer.FilterItemId _ ->
                    viewHelp PopupView.filterContent FilterMoreMenuMsg


dialogView : Model -> List (Html Msg)
dialogView model =
    case model.dialog of
        Just dialog ->
            Dialog.viewDialog { cancel = CloseDialog } dialog

        Nothing ->
            []



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
