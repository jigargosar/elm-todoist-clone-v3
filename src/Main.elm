module Main exposing (main)

import Appbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Drag exposing (Drag)
import Drawer
import Filter exposing (Filter)
import FilterCollection exposing (FilterCollection)
import FilterId exposing (FilterId)
import Html.Styled exposing (Html, toUnstyled)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Label exposing (Label)
import LabelCollection exposing (LabelCollection)
import LabelId exposing (LabelId)
import Layout
import Log exposing (logError)
import Page exposing (Page)
import Page.NotFound
import Popper exposing (Popper)
import PopupView
import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import ProjectRef exposing (ProjectRef)
import Return
import Route
import Styles
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)
import TodoView
import Url exposing (Url)
import View exposing (View)


type PopupKind
    = DrawerPanelItemPopup Drawer.PanelItemId



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
    , drawerPanelsState : Drawer.AllPanelsState
    , popup : Maybe ( PopupKind, Popper )
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
            , drawerPanelsState = Drawer.initialPanelsState
            , popup = Nothing
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
        [ Drawer.panelsSubscriptions panelsConfig model.drawerPanelsState
        , case model.popup of
            Just ( _, popper ) ->
                Popper.subscriptions Popper popper

            Nothing ->
                Sub.none
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
    | ToggleDrawerExpansionPanel Drawer.Panel
    | DrawerPanelDrag Drawer.Panel Drag.Msg
    | DrawerPanelDragComplete Drawer.Panel Drag.Info
    | PopupTriggered PopupKind String
    | Popper Popper.Msg
    | ClosePopup


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

        ToggleDrawerExpansionPanel panel ->
            ( { model
                | drawerPanelsState =
                    Drawer.togglePanelExpansion panel model.drawerPanelsState
              }
            , Cmd.none
            )

        DrawerPanelDrag panel msg ->
            Drawer.updatePanelDrag DrawerPanelDrag DrawerPanelDragComplete panel msg model.drawerPanelsState
                |> Tuple.mapFirst
                    (\drawerPanelState -> { model | drawerPanelsState = drawerPanelState })

        DrawerPanelDragComplete panel info ->
            onDrawerPanelDragComplete panel info model

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
            ( { model | popup = Nothing }, Cmd.none )


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


onDrawerPanelDragComplete : Drawer.Panel -> Drag.Info -> Model -> ( Model, Cmd Msg )
onDrawerPanelDragComplete panel info model =
    let
        rotate =
            Drag.rotateFromInfo info
    in
    case panel of
        Drawer.Projects ->
            let
                projectList =
                    ProjectCollection.sorted model.projectCollection
            in
            ( { model
                | projectCollection =
                    ProjectCollection.updateSortOrder (rotate projectList) model.projectCollection
              }
            , Cmd.none
            )

        Drawer.Labels ->
            let
                labelList =
                    LabelCollection.sorted model.labelCollection
            in
            ( { model
                | labelCollection =
                    LabelCollection.updateSortOrder (rotate labelList) model.labelCollection
              }
            , Cmd.none
            )

        Drawer.Filters ->
            let
                filterList =
                    FilterCollection.sorted model.filterCollection
            in
            ( { model
                | filterCollection =
                    FilterCollection.updateSortOrder (rotate filterList) model.filterCollection
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    Layout.view { closeDrawerModal = CloseDrawerModal }
        { appbar = Appbar.view { menuClicked = OpenDrawerModal }
        , drawer = View.concat [ drawerView model, popupView model ]
        , main = pageView model
        }
        model.isDrawerModalOpen


moreClickedDecoder : (id -> Drawer.PanelItemId) -> String -> id -> JD.Decoder Msg
moreClickedDecoder panelItemId anchorId id =
    let
        kind =
            DrawerPanelItemPopup (panelItemId id)

        msg =
            PopupTriggered kind anchorId
    in
    JD.succeed msg


panelDragSystem : Drawer.Panel -> Drag.System a Msg
panelDragSystem panel =
    Drag.system (DrawerPanelDrag panel) (DrawerPanelDragComplete panel)


projectPanelConfig : Drawer.PanelConfig ProjectId Project Msg
projectPanelConfig =
    { toggleExpansionClicked = ToggleDrawerExpansionPanel Drawer.Projects
    , panelTitle = "Projects"
    , itemConfig =
        { moreClicked = moreClickedDecoder Drawer.ProjectItemId
        , dragSystem = panelDragSystem Drawer.Projects
        , domIdPrefix = "drawer-project-panel-item__"
        , id = Project.id
        , idToString = ProjectId.toString
        , title = Project.title
        , route = Project.id >> Route.Project
        , iconName = "folder"
        , iconStyle = Styles.c_ << Project.cssColor
        }
    }


labelPanelConfig : Drawer.PanelConfig LabelId Label Msg
labelPanelConfig =
    { toggleExpansionClicked = ToggleDrawerExpansionPanel Drawer.Labels
    , panelTitle = "Labels"
    , itemConfig =
        { moreClicked = moreClickedDecoder Drawer.LabelItemId
        , dragSystem = panelDragSystem Drawer.Labels
        , domIdPrefix = "drawer-label-panel-item__"
        , id = Label.id
        , idToString = LabelId.toString
        , title = Label.title
        , route = Label.id >> Route.Label
        , iconName = "label"
        , iconStyle = Styles.c_ << Label.cssColor
        }
    }


filterPanelConfig : Drawer.PanelConfig FilterId Filter Msg
filterPanelConfig =
    { toggleExpansionClicked = ToggleDrawerExpansionPanel Drawer.Filters
    , panelTitle = "Filters"
    , itemConfig =
        { moreClicked = moreClickedDecoder Drawer.FilterItemId
        , dragSystem = panelDragSystem Drawer.Filters
        , domIdPrefix = "drawer-filter-panel-item__"
        , id = Filter.id
        , idToString = FilterId.toString
        , title = Filter.title
        , route = Filter.id >> Route.Filter
        , iconName = "filter_list"
        , iconStyle = Styles.c_ << Filter.cssColor
        }
    }


panelsConfig : Drawer.AllPanelsConfig Msg
panelsConfig =
    { projects = projectPanelConfig
    , labels = labelPanelConfig
    , filters = filterPanelConfig
    }


drawerView : Model -> { content : List (Html Msg), portal : List (Html Msg) }
drawerView model =
    Drawer.view panelsConfig
        { projects = ProjectCollection.sorted model.projectCollection
        , labels = LabelCollection.sorted model.labelCollection
        , filters = FilterCollection.sorted model.filterCollection
        }
        model.drawerPanelsState


pageView : Model -> View (Html Msg)
pageView model =
    case model.page of
        Page.NotFound url ->
            Page.NotFound.view url

        Page.TodoListByProjectRef projectRef ->
            View.content <| mainView projectRef model.projectCollection model.labelCollection model.todoDict

        Page.TodoListByLabelId labelId ->
            View.content <| todoListByLabelIdView labelId model.projectCollection model.labelCollection model.todoDict

        Page.TodoListByFilterId filterId ->
            View.content <| todoListByFilterIdView filterId model.projectCollection model.labelCollection model.todoDict


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


popupView model =
    case model.popup of
        Nothing ->
            View.none

        Just ( kind, popper ) ->
            case kind of
                DrawerPanelItemPopup panelItemId ->
                    let
                        mockView content =
                            PopupView.mockPopupView { onClose = ClosePopup, noOp = NoOp } content popper
                    in
                    case panelItemId of
                        Drawer.ProjectItemId projectId ->
                            mockView PopupView.projectContent

                        Drawer.LabelItemId labelId ->
                            mockView PopupView.projectContent

                        Drawer.FilterItemId filterId ->
                            mockView PopupView.projectContent



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
