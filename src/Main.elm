port module Main exposing (main)

import Appbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Drag exposing (Drag)
import Drawer
import FilterCollection exposing (FilterCollection)
import FilterId exposing (FilterId)
import Html.Styled exposing (Html, toUnstyled)
import Json.Decode as JD
import Json.Encode exposing (Value)
import LabelCollection exposing (LabelCollection)
import LabelId exposing (LabelId)
import Layout
import Page exposing (Page)
import Page.NotFound
import ProjectCollection exposing (ProjectCollection)
import ProjectId exposing (ProjectId)
import ProjectRef exposing (ProjectRef)
import Return
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)
import TodoView
import Url exposing (Url)
import View exposing (View)


port logError : String -> Cmd msg



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
    , drawerPanelState : Drawer.PanelState
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
            , drawerPanelState = Drawer.initialPanelState
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
        [ Drawer.panelSubscriptions DrawerPanelDrag model.drawerPanelState
        ]



-- UPDATE


type Msg
    = NoOp
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | ToggleTodoCompleted TodoId
    | OpenDrawerModal
    | CloseDrawerModal
    | ToggleDrawerExpansionPanel Drawer.Panel
    | DrawerPanelDrag Drawer.Panel Drag.Msg
    | DrawerPanelDragComplete Drawer.Panel Drag.Info
    | PanelItemMoreMenuClicked Drawer.PanelItemId


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            Return.singleton model

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
                | drawerPanelState =
                    Drawer.toggleExpansionPanel panel model.drawerPanelState
              }
            , Cmd.none
            )

        DrawerPanelDrag panel msg ->
            Drawer.updatePanelDrag DrawerPanelDrag DrawerPanelDragComplete panel msg model.drawerPanelState
                |> Tuple.mapFirst
                    (\drawerPanelState -> { model | drawerPanelState = drawerPanelState })

        DrawerPanelDragComplete panel info ->
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

        PanelItemMoreMenuClicked _ ->
            ( model, Cmd.none )


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
    Layout.view { closeDrawerModal = CloseDrawerModal }
        { appbar = Appbar.view { menuClicked = OpenDrawerModal }
        , drawer = drawerCP model
        , main = mainCP model
        }
        model.isDrawerModalOpen


panelDragSystem : Drawer.PanelDragSystems Msg
panelDragSystem =
    Drawer.createPanelDragSystem DrawerPanelDrag DrawerPanelDragComplete


drawerConfig : Drawer.Config Msg
drawerConfig =
    { onToggleExpansionPanel = ToggleDrawerExpansionPanel
    , panelDragSystem = panelDragSystem
    , onPanelItemMoreMenuClicked = PanelItemMoreMenuClicked
    }


drawerCP : Model -> { content : List (Html Msg), portal : List (Html Msg) }
drawerCP model =
    Drawer.view drawerConfig
        { projects = ProjectCollection.sorted model.projectCollection
        , labels = LabelCollection.sorted model.labelCollection
        , filters = FilterCollection.sorted model.filterCollection
        }
        model.drawerPanelState


mainCP : Model -> View (Html Msg)
mainCP model =
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
todoListByFilterIdView filterId pc lc todoDict =
    [ TodoView.viewList { toggle = ToggleTodoCompleted } pc lc (TodoDict.sortedByIdx todoDict)
    ]



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



-- PLAYGROUND


type Popup
    = ProjectPopup ProjectId
    | LabelPopup LabelId
    | FilterPopup FilterId
