module Main exposing (main)

import Appbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Dialog exposing (Dialog)
import Drag exposing (Drag)
import Drawer
import Filter exposing (Filter)
import FilterCollection exposing (FilterCollection)
import FilterId exposing (FilterId)
import Html.Styled as H exposing (Html, toUnstyled)
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



-- POPUP


type alias PopupKind =
    Drawer.PanelItemId



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
    , projectsExpanded : Bool
    , labelsExpanded : Bool
    , filtersExpanded : Bool
    , panelDrag : Maybe ( Drawer.Panel, Drag )
    , popup : Maybe ( PopupKind, Popper )
    , dialog : Maybe Dialog
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
            , projectsExpanded = True
            , labelsExpanded = True
            , filtersExpanded = True
            , popup = Nothing
            , panelDrag = Nothing
            , dialog = Nothing
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
        [ case model.panelDrag of
            Just ( panel, drag ) ->
                Drag.subscriptions (DrawerPanelDrag panel) drag

            Nothing ->
                Sub.none
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
    | TogglePanel Drawer.Panel
    | PanelAddClicked Drawer.Panel
    | DrawerPanelDrag Drawer.Panel Drag.Msg
    | DrawerPanelDragComplete Drawer.Panel Drag.Info
    | PopupTriggered PopupKind String
    | Popper Popper.Msg
    | ClosePopup
    | ProjectMoreMenu PopupView.ProjectMenuItem
    | LabelMoreMenu PopupView.LabelMenuItem
    | FilterMoreMenu PopupView.FilterMenuItem
    | OpenDialog Dialog
    | CloseDialog


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

        TogglePanel panel ->
            ( case panel of
                Drawer.Projects ->
                    { model | projectsExpanded = not model.projectsExpanded }

                Drawer.Labels ->
                    { model | labelsExpanded = not model.labelsExpanded }

                Drawer.Filters ->
                    { model | filtersExpanded = not model.filtersExpanded }
            , Cmd.none
            )

        PanelAddClicked panel ->
            case panel of
                Drawer.Projects ->
                    ( { model | dialog = Just Dialog.initAddProject }, Cmd.none )

                Drawer.Labels ->
                    ( { model | dialog = Just Dialog.AddLabel }, Cmd.none )

                Drawer.Filters ->
                    ( { model | dialog = Just Dialog.AddFilter }, Cmd.none )

        DrawerPanelDrag panel msg ->
            Drag.update
                (DrawerPanelDrag panel)
                (DrawerPanelDragComplete panel)
                msg
                (dragForPanel panel model.panelDrag)
                |> Tuple.mapFirst
                    (\newDrag -> { model | panelDrag = Just ( panel, newDrag ) })

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
            ( closePopup model, Cmd.none )

        ProjectMoreMenu action ->
            case model.popup of
                Just ( Drawer.ProjectItemId projectId, _ ) ->
                    onProjectMoreMenuAction projectId action model

                _ ->
                    ( model, Cmd.none )

        LabelMoreMenu action ->
            case model.popup of
                Just ( Drawer.LabelItemId labelId, _ ) ->
                    onLabelMoreMenuAction labelId action model

                _ ->
                    ( model, Cmd.none )

        FilterMoreMenu action ->
            case model.popup of
                Just ( Drawer.FilterItemId filterId, _ ) ->
                    onFilterMoreMenuAction filterId action model

                _ ->
                    ( model, Cmd.none )

        OpenDialog dialog ->
            ( { model | dialog = Just dialog }, Cmd.none )

        CloseDialog ->
            ( { model | dialog = Nothing }, Cmd.none )


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


dragForPanel : a -> Maybe ( a, Drag ) -> Drag
dragForPanel panel panelDrag =
    case panelDrag of
        Nothing ->
            Drag.initial

        Just ( panel_, drag ) ->
            if panel_ == panel then
                drag

            else
                Drag.initial


isPanelExpanded : Drawer.Panel -> { a | projectsExpanded : c, labelsExpanded : c, filtersExpanded : c } -> c
isPanelExpanded panel model =
    case panel of
        Drawer.Projects ->
            model.projectsExpanded

        Drawer.Labels ->
            model.labelsExpanded

        Drawer.Filters ->
            model.filtersExpanded


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
        , drawer = View.concat [ drawerView model ]
        , main = pageView model
        }
        (View.concat [ popupView model, dialogView model ])
        model.isDrawerModalOpen


moreClickedDecoder : (id -> Drawer.PanelItemId) -> String -> id -> JD.Decoder Msg
moreClickedDecoder panelItemId anchorId id =
    let
        kind =
            panelItemId id

        msg =
            PopupTriggered kind anchorId
    in
    JD.succeed msg


panelConfig : Drawer.PanelConfig Msg
panelConfig =
    { toggle = TogglePanel
    , add = PanelAddClicked
    }


projectPanelItemConfig : Drawer.PanelItemConfig ProjectId Project Msg
projectPanelItemConfig =
    let
        panel =
            Drawer.Projects
    in
    { moreClicked = moreClickedDecoder Drawer.ProjectItemId
    , dragMsg = DrawerPanelDrag panel
    , panelId = "project"
    , iconName = "folder"
    , id = Project.id
    , idToString = ProjectId.toString
    , title = Project.title
    , route = Project.id >> Route.Project
    , iconStyle = Styles.c_ << Project.cssColor
    }


labelPanelItemConfig : Drawer.PanelItemConfig LabelId Label Msg
labelPanelItemConfig =
    let
        panel =
            Drawer.Labels
    in
    { moreClicked = moreClickedDecoder Drawer.LabelItemId
    , dragMsg = DrawerPanelDrag panel
    , panelId = "label"
    , id = Label.id
    , idToString = LabelId.toString
    , title = Label.title
    , route = Label.id >> Route.Label
    , iconName = "label"
    , iconStyle = Styles.c_ << Label.cssColor
    }


filterPanelItemConfig : Drawer.PanelItemConfig FilterId Filter Msg
filterPanelItemConfig =
    let
        panel =
            Drawer.Filters
    in
    { moreClicked = moreClickedDecoder Drawer.FilterItemId
    , dragMsg = DrawerPanelDrag panel
    , panelId = "filter"
    , id = Filter.id
    , idToString = FilterId.toString
    , title = Filter.title
    , route = Filter.id >> Route.Filter
    , iconName = "filter_list"
    , iconStyle = Styles.c_ << Filter.cssColor
    }


drawerView : Model -> View (Html Msg)
drawerView model =
    let
        panelView : Drawer.PanelItemConfig id item Msg -> Drawer.Panel -> List item -> View (Html Msg)
        panelView config panel items =
            Drawer.panelView panelConfig
                panel
                (isPanelExpanded panel model)
                (\_ ->
                    Drawer.viewPanelItems config
                        items
                        (dragForPanel panel model.panelDrag)
                )
    in
    View.concat
        [ Drawer.prefixNavItemsView
        , panelView projectPanelItemConfig
            Drawer.Projects
            (ProjectCollection.sorted model.projectCollection)
        , panelView labelPanelItemConfig
            Drawer.Labels
            (LabelCollection.sorted model.labelCollection)
        , panelView filterPanelItemConfig
            Drawer.Filters
            (FilterCollection.sorted model.filterCollection)
        ]


pageView : Model -> View (Html Msg)
pageView model =
    case model.page of
        Page.NotFound url ->
            Page.NotFound.view url

        Page.TodoListByProjectRef projectRef ->
            View.content <|
                mainView projectRef
                    model.projectCollection
                    model.labelCollection
                    model.todoDict

        Page.TodoListByLabelId labelId ->
            View.content <|
                todoListByLabelIdView
                    labelId
                    model.projectCollection
                    model.labelCollection
                    model.todoDict

        Page.TodoListByFilterId filterId ->
            View.content <|
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


popupView : Model -> View (Html Msg)
popupView model =
    case model.popup of
        Nothing ->
            View.none

        Just ( kind, popper ) ->
            let
                viewHelp : List (Html a) -> (a -> Msg) -> View (Html Msg)
                viewHelp content toMsg =
                    PopupView.container
                        { onClose = ClosePopup
                        , noOp = NoOp
                        }
                        (content |> List.map (H.map toMsg))
                        popper
            in
            case kind of
                Drawer.ProjectItemId _ ->
                    viewHelp PopupView.projectContent ProjectMoreMenu

                Drawer.LabelItemId _ ->
                    viewHelp PopupView.labelContent LabelMoreMenu

                Drawer.FilterItemId _ ->
                    viewHelp PopupView.filterContent FilterMoreMenu


dialogView : Model -> View (Html Msg)
dialogView model =
    case model.dialog of
        Just dialog ->
            Dialog.viewDialog { cancel = CloseDialog } dialog

        Nothing ->
            View.none



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
