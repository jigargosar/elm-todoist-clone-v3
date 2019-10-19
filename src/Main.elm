port module Main exposing (main)

import Appbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Drag exposing (Drag)
import Drawer exposing (FilterView, LabelView)
import Html.Styled exposing (Html, toUnstyled)
import Json.Decode as JD
import Json.Encode exposing (Value)
import LabelCollection exposing (LabelCollection)
import Layout
import ProjectCollection exposing (ProjectCollection)
import Return
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)
import TodoView
import Url exposing (Url)


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
    { todoDict : TodoDict
    , projectCollection : ProjectCollection
    , labelCollection : LabelCollection
    , filterList : List FilterView
    , isDrawerModalOpen : Bool
    , drawerExpansionPanels : Drawer.ExpansionPanels
    , drawerPanelDrag : Drawer.PanelsDragState
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        initial : Model
        initial =
            { todoDict = TodoDict.initial
            , projectCollection = ProjectCollection.initial
            , labelCollection = LabelCollection.initial
            , filterList = Drawer.filterList
            , isDrawerModalOpen = False
            , drawerExpansionPanels = Drawer.initialExpansionPanels
            , drawerPanelDrag = Drawer.initialPanelsDragState
            }
    in
    Return.singleton initial
        |> Return.andThen
            (Return.pipelK
                [ initTodoDict flags.todoList
                , initProjectCollection flags.projectList
                , initLabelCollection flags.labelList
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
        [ Drawer.panelDragSubscriptions DrawerPanelDrag model.drawerPanelDrag
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            Return.singleton model

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
                | drawerExpansionPanels =
                    Drawer.toggleExpansionPanel panel model.drawerExpansionPanels
              }
            , Cmd.none
            )

        DrawerPanelDrag panel msg ->
            Drawer.updatePanelDrag DrawerPanelDrag DrawerPanelDragComplete panel msg model.drawerPanelDrag
                |> Tuple.mapFirst
                    (\drawerPanelDrag -> { model | drawerPanelDrag = drawerPanelDrag })

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
                    ( { model | filterList = rotate model.filterList }
                    , Cmd.none
                    )



-- VIEW


view : Model -> Html Msg
view model =
    Layout.view { closeDrawerModal = CloseDrawerModal }
        { appbar = Appbar.view { menuClicked = OpenDrawerModal }
        , drawer = drawerView model
        , main =
            { content =
                mainView model.projectCollection model.labelCollection model.todoDict
            , portal = []
            }
        }
        model.isDrawerModalOpen


drawerView : Model -> { content : List (Html Msg), portal : List (Html Msg) }
drawerView model =
    let
        drawerConfig : Drawer.Config Msg
        drawerConfig =
            { onToggleExpansionPanel = ToggleDrawerExpansionPanel
            , panelToDragMsg = DrawerPanelDrag
            , panelToDragCompleteMsg = DrawerPanelDragComplete
            }
    in
    Drawer.view drawerConfig
        { projectList = ProjectCollection.sorted model.projectCollection
        , labelList = LabelCollection.sorted model.labelCollection
        , filterList = model.filterList
        }
        model.drawerExpansionPanels
        model.drawerPanelDrag


mainView : ProjectCollection -> LabelCollection -> TodoDict -> List (Html Msg)
mainView pc lc todoDict =
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
