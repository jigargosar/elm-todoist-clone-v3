port module Main exposing (main)

import Appbar
import Browser
import Drag exposing (Drag)
import Drawer exposing (FilterView, LabelView)
import Html.Styled exposing (Html, toUnstyled)
import Json.Decode as JD
import Json.Encode exposing (Value)
import LabelCollection exposing (LabelCollection)
import Layout
import ProjectCollection exposing (ProjectCollection)
import Return
import Todo
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)


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
    , labelList : List LabelView
    , filterList : List FilterView
    , isDrawerModalOpen : Bool
    , drawerExpansionPanels : Drawer.ExpansionPanels
    , drawerPanelDrag : Drawer.PanelsDragState
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initial : Model
        initial =
            { todoDict = TodoDict.initial
            , projectCollection = ProjectCollection.initial
            , labelCollection = LabelCollection.initial
            , labelList = Drawer.labelList
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
                    ( { model | labelList = rotate model.labelList }
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
        , main = { content = mainView model.todoDict, portal = [] }
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
        , labelList = model.labelList
        , filterList = model.filterList
        }
        model.drawerExpansionPanels
        model.drawerPanelDrag


mainView : TodoDict -> List (Html Msg)
mainView todoDict =
    [ Todo.viewList { toggle = ToggleTodoCompleted } (TodoDict.sortedByIdx todoDict)
    ]



--
-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
