port module Main exposing (main)

import Appbar
import Browser
import Drag exposing (Drag)
import Drawer
import Html.Styled exposing (Html, toUnstyled)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Layout
import Project exposing (Project)
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
    }



-- MODEL


type alias Model =
    { todoDict : TodoDict
    , projectCollection : ProjectCollection
    , isDrawerModalOpen : Bool
    , drawerExpansionPanels : Drawer.ExpansionPanels
    , projectsDrag : Drag
    , labelsDrag : Drag
    , filtersDrag : Drag
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initial : Model
        initial =
            { todoDict = TodoDict.initial
            , projectCollection = ProjectCollection.initial
            , isDrawerModalOpen = False
            , drawerExpansionPanels = Drawer.initialExpansionPanels
            , projectsDrag = Drag.initial
            , labelsDrag = Drag.initial
            , filtersDrag = Drag.initial
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



-- DND


projectsDragSystem : Drag.System Project Msg
projectsDragSystem =
    Drag.system ProjectsDrag


labelsDragSystem : Drag.System a Msg
labelsDragSystem =
    Drag.system LabelsDrag


filterDragSystem : Drag.System a Msg
filterDragSystem =
    Drag.system FiltersDrag



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Drag.subscriptions ProjectsDrag model.projectsDrag
        , Drag.subscriptions LabelsDrag model.labelsDrag
        , Drag.subscriptions FiltersDrag model.filtersDrag
        ]



-- UPDATE


type Msg
    = NoOp
    | ToggleTodoCompleted TodoId
    | OpenDrawerModal
    | CloseDrawerModal
    | ToggleDrawerExpansionPanel Drawer.Panel
    | ProjectsDrag Drag.Msg
    | LabelsDrag Drag.Msg
    | FiltersDrag Drag.Msg


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

        ProjectsDrag msg ->
            Drag.update ProjectsDrag msg model.projectsDrag
                |> Tuple.mapFirst (\drag -> { model | projectsDrag = drag })

        LabelsDrag msg ->
            Drag.update LabelsDrag msg model.labelsDrag
                |> Tuple.mapFirst (\drag -> { model | labelsDrag = drag })

        FiltersDrag msg ->
            Drag.update FiltersDrag msg model.filtersDrag
                |> Tuple.mapFirst (\drag -> { model | filtersDrag = drag })



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
            , projectsDragSystem = projectsDragSystem
            , labelsDragSystem = labelsDragSystem
            , filtersDragSystem = filterDragSystem
            }
    in
    Drawer.view drawerConfig
        (ProjectCollection.sorted model.projectCollection)
        model.drawerExpansionPanels
        { projectsDrag = model.projectsDrag
        , filtersDrag = model.filtersDrag
        , labelsDrag = model.labelsDrag
        }


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
