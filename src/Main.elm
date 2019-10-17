port module Main exposing (main)

import Appbar
import Browser
import Drawer exposing (Drawer)
import Html.Styled exposing (Html, toUnstyled)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Layout
import Lens
import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import Return
import Todo exposing (Todo)
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
    , drawer : Drawer
    }


drawerSystem : Drawer.System Msg Model
drawerSystem =
    Drawer.system Drawer
        { onProjectListSorted = UpdateProjectSortOrder }
        projectsSystem.sorted
        (Lens.system { get = .drawer, set = \s b -> { b | drawer = s } })



--todoDictSystem =
--    let
--        todoDictL : Lens.Lens TodoDict Model
--        todoDictL =
--            Lens.system { get = .todoDict, set = \s b -> { b | todoDict = s } }
--    in
--    { sortedByIdx = todoDictL.get >> TodoDict.sortedByIdx
--    , init =
--        \encoded big ->
--            let
--                res =
--                    case TodoDict.fromEncodedList encoded of
--                        Ok todoDict_ ->
--                            ( todoDict_, Cmd.none )
--
--                        Err e ->
--                            ( TodoDict.initial, logError <| JD.errorToString e )
--            in
--            res |> Tuple.mapFirst (\s -> todoDictL.set s big)
--    , toggle =
--        \todoId big ->
--            ( Lens.map todoDictL (TodoDict.toggleCompleted todoId) big, Cmd.none )
--    }
--


projectsSystem =
    let
        lens : Lens.Lens ProjectCollection Model
        lens =
            Lens.system { get = .projectCollection, set = \s b -> { b | projectCollection = s } }
    in
    { init =
        \encoded ->
            let
                func old =
                    case ProjectCollection.fromEncodedList encoded of
                        Ok new ->
                            ( new, Cmd.none )

                        Err e ->
                            ( old, logError <| JD.errorToString e )
            in
            Lens.update lens func
    , sorted = lens.get >> ProjectCollection.sorted
    , updateSortOrder =
        \pl big ->
            ( Lens.map lens (ProjectCollection.updateSortOrder pl) big, Cmd.none )
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initial : Model
        initial =
            { todoDict = TodoDict.initial
            , projectCollection = ProjectCollection.initial
            , isDrawerModalOpen = False
            , drawer = drawerSystem.initial
            }
    in
    Return.singleton initial
        |> Return.andThen
            (Return.pipelK
                [ initTodoDict flags.todoList
                , projectsSystem.init flags.projectList
                ]
            )


initTodoDict encodedTodoList model =
    let
        ( newTodoDict, cmd ) =
            case TodoDict.fromEncodedList encodedTodoList of
                Ok todoDict_ ->
                    ( todoDict_, Cmd.none )

                Err e ->
                    ( TodoDict.initial, logError <| JD.errorToString e )
    in
    ( { model | todoDict = newTodoDict }, cmd )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ drawerSystem.subscriptions model
        ]



-- UPDATE


type Msg
    = NoOp
    | ToggleTodoCompleted TodoId
    | OpenDrawerModal
    | CloseDrawerModal
    | Drawer Drawer.Msg
    | UpdateProjectSortOrder (List Project)


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

        Drawer msg ->
            updateDrawer msg model

        UpdateProjectSortOrder projectList ->
            projectsSystem.updateSortOrder projectList model


updateDrawer : Drawer.Msg -> Model -> ( Model, Cmd Msg )
updateDrawer msg model =
    drawerSystem.update msg model



-- VIEW


view : Model -> Html Msg
view model =
    Layout.view { closeDrawerModal = CloseDrawerModal }
        { appbar = Appbar.view { onMenu = OpenDrawerModal }
        , drawer = drawerSystem.view model
        , content = mainView model.todoDict
        }
        model.isDrawerModalOpen


mainView : TodoDict -> List (Html Msg)
mainView todoDict =
    [ Todo.viewList { toggle = ToggleTodoCompleted } (TodoDict.sortedByIdx todoDict)
    ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
