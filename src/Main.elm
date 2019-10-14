port module Main exposing (main)

import Appbar
import Browser
import Drawer exposing (Drawer)
import Html.Styled exposing (Html, toUnstyled)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Layout exposing (Layout)
import Lens
import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import Return
import Screen exposing (Screen)
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
    , screen : Screen
    , layout : Layout
    , drawer : Drawer
    }


drawerSystem : Drawer.System Msg Model
drawerSystem =
    Drawer.system Drawer
        { onProjectListSorted = UpdateProjectSortOrder }
        projectsSystem.sorted
        (Lens.system { get = .drawer, set = \s b -> { b | drawer = s } })


screenSystem : Screen.System Msg Model
screenSystem =
    let
        screenL : Lens.System Screen Model
        screenL =
            Lens.system { get = .screen, set = \s b -> { b | screen = s } }
    in
    Screen.system screenL (\_ -> NoOp) (\_ _ -> NoOp)


todoDictSystem =
    let
        todoDictL : Lens.System TodoDict Model
        todoDictL =
            Lens.system { get = .todoDict, set = \s b -> { b | todoDict = s } }
    in
    { sortedByIdx = todoDictL.get >> TodoDict.sortedByIdx
    , init =
        \encoded big ->
            let
                res =
                    case TodoDict.fromEncodedList encoded of
                        Ok todoDict_ ->
                            ( todoDict_, Cmd.none )

                        Err e ->
                            ( TodoDict.initial, logError <| JD.errorToString e )
            in
            res |> Tuple.mapFirst (\s -> todoDictL.set s big)
    , toggle =
        \todoId big ->
            ( Lens.map todoDictL (TodoDict.toggleCompleted todoId) big, Cmd.none )
    }


projectsSystem =
    let
        lens : Lens.System ProjectCollection Model
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
            , screen = screenSystem.initial
            , layout = Layout.initial
            , drawer = drawerSystem.initial
            }
    in
    Return.singleton initial
        |> Return.andThen
            (Return.pipelK
                [ todoDictSystem.init flags.todoList
                , projectsSystem.init flags.projectList
                , screenSystem.init
                ]
            )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ screenSystem.subscriptions model
        , drawerSystem.subscriptions model
        ]



-- UPDATE


type Msg
    = NoOp
    | Toggle TodoId
    | Screen Screen.Msg
    | Layout Layout.Msg
    | Drawer Drawer.Msg
    | UpdateProjectSortOrder (List Project)


update : Msg -> Model -> ( Model, Cmd Msg )
update message =
    case message of
        NoOp ->
            Return.singleton

        Toggle todoId ->
            todoDictSystem.toggle todoId

        Screen msg ->
            screenSystem.update msg

        Layout msg ->
            updateLayout msg

        Drawer msg ->
            updateDrawer msg

        UpdateProjectSortOrder projectList ->
            projectsSystem.updateSortOrder projectList


updateDrawer : Drawer.Msg -> Model -> ( Model, Cmd Msg )
updateDrawer msg model =
    drawerSystem.update msg model


updateLayout : Layout.Msg -> { a | layout : Layout } -> ( { a | layout : Layout }, Cmd Msg )
updateLayout msg big =
    Layout.update Layout msg big.layout
        |> Tuple.mapFirst (\s -> { big | layout = s })



-- VIEW


view : Model -> Html Msg
view model =
    Layout.view Layout
        { appbar = Appbar.view { onMenu = Layout Layout.openDrawer }
        , drawer = drawerSystem.view model
        , content = mainView model
        }
        model.layout


mainView model =
    [ Todo.viewList { toggle = Toggle } (todoDictSystem.sortedByIdx model)
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
