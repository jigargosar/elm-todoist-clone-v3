port module Main exposing (main)

import Appbar
import Browser
import Drawer exposing (Drawer)
import Html.Styled exposing (Html, toUnstyled)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Layout exposing (Layout)
import Lens
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
            res |> Tuple.mapFirst (todoDictL.setIn big)
    , toggle =
        \todoId big ->
            ( todoDictL.map (TodoDict.toggleCompleted todoId) big, Cmd.none )
    }


projectsSystem : { init : Value -> Model -> ( Model, Cmd msg ) }
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
            , drawer = Drawer.initial
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
        ]



-- UPDATE


type Msg
    = NoOp
    | Toggle TodoId
    | Screen Screen.Msg
    | Layout Layout.Msg
    | Drawer Drawer.Msg


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


updateDrawer : Drawer.Msg -> Model -> ( Model, Cmd Msg )
updateDrawer msg model =
    Drawer.update Drawer msg model.drawer
        |> Tuple.mapFirst (\s -> { model | drawer = s })


updateLayout : Layout.Msg -> { a | layout : Layout } -> ( { a | layout : Layout }, Cmd Msg )
updateLayout msg big =
    Layout.update Layout msg big.layout
        |> Tuple.mapFirst (\s -> { big | layout = s })



-- VIEW


view : Model -> Html Msg
view model =
    Layout.view Layout
        { appbar = Appbar.view { onMenu = Layout Layout.openDrawer }
        , drawer = Drawer.view Drawer model.drawer
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
