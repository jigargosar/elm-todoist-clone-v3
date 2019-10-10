port module Main exposing (main)

import Appbar
import Browser
import Html exposing (..)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Lens exposing (Lens)
import Screen exposing (Screen)
import Sidebar
import Todo exposing (Todo)
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)


port logError : String -> Cmd msg



-- Flags


type alias Flags =
    { todoList : Value }



-- MODEL


type alias Model =
    { todoDict : TodoDict
    , screen : Screen
    }


screenSystem : Screen.System Msg Model
screenSystem =
    Screen.system screenL (\_ -> NoOp) (\_ _ -> NoOp)


initial : Model
initial =
    { todoDict = TodoDict.initial
    , screen = screenSystem.model
    }


init : Flags -> ( Model, Cmd msg )
init flags =
    let
        ( todoDict, todoDictDecodeCmds ) =
            case TodoDict.fromEncodedList flags.todoList of
                Ok todoDict_ ->
                    ( todoDict_, Cmd.none )

                Err e ->
                    ( TodoDict.initial, logError <| JD.errorToString e )
    in
    ( Lens.set todoDictL todoDict initial
    , todoDictDecodeCmds
    )


todoListSortedByIdx : { a | todoDict : TodoDict } -> List Todo
todoListSortedByIdx =
    .todoDict >> TodoDict.sortedByIdx



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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Toggle todoId ->
            ( mapTodoDict (TodoDict.toggleCompleted todoId) model, Cmd.none )

        Screen msg ->
            screenSystem.update msg model


screenL : Lens Screen Model
screenL =
    Lens.init { get = .screen, set = \s b -> { b | screen = s } }


todoDictL : Lens TodoDict Model
todoDictL =
    Lens.init { get = .todoDict, set = \s b -> { b | todoDict = s } }


mapTodoDict func model =
    { model | todoDict = func model.todoDict }


setTodoDict =
    Lens.set todoDictL



-- VIEW


view : Model -> Html Msg
view model =
    screenSystem.view Appbar.view Sidebar.view (mainView model) model


mainView model =
    [ Todo.viewList { toggle = Toggle } (todoListSortedByIdx model)
    ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
