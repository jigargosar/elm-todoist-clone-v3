module Main exposing (main)

import Appbar
import Basics.More exposing (flip)
import Browser
import Html exposing (..)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Random exposing (Generator)
import Random.More as Random
import Screen exposing (Screen)
import Sidebar
import Timestamp
import Todo exposing (Todo)
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)



-- Flags


type alias Flags =
    { todoList : Value }



-- MODEL


type alias Model =
    { todoDict : TodoDict
    , screen : Screen
    }


screenSystem : Screen.System Msg
screenSystem =
    Screen.system (\_ -> NoOp) (\_ _ -> NoOp)


emptyModel : Model
emptyModel =
    { todoDict = TodoDict.empty
    , screen = screenSystem.model
    }


init : Flags -> ( Model, Cmd msg )
init flags =
    let
        todoListResult =
            JD.decodeValue TodoDict.fromEncodedList flags.todoList
    in
    ( { emptyModel
        | todoDict = TodoDict.fromList mockTodoList
      }
    , Cmd.none
    )


mockTodoList : List Todo
mockTodoList =
    let
        ts =
            Timestamp.zero

        todoGenerator : String -> Generator Todo
        todoGenerator title =
            Random.map2 (\isCompleted -> Todo.mapCompleted (always isCompleted))
                Random.mostlyFalse
                (Todo.fromTitleAndTimestamp title ts)
    in
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]
        |> List.map todoGenerator
        |> Random.fromList
        |> flip Random.step (Random.initialSeed 0)
        |> Tuple.first
        |> List.indexedMap (always >> Todo.mapIdx)


todoList : Model -> List Todo
todoList =
    .todoDict
        >> TodoDict.toList
        >> List.sortBy Todo.idx



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ screenSystem.subscriptions model.screen
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
            screenSystem.update msg model.screen
                |> Tuple.mapFirst (setScreenIn model)


setScreenIn model screen =
    { model | screen = screen }


mapTodoDict func model =
    { model | todoDict = func model.todoDict }



-- VIEW


view : Model -> Html Msg
view model =
    screenSystem.view Appbar.view Sidebar.view (mainView model) model.screen


mainView model =
    [ Todo.viewList { toggle = Toggle } (todoList model)
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
