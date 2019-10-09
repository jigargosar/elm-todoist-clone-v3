module Main exposing (main)

import Appbar
import Basics.More exposing (flip)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (Generator)
import Random.More as Random
import Screen exposing (Screen)
import Timestamp
import Todo exposing (Todo)
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)



-- MODEL


type alias Model =
    { todoDict : TodoDict
    , screen : Screen
    }


screen =
    Screen.system (\_ -> NoOp) (\_ _ -> NoOp)


emptyModel : Model
emptyModel =
    { todoDict = TodoDict.empty
    , screen = screen.model
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { emptyModel | todoDict = TodoDict.fromList mockTodoList }, Cmd.none )


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



-- UPDATE


type Msg
    = NoOp
    | Toggle TodoId


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Toggle todoId ->
            ( mapTodoDict (TodoDict.toggleCompleted todoId) model, Cmd.none )


mapTodoDict func model =
    { model | todoDict = func model.todoDict }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Appbar.view
        , main_ [ class "measure center" ]
            [ Todo.viewList { toggle = Toggle } (todoList model)
            ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
