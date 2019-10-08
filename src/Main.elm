module Main exposing (..)

import Appbar
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (Generator)
import Timestamp
import Todo exposing (Todo)
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)



-- MODEL


type alias Model =
    { todoDict : TodoDict }


emptyModel : Model
emptyModel =
    { todoDict = TodoDict.empty
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { todoDict = TodoDict.fromList (generateInitialTodoList emptyModel) }, Cmd.none )


generateInitialTodoList : Model -> List Todo
generateInitialTodoList _ =
    let
        ts =
            Timestamp.zero

        boolGen : Generator Bool
        boolGen =
            Random.weighted ( 2, True ) [ ( 1, False ) ]

        toggleGen : Todo -> Generator Todo
        toggleGen todo =
            boolGen |> Random.map (always >> flip Todo.mapCompleted todo)

        gen : String -> Generator Todo
        gen title =
            Todo.generatorFromTitle title ts
                |> Random.andThen toggleGen
    in
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]
        |> List.map gen
        |> List.foldr (Random.map2 (::)) (Random.constant [])
        |> flip Random.step (Random.initialSeed 0)
        |> Tuple.first
        |> List.indexedMap (\idx -> Todo.mapIdx (always idx) >> Todo.toggle)


todoList =
    .todoDict
        >> TodoDict.toList
        >> List.sortBy Todo.idx


flip : (c -> b -> a) -> b -> c -> a
flip func b a =
    func a b



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
