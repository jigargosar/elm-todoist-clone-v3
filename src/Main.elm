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


screenS =
    Screen.system (\_ -> NoOp) (\_ _ -> NoOp)


emptyModel : Model
emptyModel =
    { todoDict = TodoDict.empty
    , screen = screenS.model
    }


init : () -> ( Model, Cmd msg )
init _ =
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
            screenS.update msg model.screen
                |> Tuple.mapFirst (setScreenIn model)


setScreenIn model screen =
    { model | screen = screen }


mapTodoDict func model =
    { model | todoDict = func model.todoDict }



-- VIEW


view : Model -> Html Msg
view model =
    let
        centeredContent attrs =
            div <|
                [ class "flex center w-100"
                , style "max-width" "800px"
                ]
                    ++ attrs
    in
    div [ class "flex flex-column bg-body", style "min-height" "100vh" ]
        [ header [ class "flex top-0 bg-light-red white ttu tracked w-100 h-header", style "position" "sticky" ]
            [ centeredContent [ class "pa3" ] [ text "header" ]
            ]
        , centeredContent [ class "flex-grow-1" ]
            [ aside
                [ class "fixed top-sidebar bottom-0 w-sidebar pa3 hover-overflow-y br b--main"
                , style "position" "fixed"
                ]
                [ div [ style "height" "200vh" ] [ text "aside sidebar" ] ]
            , main_
                [ class "ml-main pa3 flex-grow-1 bg-white br b--main"
                ]
                [ div [ style "height" "" ] [ text "main content" ]
                ]
            ]
        ]


view2 model =
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
