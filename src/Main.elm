module Main exposing (..)

import Appbar
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Random
import Todo exposing (Todo)
import TodoId exposing (TodoId)



-- MODEL


type alias Model =
    {}


init _ =
    ( {}, Cmd.none )


getTodoList : Model -> List Todo
getTodoList _ =
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]
        |> List.map Todo.generatorFromTitle
        |> List.foldr (Random.map2 (::)) (Random.constant [])
        |> flip Random.step (Random.initialSeed 0)
        |> Tuple.first


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
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Appbar.view
        , main_ [ class "measure center" ]
            [ Todo.viewList { toggle = Toggle } (getTodoList model)
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
