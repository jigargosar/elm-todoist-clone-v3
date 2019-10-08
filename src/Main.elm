module Main exposing (..)

import Browser
import Emoji
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Todo exposing (Todo)
import UI



-- MODEL


type alias Model =
    {}


init _ =
    ( {}, Cmd.none )


getTodoList : Model -> List Todo
getTodoList _ =
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]
        |> List.map Todo.fromTitle



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view model =
    div []
        [ UI.topBar [ UI.search, UI.filler, UI.addIconBtn ]
        , main_ [ class "measure center" ]
            [ todoListC (List.map viewTodoListItem (getTodoList model))
            ]
        ]


todoListC =
    ol [ class "list pl0" ]


viewTodoListItem todo =
    todoListLi [ text <| Todo.title todo ]


todoListLi =
    li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }
