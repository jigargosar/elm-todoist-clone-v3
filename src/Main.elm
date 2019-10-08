module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


todoList =
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]


foo =
    onClick


main =
    ol []
        (List.map viewTodoListItem todoList)


viewTodoListItem title =
    div [ class "" ] [ text title ]
