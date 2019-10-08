module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


todoList =
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]


foo =
    onClick


main =
    ol [ class "list pl0 measure center" ]
        (List.map viewTodoListItem todoList)


viewTodoListItem title =
    li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ] [ text title ]
