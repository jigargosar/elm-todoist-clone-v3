module Main exposing (..)

import Html exposing (..)


todoList =
    [ "Get Milk", "Remember to call", "Do Stuff!" ]


main =
    div []
        (List.map viewTodoListItem todoList)


viewTodoListItem title =
    div [] [ text title ]
