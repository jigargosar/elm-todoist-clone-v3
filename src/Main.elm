module Main exposing (..)

import Emoji
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import UI


todoList =
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]


main =
    div []
        [ UI.topBar [ UI.search, UI.filler, UI.addIconBtn ]
        , main_ [ class "measure center" ]
            [ todoListC (List.map viewTodoListItem todoList)
            ]
        ]


todoListC =
    ol [ class "list pl0" ]


viewTodoListItem title =
    todoListLi [ text title ]


todoListLi =
    li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]
