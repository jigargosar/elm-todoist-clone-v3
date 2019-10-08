module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


todoList =
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]


main =
    div [ class "measure center" ]
        [ header [ class "pa0" ]
            [ button [ class "pa1 lh-solid ma0 bn bg-white" ]
                [ text <| String.fromChar heavy_plus_sign
                ]
            ]
        , main_ []
            [ ol [ class "list pl0 measure center" ]
                (List.map viewTodoListItem todoList)
            ]
        ]


heavy_plus_sign =
    '➕'


viewTodoListItem title =
    li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ] [ text title ]
