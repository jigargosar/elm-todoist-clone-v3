module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


todoList =
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]


main =
    div []
        [ toolbar [ filler, addIconBtn ]
        , main_ [ class "measure center" ]
            [ ol [ class "list pl0 measure center" ]
                (List.map viewTodoListItem todoList)
            ]
        ]


heavy_plus_sign =
    '➕'


filler =
    div [ class "flex-grow-1" ] []


toolbar c =
    div [ class "ph2 pv1 bg-black white bn shadow-1" ]
        [ div [ class "measure center flex" ] c ]


addIconBtn =
    button [ class "pa1 lh-solid bn bg-inherit color-inherit" ]
        [ text <| String.fromChar heavy_plus_sign
        ]


viewTodoListItem title =
    li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ] [ text title ]
