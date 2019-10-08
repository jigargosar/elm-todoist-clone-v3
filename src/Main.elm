module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


todoList =
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]


main =
    div []
        [ toolbar [ search, filler, addIconBtn ]
        , main_ [ class "measure center" ]
            [ todoListC (List.map viewTodoListItem todoList)
            ]
        ]


todoListC =
    ol [ class "list pl0 measure center" ]


viewTodoListItem title =
    todoListLi [ text title ]


heavy_plus_sign =
    "➕"


magnifying_glass =
    "🔍"


filler =
    div [ class "flex-grow-1" ] []


toolbar c =
    div [ class "ph2 pv1 bg-black white bn shadow-1" ]
        [ div [ class "measure center flex" ] c ]


addIconBtn =
    button [ class "pa1 lh-solid bn bg-inherit color-inherit" ]
        [ text heavy_plus_sign
        ]


search =
    input
        [ class "pa1 br2 bn"
        , placeholder <| magnifying_glass ++ " Search"
        ]
        []


todoListLi =
    li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]
