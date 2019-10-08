module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


todoList =
    [ "Get Milk", "Remember to call", "Do Stuff!", "And More" ]


foo =
    onClick


buttonClass =
    """
    f6 f5-ns fw6 dib
    ba br-pill b--black-20
    bg-blue white
    ph3 ph4-ns pv2 pv3-ns
    grow no-underline
    """


main =
    div []
        [ header [ class "pa4" ]
            [ button
                [ class buttonClass, class "mr2" ]
                [ text <| String.fromChar heavy_plus_sign ]
            , button
                [ class buttonClass, class "mr2" ]
                [ text "+" ]
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
