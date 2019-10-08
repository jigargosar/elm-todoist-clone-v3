module Todo exposing (Todo, fromTitle, title, viewList)

import Emoji
import Html exposing (..)
import Html.Attributes exposing (..)
import UI



-- MODEL


type Todo
    = Todo Internal


type alias Internal =
    { title : String
    , isCompleted : Bool
    }


fromTitle title_ =
    Todo <| { title = title_, isCompleted = False }


title : Todo -> String
title =
    unwrap >> .title


unwrap (Todo m) =
    m


isCompleted =
    unwrap >> .isCompleted



-- VIEW


view : Todo -> Html msg
view todo =
    li [ viewDoneCheck <| isCompleted todo, span [] [ text <| title todo ] ]


viewDoneCheck isChecked =
    let
        emoji =
            if isChecked then
                Emoji.heavy_check_mark

            else
                Emoji.heavy_large_circle
    in
    button [ class "pa1 lh-solid bn bg-inherit color-inherit" ]
        [ text emoji
        ]


li : List (Html msg) -> Html msg
li =
    Html.li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]


viewList : List Todo -> Html msg
viewList =
    listContainer << List.map view


listContainer =
    ol [ class "list pl0" ]
