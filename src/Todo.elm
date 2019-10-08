module Todo exposing (Todo, fromTitle, title, view, viewList)

import Html exposing (ol, text)
import Html.Attributes exposing (class)


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


li : List (Html.Html msg) -> Html.Html msg
li =
    Html.li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]


view : Todo -> Html.Html msg
view todo =
    li [ text <| title todo ]


viewList : List Todo -> Html.Html msg
viewList =
    listContainer << List.map view


listContainer =
    ol [ class "list pl0" ]
