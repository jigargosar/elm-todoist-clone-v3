module Todo exposing (Todo, fromTitle, li, title)

import Html
import Html.Attributes exposing (class)


type Todo
    = Todo Internal


type alias Internal =
    { title : String
    }


fromTitle title_ =
    Todo <| { title = title_ }


title : Todo -> String
title =
    unwrap >> .title


unwrap (Todo i) =
    i


li : List (Html.Html msg) -> Html.Html msg
li =
    Html.li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]
