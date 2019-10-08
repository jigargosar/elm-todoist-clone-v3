module Todo exposing (Todo, fromTitle, title)


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
