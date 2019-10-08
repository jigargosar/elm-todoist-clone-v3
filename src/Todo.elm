module Todo exposing (Todo, fromTitleAndTimestamp, id, idx, mapCompleted, mapIdx, title, toggle, viewList)

import Emoji
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Time exposing (Posix)
import Timestamp exposing (Timestamp)
import TodoId exposing (TodoId)



-- MODEL


type Todo
    = Todo Internal


type alias Internal =
    { id : TodoId
    , createdAt : Timestamp
    , modifiedAt : Timestamp
    , title : String
    , isCompleted : Bool
    , idx : Int
    }


fromTitleAndTimestamp : String -> Posix -> Random.Generator Todo
fromTitleAndTimestamp title_ timestamp =
    TodoId.generator
        |> Random.map (fromTitle title_ timestamp)


fromTitle : String -> Posix -> TodoId -> Todo
fromTitle title_ timestamp id_ =
    Todo <|
        { id = id_
        , createdAt = timestamp
        , modifiedAt = timestamp
        , title = title_
        , isCompleted = False
        , idx = 0
        }


title : Todo -> String
title =
    unwrap >> .title


unwrap (Todo t) =
    t


isCompleted =
    unwrap >> .isCompleted


id : Todo -> TodoId
id =
    unwrap >> .id


idx : Todo -> Int
idx =
    unwrap >> .idx



-- UPDATE


map : (Internal -> Internal) -> Todo -> Todo
map func =
    unwrap >> func >> Todo


mapCompleted : (Bool -> Bool) -> Todo -> Todo
mapCompleted func =
    map (\t -> { t | isCompleted = func t.isCompleted })


toggle : Todo -> Todo
toggle =
    mapCompleted not


mapIdx : (Int -> Int) -> Todo -> Todo
mapIdx func =
    map (\t -> { t | idx = func t.idx })



-- VIEW


type alias Config msg =
    { toggle : TodoId -> msg }


viewList : Config msg -> List Todo -> Html msg
viewList config =
    listContainer << List.map (viewListItem config)


listContainer =
    ol [ class "list pl0" ]


viewListItem : Config msg -> Todo -> Html msg
viewListItem config todo =
    li [ viewIsCompleted config todo, viewTitle todo ]


li : List (Html msg) -> Html msg
li =
    Html.li [ class "flex items-center lh-copy pv1 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]


viewIsCompleted : { a | toggle : TodoId -> msg } -> Todo -> Html msg
viewIsCompleted config todo =
    let
        emoji =
            if isCompleted todo then
                Emoji.heavy_check_mark

            else
                Emoji.heavy_large_circle

        toggleMsg =
            config.toggle <| id todo
    in
    button [ class "pa2 bn bg-inherit color-inherit", onClick toggleMsg ]
        [ Emoji.view emoji ]


viewTitle : Todo -> Html msg
viewTitle todo =
    div [ class "pa2 flex-grow-1" ] [ text <| title todo ]
