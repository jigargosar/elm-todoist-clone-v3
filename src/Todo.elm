module Todo exposing (Todo, generatorFromTitle, title, viewList)

import Emoji
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import TodoId exposing (TodoId)



-- MODEL


type Todo
    = Todo Internal


type alias Internal =
    { id : TodoId
    , title : String
    , isCompleted : Bool
    }


generatorFromTitle : String -> Random.Generator Todo
generatorFromTitle title_ =
    TodoId.generator
        |> Random.map (fromTitle title_)


fromTitle : String -> TodoId -> Todo
fromTitle title_ id_ =
    Todo <| { id = id_, title = title_, isCompleted = False }


title : Todo -> String
title =
    unwrap >> .title


unwrap (Todo m) =
    m


isCompleted =
    unwrap >> .isCompleted


id =
    unwrap >> .id



-- VIEW


type alias Config msg =
    { toggle : TodoId -> msg }


view : Config msg -> Todo -> Html msg
view config todo =
    li [ viewDoneCheck config todo, span [] [ text <| title todo ] ]


viewDoneCheck : Config msg -> Todo -> Html msg
viewDoneCheck config todo =
    let
        emoji =
            if isCompleted todo then
                Emoji.heavy_check_mark

            else
                Emoji.heavy_large_circle

        toggle =
            config.toggle <| id todo
    in
    button [ class "pa1 lh-solid bn bg-inherit color-inherit", onClick toggle ]
        [ text emoji
        ]


li : List (Html msg) -> Html msg
li =
    Html.li [ class "lh-copy pv3 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]


viewList : Config msg -> List Todo -> Html msg
viewList config =
    listContainer << List.map (view config)


listContainer =
    ol [ class "list pl0" ]
