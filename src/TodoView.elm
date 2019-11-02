module TodoView exposing (Config, viewList)

import Color
import Css
import Emoji
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Label
import LabelCollection exposing (LabelCollection)
import ProjectCollection exposing (ProjectCollection)
import Route
import Styles exposing (..)
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import TodoProject exposing (TodoProject)


type alias Config msg =
    { toggle : TodoId -> msg }


viewList : Config msg -> ProjectCollection -> LabelCollection -> List Todo -> Html msg
viewList config pc lc =
    listContainer << List.map (viewListItem config pc lc)


listContainer =
    ol [ class "list pl0 ma0" ]


viewListItem : Config msg -> ProjectCollection -> LabelCollection -> Todo -> Html msg
viewListItem config pc lc todo =
    li
        [ div [ css [ flex, itemsCenter ] ]
            [ viewIsCompleted config todo
            , viewTitle todo
            , viewProject pc todo
            ]
        , div [ css [ flex ] ] (viewLabels lc todo)
        ]


li : List (Html msg) -> Html msg
li =
    Html.li [ class "ph2 pv1 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]


viewIsCompleted : { a | toggle : TodoId -> msg } -> Todo -> Html msg
viewIsCompleted config todo =
    let
        emoji =
            if Todo.isCompleted todo then
                Emoji.heavy_check_mark

            else
                Emoji.heavy_large_circle

        toggleMsg =
            config.toggle <| Todo.id todo
    in
    Emoji.button toggleMsg emoji


viewTitle : Todo -> Html msg
viewTitle todo =
    div [ class "pa2 flex-grow-1" ] [ text <| Todo.title todo ]


viewProject : ProjectCollection -> Todo -> Html msg
viewProject pc todo =
    let
        todoProject : TodoProject
        todoProject =
            TodoProject.fromTodo pc todo
    in
    a
        [ css
            [ ph 1
            , lh 1.5
            , Css.fontSize Css.small
            , bg (toCssColor todoProject.color)
            , c_ (toCssColor <| Color.highContrast todoProject.color)
            , bor 2
            , hover [ underline, pointer ]
            ]
        , TodoProject.href todoProject
        ]
        [ text todoProject.title ]


viewLabels lc todo =
    List.filterMap (\lid -> LabelCollection.byId lid lc |> Maybe.map viewLabel) (Todo.labelIdList todo)


viewLabel label =
    div
        [ css
            [ ph 1
            , Css.fontSize Css.small
            , c_ (Label.color label |> Color.blacken 15 |> toCssColor)
            , hover [ underline, pointer ]
            ]
        , Route.labelHref label
        ]
        [ text <| Label.title label ]
