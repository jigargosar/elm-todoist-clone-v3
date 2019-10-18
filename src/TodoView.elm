module TodoView exposing (Config, viewList)

import Color
import Css
import Emoji
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Label
import LabelCollection exposing (LabelCollection)
import ProjectCollection exposing (ProjectCollection)
import Styles exposing (..)
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import TodoProject


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
        ([ viewIsCompleted config todo
         , viewTitle todo
         , viewProject pc todo
         ]
            ++ viewLabels lc todo
        )


li : List (Html msg) -> Html msg
li =
    Html.li [ class "flex items-center lh-copy ph2 pv1 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]


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
        tp =
            TodoProject.fromTodo pc todo
    in
    div
        [ css
            [ ph 1
            , Css.fontSize Css.small
            , bg (toCssColor tp.color)
            , c_ (toCssColor <| Color.highContrast tp.color)
            , bor 2
            ]
        ]
        [ text tp.title ]


viewLabels lc todo =
    List.filterMap (\lid -> LabelCollection.byId lid lc |> Maybe.map viewLabelId) (Todo.labelIdList todo)


viewLabelId label =
    div
        [ css
            [ ph 1
            , Css.fontSize Css.small
            , c_ (fromHue (Label.hue label) |> Color.blacken 10 |> toCssColor)
            ]
        ]
        [ text <| Label.title label ]


fromHue h =
    Color.fromHSL ( toFloat h, 70, 30 )
