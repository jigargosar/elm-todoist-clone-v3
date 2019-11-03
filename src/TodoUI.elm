module TodoUI exposing (view, viewCheck, viewLabel, viewProject)

import Color
import Css
import Emoji
import Html.Styled exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes exposing (class, css)
import Label exposing (Label)
import Route
import Styles exposing (..)
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import TodoProject exposing (TodoProject)


view :
    { a | toggle : TodoId -> msg }
    -> TodoProject
    -> List Label
    -> Todo
    -> Html msg
view config todoProject labelList todo =
    div [ class "ph2 pv1 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]
        [ div [ css [ flex, itemsCenter ] ]
            [ viewCheck config.toggle todo
            , div [ class "pa2 flex-grow-1" ] [ text <| Todo.title todo ]
            , viewProject todoProject
            ]
        , div [ css [ flex ] ] (List.map viewLabel labelList)
        ]


viewCheck : (TodoId -> msg) -> Todo -> Html msg
viewCheck toggle todo =
    let
        emoji =
            if Todo.isCompleted todo then
                Emoji.heavy_check_mark

            else
                Emoji.heavy_large_circle

        toggleMsg =
            toggle <| Todo.id todo
    in
    Emoji.button toggleMsg emoji


viewLabel : Label -> Html msg
viewLabel label =
    a
        [ css
            [ linkReset
            , ph 1
            , Css.fontSize Css.small
            , c_ (Label.color label |> Color.blacken 15 |> toCssColor)
            , hover [ underline, pointer ]
            ]
        , Route.labelHref label
        ]
        [ text <| Label.title label ]


viewProject : TodoProject -> Html msg
viewProject todoProject =
    a
        [ css
            [ linkReset
            , ph 1
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
