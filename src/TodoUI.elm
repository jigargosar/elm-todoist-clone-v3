module TodoUI exposing (view, viewCheck, viewLabel, viewProject)

import Color
import Css
import Html.Styled exposing (Attribute, Html, a, div, text)
import Html.Styled.Attributes exposing (class, css)
import Label exposing (Label)
import Px
import Route
import Styles exposing (..)
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import TodoProject exposing (TodoProject)
import UI.Icon as Icon exposing (Icon)
import UI.IconButton as IconBtn


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
            , div [ css [ Px.pa 4, flexGrow1 ] ] [ text <| Todo.title todo ]
            , viewProject todoProject
            ]
        , div [ css [ flex ] ] (List.map viewLabel labelList)
        ]


viewCheck : (TodoId -> msg) -> Todo -> Html msg
viewCheck toggle todo =
    let
        icon =
            if Todo.isCompleted todo then
                Icon.CheckCircleOutline

            else
                Icon.CircleOutline

        toggleMsg =
            toggle <| Todo.id todo
    in
    IconBtn.view icon toggleMsg


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
            , boRad 2
            , hover [ underline, pointer ]
            ]
        , TodoProject.href todoProject
        ]
        [ text todoProject.title ]
