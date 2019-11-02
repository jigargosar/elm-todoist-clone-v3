module TodoUI exposing (..)

import Color
import Css
import Emoji
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (class, css)
import Label exposing (Label)
import LabelCollection exposing (LabelCollection)
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
    let
        viewIsCompleted =
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

        viewProject : Html msg
        viewProject =
            div
                [ css
                    [ ph 1
                    , lh 1.5
                    , Css.fontSize Css.small
                    , bg (toCssColor todoProject.color)
                    , c_ (toCssColor <| Color.highContrast todoProject.color)
                    , bor 2
                    , hover [ underline, pointer ]
                    ]
                ]
                [ text todoProject.title ]

        viewLabel label =
            div
                [ css
                    [ ph 1
                    , Css.fontSize Css.small
                    , c_ (Label.color label |> Color.blacken 15 |> toCssColor)
                    , hover [ underline, pointer ]
                    ]
                ]
                [ text <| Label.title label ]
    in
    div [ class "ph2 pv1 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]
        [ div [ css [ flex, itemsCenter ] ]
            [ viewIsCompleted
            , div [ class "pa2 flex-grow-1" ] [ text <| Todo.title todo ]
            , viewProject
            ]
        , div [ css [ flex ] ] (List.map viewLabel labelList)
        ]
