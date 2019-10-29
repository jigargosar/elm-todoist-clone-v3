module Dialog.AddProject exposing (Model, initial, view)

import Css
import Html.Styled exposing (Attribute, Html, button, div, form, input, label, span, text)
import Html.Styled.Attributes as A exposing (css, type_, value)
import Html.Styled.Events exposing (onSubmit)
import Key
import Px as PX
import Styles exposing (..)
import Theme


type alias Model =
    { title : String
    , color : String
    }


initial : Model
initial =
    Model "" ""


view : { a | cancel : msg } -> Model -> List (Html msg)
view config model =
    let
        formAttrs =
            [ css
                [ bgWhite
                , Styles.bor 3
                , w_ 300
                , max_w_pct 100
                ]
            , A.class "shadow-1"
            , Key.onKeyDown [ Key.escape config.cancel ]
            , onSubmit config.cancel
            ]

        innerView =
            [ div
                [ css
                    [ Css.fontSize Css.larger
                    , pa 3
                    , bo_b
                    , boc <| Theme.borderGray
                    ]
                ]
                [ text "Add Project" ]
            , div [ css [ ph 3 ] ]
                [ formTextIpt "Project name" model.title
                , formTextIpt "Project color" model.color
                , label [ css [ flex, itemsCenter, pv 2 ] ]
                    [ div [ css [ pa 1 ] ] [ input [ css [], type_ "checkbox" ] [] ]
                    , text "Add to favorites"
                    ]
                ]
            , div [ css [ flex, flexRowReverse, PX.p2 12 12, bo_t, boc <| Theme.borderGray ] ]
                [ btnSubmit "Add"
                , btnSubmit "Cancel"
                ]
            ]
    in
    [ div [ css [ overlayStyles ] ]
        [ form formAttrs innerView ]
    ]


overlayStyles =
    batch
        [ fixed
        , absFill
        , flex
        , itemsCenter
        , justifyCenter
        , bg (Css.hsla 0 0 0 0.2)

        --                 , bg (Css.hsla 0 1 1 0.6)
        , z_ 10
        ]


btnSubmit title =
    button [ css [ plainBtnStyles ] ] [ text title ]


plainBtnStyles =
    batch
        [ btnReset
        , PX.p2 4 8
        , bor 1
        , hover [ bgGrayL 0.95 ]
        , focus [ bgGrayL 0.9, z_ 1 ]
        ]


ipt val =
    input
        [ css [ lh 1.5, pa 1, bo_a, boc <| Theme.borderGray ]
        , value val
        ]
        []


lbl title =
    span [ css [ lh 1.5, bold ] ] [ text title ]


formTextIpt title val =
    label [ css [ flex, flexColumn, pv 2 ] ]
        [ lbl title
        , ipt val
        ]
