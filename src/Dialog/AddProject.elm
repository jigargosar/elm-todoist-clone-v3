module Dialog.AddProject exposing (Config, Model, Msg, SavedWith, init, update, view)

import Basics.More exposing (msgToCmd)
import Css
import Html.Styled as H exposing (Attribute, Html, button, div, form, input, label, span, text)
import Html.Styled.Attributes as A exposing (autofocus, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Key
import Px as PX
import Styles exposing (..)
import Theme


type alias Model =
    { title : String
    , color : String
    }


type alias SavedWith =
    { title : String, color : String, isFavorite : Bool }


initial : Model
initial =
    Model "" ""


init : Config msg -> ( Model, Cmd msg )
init _ =
    ( initial, Cmd.none )


type Msg
    = Save
    | Cancel
    | Title String
    | Color String


type alias Config msg =
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update { saved, canceled, toMsg } message model =
    case message of
        Save ->
            ( model
            , SavedWith model.title model.color False
                |> saved
                |> msgToCmd
            )

        Cancel ->
            ( model, msgToCmd canceled )

        Title title ->
            ( { model | title = title }, Cmd.none )

        Color color ->
            ( { model | color = color }, Cmd.none )


view : Config msg -> Model -> List (Html msg)
view { toMsg } model =
    let
        formAttrs =
            [ css
                [ bgWhite
                , Styles.bor 3
                , w_ 300
                , max_w_pct 100
                ]
            , A.class "shadow-1"
            , Key.onKeyDown [ Key.escape Cancel ]
            , onSubmit Save
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
                [ formTextIpt "Project name" (ipt model.title Title True)
                , formTextIpt "Project color" (ipt model.color Color False)
                , label [ css [ flex, itemsCenter, pv 2 ] ]
                    [ div [ css [ pa 1 ] ] [ input [ css [], type_ "checkbox" ] [] ]
                    , text "Add to favorites"
                    ]
                ]
            , div [ css [ flex, flexRowReverse, PX.p2 12 12, bo_t, boc <| Theme.borderGray ] ]
                [ btnSubmit "Add"
                , btnCancel
                ]
            ]
    in
    [ div [ css [ overlayStyles ] ]
        [ form formAttrs innerView ]
        |> H.map toMsg
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


btnCancel =
    button [ css [ plainBtnStyles ], onClick Cancel ] [ text "Cancel" ]


plainBtnStyles =
    batch
        [ btnReset
        , PX.p2 4 8
        , bor 1
        , hover [ bgGrayL 0.95 ]
        , focus [ bgGrayL 0.9, z_ 1 ]
        ]


ipt val oi af =
    input
        [ css [ lh 1.5, pa 1, bo_a, boc <| Theme.borderGray ]
        , value val
        , onInput oi
        , autofocus af
        ]
        []


lbl title =
    span [ css [ lh 1.5, bold ] ] [ text title ]


formTextIpt title i =
    label [ css [ flex, flexColumn, pv 2 ] ]
        [ lbl title
        , i
        ]
