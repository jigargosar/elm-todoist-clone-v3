module Dialog.SelectColor exposing (Config, Model, Msg, initial, update, view)

import Browser.Dom as Dom
import Css exposing (hex)
import Html.Styled as H exposing (Html, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import Html.Styled.Events exposing (onBlur)
import Log exposing (logError)
import Px
import Styles exposing (..)
import Task
import Theme


type CColor
    = Blue
    | Green
    | Yellow


allColors =
    [ Blue, Green, Yellow ]


initial : Model
initial =
    Model Blue True


type alias Model =
    { color : CColor, open : Bool }


type Msg
    = Close
    | Open
    | PopupFocused (Result Dom.Error ())


type alias Config msg =
    { toMsg : Msg -> msg }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update { toMsg } message model =
    case message of
        Close ->
            ( { model | open = False }, Cmd.none )

        Open ->
            ( { model | open = True }
            , Dom.focus selectPopupDomId
                |> Task.attempt PopupFocused
                |> Cmd.map toMsg
            )

        PopupFocused result ->
            case result of
                Ok () ->
                    ( model, Cmd.none )

                Err (Dom.NotFound domId) ->
                    ( model, logError <| "focus failed: " ++ domId )


selectPopupDomId : String
selectPopupDomId =
    "select-color-popup"


view : Config msg -> Model -> Html msg
view { toMsg } model =
    div
        [ css
            [ relative
            , lh 1.5
            ]
        ]
        [ div
            [ css
                [ boAll
                , boColor Theme.borderGray
                ]
            , case model.open of
                True ->
                    class ""

                False ->
                    tabindex 0
            ]
            [ viewItem model.color ]
        , case model.open of
            True ->
                div
                    [ A.id selectPopupDomId
                    , css
                        [ absolute
                        , bgWhite
                        , w_100
                        , left_0
                        , top_0
                        , boAll
                        , boColor Theme.borderGray
                        , z_ 1
                        ]
                    , onBlur Close
                    , tabindex 0
                    ]
                    (List.map viewItem allColors)

            False ->
                text ""
        ]
        |> H.map toMsg


viewItem : CColor -> Html msg
viewItem color =
    div [ css [ flex, Px.pa 4 ] ]
        [ i [ css [ Px.p2 0 4, c_ <| colorCssValue color ], class "material-icons" ] [ text "folder" ]
        , div [ css [ Px.p2 0 4 ] ] [ text <| colorText color ]
        ]


colorText : CColor -> String
colorText =
    colorInfo >> Tuple.second


colorCssValue : CColor -> Css.Color
colorCssValue =
    colorInfo >> Tuple.first


colorInfo : CColor -> ( Css.Color, String )
colorInfo color =
    case color of
        Blue ->
            ( hex "#4073ff", "Blue" )

        Green ->
            ( hex "#299438", "Green" )

        Yellow ->
            ( hex "#fad000", "Yellow" )
