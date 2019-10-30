module Dialog.SelectColor exposing (Config, Model, Msg, initial, update, view)

import Basics.More exposing (apply, attrIf, viewIf)
import Browser.Dom as Dom
import Css exposing (hex)
import Focus
import Html.Styled as H exposing (Html, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import Html.Styled.Events exposing (onBlur)
import Json.Decode as JD
import Key
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
    | Focused Focus.FocusResult


type alias Config msg =
    { toMsg : Msg -> msg }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update { toMsg } message model =
    case message of
        Close ->
            ( { model | open = False }
            , Focus.attempt selectInputDomId (toMsg << Focused)
            )

        Open ->
            ( { model | open = True }
            , Focus.attempt selectPopupDomId (toMsg << Focused)
            )

        Focused result ->
            ( model, Focus.logIfError result )


selectPopupDomId : String
selectPopupDomId =
    "select-color-popup"


selectInputDomId : String
selectInputDomId =
    "select-color-input"


view : Config msg -> Model -> Html msg
view { toMsg } model =
    div
        [ css [ relative, lh 1.5 ] ]
        [ viewSelectInput model
        , viewIf model.open viewPopup
        ]
        |> H.map toMsg


viewPopup _ =
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
        , Key.stopPropagationOnKeyDown [ Key.escape ( Close, True ) ]
        , tabindex 0
        ]
        (List.map viewItem allColors)


viewSelectInput model =
    let
        keydownDecoders =
            [ Key.enter
            , Key.space
            , Key.arrowDown
            ]
                |> List.map (apply ( Open, True ))
    in
    div
        (css [ boAll, boColor Theme.borderGray ]
            :: (if model.open then
                    []

                else
                    [ tabindex 0, Key.preventDefaultOnKeyDown keydownDecoders ]
               )
        )
        [ viewItem model.color ]


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
