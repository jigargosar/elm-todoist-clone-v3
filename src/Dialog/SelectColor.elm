module Dialog.SelectColor exposing (Config, Model, Msg, initial, update, view)

import Basics.More exposing (apply, viewIf, viewMaybe)
import Css exposing (hex)
import Focus
import Html.Styled as H exposing (Html, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import Html.Styled.Events exposing (onBlur, onClick)
import Key
import Px
import Styles exposing (..)
import Theme


type CColor
    = Blue
    | Green
    | Yellow


allColors =
    [ Blue, Green, Yellow ]


initial : Model
initial =
    Model Blue DropdownClosed


type Dropdown
    = DropdownClosed
    | DropdownOpened DropdownState


type alias DropdownState =
    {}


type alias Model =
    { color : CColor, dropdown : Dropdown }


type Msg
    = Close
    | Open
    | Focused Focus.FocusResult


type alias Config msg =
    { toMsg : Msg -> msg, domIdPrefix : String }


getDropdownState : Model -> Maybe DropdownState
getDropdownState model =
    case model.dropdown of
        DropdownClosed ->
            Nothing

        DropdownOpened state ->
            Just state


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update ({ toMsg } as config) message model =
    case message of
        Close ->
            ( { model | dropdown = DropdownClosed }
            , focus config selectInputDomId
            )

        Open ->
            ( { model | dropdown = DropdownOpened {} }
            , focus config selectPopupDomId
            )

        Focused result ->
            ( model, Focus.logIfError result )


focus : Config msg -> (Config msg -> String) -> Cmd msg
focus config domIdFromConfig =
    Focus.attempt (domIdFromConfig config) (config.toMsg << Focused)


selectPopupDomId : Config msg -> String
selectPopupDomId { domIdPrefix } =
    domIdPrefix ++ "__select-color-popup"


selectInputDomId : Config msg -> String
selectInputDomId { domIdPrefix } =
    domIdPrefix ++ "__select-color-input"


view : Config msg -> Model -> Html msg
view ({ toMsg } as config) model =
    div
        [ css [ relative, lh 1.5 ] ]
        [ viewSelectInput config model
        , viewMaybe (\_ -> viewPopup config) (getDropdownState model)
        ]
        |> H.map toMsg


viewPopup : Config msg -> Html Msg
viewPopup config =
    div
        [ A.id <| selectPopupDomId config
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


viewSelectInput : Config msg -> Model -> Html Msg
viewSelectInput config model =
    let
        keydownDecoders =
            [ Key.enter
            , Key.space
            , Key.arrowDown
            ]
                |> List.map (apply ( Open, True ))

        attrsWhenPopupClosed =
            case model.dropdown of
                DropdownClosed ->
                    [ tabindex 0
                    , Key.preventDefaultOnKeyDown keydownDecoders
                    , onClick Open
                    ]

                DropdownOpened _ ->
                    []
    in
    div
        (A.id (selectInputDomId config)
            :: css [ boAll, boColor Theme.borderGray ]
            :: attrsWhenPopupClosed
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
