module Dialog.SelectColor exposing (Config, Model, Msg, initial, subscriptions, update, view)

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


allColors : List CColor
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
    = CloseAndRestoreFocus
    | Open
    | Focused Focus.FocusResult
    | Selected CColor
    | OnFocusOrClickOutside String


type alias Config msg =
    { toMsg : Msg -> msg, domIdPrefix : String }


getDropdownState : Model -> Maybe DropdownState
getDropdownState model =
    case model.dropdown of
        DropdownClosed ->
            Nothing

        DropdownOpened state ->
            Just state


subscriptions : Config msg -> Model -> Sub msg
subscriptions { toMsg } model =
    case getDropdownState model of
        Just _ ->
            Focus.onFocusOrClickOutside OnFocusOrClickOutside
                |> Sub.map toMsg

        Nothing ->
            Sub.none


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update ({ toMsg } as config) message model =
    case message of
        CloseAndRestoreFocus ->
            ( { model | dropdown = DropdownClosed }
            , Cmd.batch
                [ focus config inputDomId
                , unregisterDropdownFocusMonitor config
                ]
            )

        Open ->
            ( { model | dropdown = DropdownOpened {} }
            , focus config dropdownDomId
            )

        Focused result ->
            ( model
            , case result of
                Ok () ->
                    Focus.registerOnFocusOrClickOutSide (dropdownDomId config)

                Err focusError ->
                    Focus.logError focusError
            )

        Selected color ->
            ( { model | color = color, dropdown = DropdownClosed }
            , unregisterDropdownFocusMonitor config
            )

        OnFocusOrClickOutside domId ->
            case domId == dropdownDomId config of
                True ->
                    ( { model | dropdown = DropdownClosed }
                    , unregisterDropdownFocusMonitor config
                    )

                False ->
                    ( model, Cmd.none )


unregisterDropdownFocusMonitor config =
    Focus.unRegisterOnFocusOrClickOutSide (dropdownDomId config)


focus : Config msg -> (Config msg -> String) -> Cmd msg
focus config domIdFromConfig =
    Focus.attempt (domIdFromConfig config) (config.toMsg << Focused)


dropdownDomId : Config msg -> String
dropdownDomId { domIdPrefix } =
    domIdPrefix ++ "__select-color-dropdown"


inputDomId : Config msg -> String
inputDomId { domIdPrefix } =
    domIdPrefix ++ "__select-color-input"


view : Config msg -> Model -> Html msg
view ({ toMsg } as config) model =
    div
        [ css [ relative, lh 1.5 ] ]
        [ viewSelectInput config model
        , viewMaybe (viewDropdown config) (getDropdownState model)
        ]
        |> H.map toMsg


viewDropdown : Config msg -> DropdownState -> Html Msg
viewDropdown config state =
    let
        viewItem : CColor -> Html Msg
        viewItem color =
            div [ css [ flex, Px.pa 4 ], onClick <| Selected color ]
                [ i [ css [ Px.p2 0 4, c_ <| colorCssValue color ], class "material-icons" ]
                    [ text "folder" ]
                , div [ css [ Px.p2 0 4 ] ] [ text <| colorText color ]
                ]
    in
    div
        [ A.id <| dropdownDomId config
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
        , Key.stopPropagationOnKeyDown [ Key.escape ( CloseAndRestoreFocus, True ) ]
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

        attrsWhenDropdownClosed =
            case model.dropdown of
                DropdownClosed ->
                    [ Key.preventDefaultOnKeyDown keydownDecoders
                    , onClick Open
                    , tabindex 0
                    ]

                DropdownOpened _ ->
                    []

        ( cssColor, colorLabel ) =
            colorInfo model.color
    in
    div
        (A.id (inputDomId config)
            :: css [ boAll, boColor Theme.borderGray ]
            :: attrsWhenDropdownClosed
        )
        [ div [ css [ flex, Px.pa 4 ] ]
            [ i [ css [ Px.p2 0 4, c_ cssColor ], class "material-icons" ] [ text "folder" ]
            , div [ css [ Px.p2 0 4 ] ] [ text colorLabel ]
            ]
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
