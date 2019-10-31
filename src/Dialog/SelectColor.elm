module Dialog.SelectColor exposing
    ( Config
    , Model
    , Msg
    , initial
    , subscriptions
    , update
    , view
    )

import Basics.More exposing (apply, msgToCmd, viewMaybe)
import CColor exposing (CColor)
import Focus
import Html.Styled as H exposing (Html, div, i, text)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import Html.Styled.Events exposing (onClick, onMouseOver)
import Key
import Px
import Return
import Styles exposing (..)
import Theme


rollListIndexBy : Int -> List a -> Int -> Int
rollListIndexBy offset list index =
    index + offset |> modBy (List.length list)


cColorsList : List CColor
cColorsList =
    CColor.list


initial : Model
initial =
    Closed


type Model
    = Closed
    | Opened DropdownState


type alias DropdownState =
    { index : Int }


type Msg
    = CloseAndRestoreFocus
    | Open
    | InputFocused Focus.FocusResult
    | DropdownFocused Focus.FocusResult
    | Selected CColor
    | SelectHighlighted
    | HighlightPrevious
    | HighlightNext
    | Highlighted Int
    | OnFocusOrClickOutside String


type alias Config msg =
    { toMsg : Msg -> msg
    , domIdPrefix : String
    , changed : CColor -> msg
    }


getDropdownState : Model -> Maybe DropdownState
getDropdownState model =
    case model of
        Closed ->
            Nothing

        Opened state ->
            Just state


getHighlightedColor : Model -> Maybe CColor
getHighlightedColor =
    getDropdownState
        >> Maybe.andThen
            (.index
                >> (\index ->
                        List.drop index cColorsList
                            |> List.head
                   )
            )


mapDropdownState : (DropdownState -> DropdownState) -> Model -> Model
mapDropdownState func model =
    case model of
        Closed ->
            model

        Opened state ->
            Opened (func state)


mapHighlightIndex : (Int -> Int) -> Model -> Model
mapHighlightIndex func =
    mapDropdownState (\state -> { state | index = func state.index })


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
    (case message of
        Open ->
            ( Opened { index = 0 }
            , focusDropdown config
            )

        InputFocused result ->
            ( model
            , Focus.logIfError result
            )

        DropdownFocused result ->
            ( model
            , case result of
                Ok () ->
                    Focus.registerOnFocusOrClickOutSide (dropdownDomId config)

                Err focusError ->
                    Focus.logError focusError
            )

        CloseAndRestoreFocus ->
            ( Closed
            , focusInput config
            )

        Selected color ->
            ( Closed
            , Cmd.batch
                [ focusInput config
                , config.changed color |> msgToCmd
                ]
            )

        SelectHighlighted ->
            case getHighlightedColor model of
                Just color ->
                    ( Closed
                    , Cmd.batch
                        [ focusInput config
                        , config.changed color |> msgToCmd
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        OnFocusOrClickOutside domId ->
            ( case domId == dropdownDomId config of
                True ->
                    Closed

                False ->
                    model
            , Cmd.none
            )

        HighlightNext ->
            ( mapHighlightIndex (rollListIndexBy 1 cColorsList) model
            , Cmd.none
            )

        HighlightPrevious ->
            ( mapHighlightIndex (rollListIndexBy -1 cColorsList) model
            , Cmd.none
            )

        Highlighted index ->
            ( mapHighlightIndex (\_ -> index) model
            , Cmd.none
            )
    )
        |> Return.effect_ (unregisterFocusMonitorOnDropdownCloseEffect config model)


unregisterFocusMonitorOnDropdownCloseEffect : Config msg -> Model -> Model -> Cmd msg
unregisterFocusMonitorOnDropdownCloseEffect config oldModel newModel =
    if oldModel /= newModel && newModel == Closed then
        Focus.unRegisterOnFocusOrClickOutSide (dropdownDomId config)

    else
        Cmd.none


focusInput : Config msg -> Cmd msg
focusInput config =
    Focus.attempt (inputDomId config) (config.toMsg << InputFocused)


focusDropdown : Config msg -> Cmd msg
focusDropdown config =
    Focus.attempt (dropdownDomId config) (config.toMsg << DropdownFocused)


dropdownDomId : Config msg -> String
dropdownDomId { domIdPrefix } =
    domIdPrefix ++ "__select-color-dropdown"


inputDomId : Config msg -> String
inputDomId { domIdPrefix } =
    domIdPrefix ++ "__select-color-input"


view : Config msg -> CColor -> Model -> Html msg
view ({ toMsg } as config) cColor model =
    div
        [ css [ relative, lh 1.5 ] ]
        [ viewInput config cColor model
        , viewMaybe (viewDropdown config) (getDropdownState model)
        ]
        |> H.map toMsg


viewInput : Config msg -> CColor -> Model -> Html Msg
viewInput config cColor model =
    let
        keydownDecoders =
            [ Key.enter
            , Key.space
            , Key.arrowDown
            ]
                |> List.map (apply ( Open, True ))

        attrsWhenDropdownClosed =
            case model of
                Closed ->
                    [ Key.preventDefaultOnKeyDown keydownDecoders
                    , onClick Open
                    , tabindex 0
                    ]

                Opened _ ->
                    []

        ( cssColor, colorLabel ) =
            CColor.infoOld cColor
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


viewDropdown : Config msg -> DropdownState -> Html Msg
viewDropdown config state =
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
        , Key.stopPropagationOnKeyDown
            [ Key.escape ( CloseAndRestoreFocus, True )
            , Key.enter ( SelectHighlighted, True )
            , Key.arrowUp ( HighlightPrevious, True )
            , Key.arrowDown ( HighlightNext, True )
            ]
        , tabindex 0
        ]
        (List.indexedMap (viewItem state) cColorsList)


viewItem : DropdownState -> Int -> CColor -> Html Msg
viewItem state index color =
    let
        ( cssColor, colorLabel ) =
            CColor.infoOld color

        highlightedStyles =
            case state.index == index of
                True ->
                    [ bgGrayL 0.8 ]

                False ->
                    []
    in
    div
        [ css [ flex, Px.pa 4, batch highlightedStyles ]
        , onClick <| Selected color
        , onMouseOver (Highlighted index)
        ]
        [ i [ css [ Px.p2 0 4, c_ cssColor ], class "material-icons" ]
            [ text "folder" ]
        , div [ css [ Px.p2 0 4 ] ] [ text colorLabel ]
        ]
