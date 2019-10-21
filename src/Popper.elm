module Popper exposing (Msg, Popper, initial, styles, subscriptions, update)

import Browser.Dom exposing (Element)
import Browser.Events
import Css exposing (Style)
import Styles
import XY exposing (XY)


type Popper
    = Popper (Maybe State)


type alias State =
    { startXY : XY
    , anchorId : String
    , popupId : String
    , anchorEl : Element
    , popupEl : Maybe Element
    }


initial : Popper
initial =
    Popper Nothing


type Msg
    = Open XY String String
    | OpenWithAnchorEl XY String String Element
    | GotPopupEl Element
    | GotAnchorEl Element
    | Close
    | BrowserResized Int Int


subscriptions : (Msg -> msg) -> Popper -> Sub msg
subscriptions toMsg (Popper internal) =
    Sub.map toMsg <|
        case internal of
            Just _ ->
                Browser.Events.onResize BrowserResized

            Nothing ->
                Sub.none


update : (Msg -> msg) -> Msg -> Popper -> ( Popper, Cmd msg )
update toMsg message ((Popper internal) as model) =
    case message of
        Open xy anchorId popupId ->
            ( model, Cmd.none )

        OpenWithAnchorEl xy anchorId popupId anchorEl ->
            ( model, Cmd.none )

        GotPopupEl popupEl ->
            ( model, Cmd.none )

        GotAnchorEl anchorEl ->
            ( model, Cmd.none )

        Close ->
            ( model, Cmd.none )

        BrowserResized _ _ ->
            ( model, Cmd.none )


styles : Popper -> List Style
styles ((Popper internal) as model) =
    case internal of
        Just state ->
            let
                atLeastZero =
                    max 0

                xy =
                    -- popupModel.startXY
                    state.anchorEl.element

                currentTop =
                    atLeastZero xy.y

                currentLeft =
                    atLeastZero xy.x
            in
            [ Styles.bgWhite
            , Styles.pa 3
            , Styles.bor 3
            , Styles.absolute
            , Css.top <| Css.px currentTop
            , Css.left <| Css.px currentLeft
            , Css.minWidth <| Css.px 150
            , case state.popupEl of
                Just e ->
                    let
                        maxTop =
                            atLeastZero (e.viewport.height - e.element.height)

                        maxLeft =
                            atLeastZero (e.viewport.width - e.element.width)

                        finalTop =
                            min maxTop currentTop

                        finalLeft =
                            min maxLeft currentLeft

                        topDiff =
                            finalTop - currentTop

                        leftDiff =
                            finalLeft - currentLeft
                    in
                    Styles.batch
                        [ Css.transform (Css.translate2 (Css.px leftDiff) (Css.px topDiff))
                        , Styles.commonTransitions
                        ]

                Nothing ->
                    Styles.batch []
            ]

        Nothing ->
            []
