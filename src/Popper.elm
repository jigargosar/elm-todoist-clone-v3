port module Popper exposing (Msg, Popper, close, initial, open, styles, subscriptions, update)

import Browser.Dom as Dom exposing (Element)
import Browser.Events
import Css exposing (Style)
import Styles
import Task
import XY exposing (XY)


port logError : String -> Cmd msg


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
    | GotDomError Dom.Error


close : Msg
close =
    Close


open : XY -> String -> String -> Msg
open =
    Open


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
    let
        getElement domId onSuccess =
            Dom.getElement domId
                |> Task.attempt
                    (\res ->
                        case res of
                            Err error ->
                                GotDomError error

                            Ok element ->
                                onSuccess element
                    )
                |> Cmd.map toMsg
    in
    case message of
        Open xy anchorId popupId ->
            ( model
            , getElement anchorId (OpenWithAnchorEl xy anchorId popupId)
            )

        OpenWithAnchorEl xy anchorId popupId anchorEl ->
            ( State xy anchorId popupId anchorEl Nothing
                |> Just
                |> Popper
            , Cmd.none
            )

        GotPopupEl popupEl ->
            case internal of
                Just state ->
                    ( { state | popupEl = Just popupEl } |> Just |> Popper, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GotAnchorEl anchorEl ->
            case internal of
                Just state ->
                    ( { state | anchorEl = anchorEl } |> Just |> Popper, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Close ->
            ( Popper Nothing, Cmd.none )

        BrowserResized _ _ ->
            case internal of
                Just state ->
                    ( model
                    , Cmd.batch
                        [ getElement state.anchorId GotAnchorEl
                        , getElement state.popupId GotPopupEl
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        GotDomError (Dom.NotFound id) ->
            ( model, logError <| "Dom.NotFound " ++ id )


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
