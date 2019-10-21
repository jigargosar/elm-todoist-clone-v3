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
            []

        Nothing ->
            []
