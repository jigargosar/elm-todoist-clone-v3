port module Main exposing (main)

import Appbar
import Browser
import Html exposing (..)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Lens
import Return
import Screen exposing (Screen)
import Sidebar
import Todo exposing (Todo)
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)


port logError : String -> Cmd msg



-- Flags


type alias Flags =
    { todoList : Value }



-- MODEL


type alias Model =
    { todoDict : TodoDict
    , screen : Screen
    }


screenSystem : Screen.System Msg Model
screenSystem =
    let
        screenL : Lens.System Screen Model
        screenL =
            Lens.system { get = .screen, set = \s b -> { b | screen = s } }
    in
    Screen.system screenL (\_ -> NoOp) (\_ _ -> NoOp)


todoDictSystem =
    let
        todoDictL : Lens.System TodoDict Model
        todoDictL =
            Lens.system { get = .todoDict, set = \s b -> { b | todoDict = s } }
    in
    { sortedByIdx = todoDictL.get >> TodoDict.sortedByIdx
    , init =
        \encoded big ->
            let
                res =
                    case TodoDict.fromEncodedList encoded of
                        Ok todoDict_ ->
                            ( todoDict_, Cmd.none )

                        Err e ->
                            ( TodoDict.initial, logError <| JD.errorToString e )
            in
            res |> Tuple.mapFirst (todoDictL.setIn big)
    , toggle =
        \todoId big ->
            ( todoDictL.map (TodoDict.toggleCompleted todoId) big, Cmd.none )
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initial : Model
        initial =
            { todoDict = TodoDict.initial
            , screen = screenSystem.initial
            }
    in
    todoDictSystem.init flags.todoList initial
        |> Return.andThen screenSystem.init



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ screenSystem.subscriptions model
        ]



-- UPDATE


type Msg
    = NoOp
    | Toggle TodoId
    | Screen Screen.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Toggle todoId ->
            todoDictSystem.toggle todoId model

        Screen msg ->
            screenSystem.update msg model



-- VIEW


view : Model -> Html Msg
view model =
    screenSystem.view Appbar.view Sidebar.view (mainView model) model


mainView model =
    [ Todo.viewList { toggle = Toggle } (todoDictSystem.sortedByIdx model)
    ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
