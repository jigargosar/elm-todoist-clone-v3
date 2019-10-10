module Screen exposing (Msg, Screen, System, system)

import Browser.Dom exposing (Element, Viewport, getViewport)
import Browser.Events exposing (onResize)
import Lens
import Task exposing (perform, succeed)


type alias Screen =
    { width : Int, height : Int }


initial : Screen
initial =
    Screen 600 600


init : ( Screen, Cmd Msg )
init =
    ( initial, getViewport |> perform GotViewPort )


type alias System msg big =
    { initial : Screen
    , init : big -> ( big, Cmd msg )
    , update : Msg -> big -> ( big, Cmd msg )
    , subscriptions : big -> Sub msg
    }


system :
    Lens.System Screen big
    -> (Msg -> msg)
    -> (Int -> Int -> msg)
    -> System msg big
system bigL toMsg onSize =
    { initial = initial
    , init = \big -> init |> Tuple.mapBoth (bigL.setIn big) (Cmd.map toMsg)
    , update =
        \msg big ->
            update (\w h -> onSize w h |> succeed |> perform identity)
                msg
                (bigL.get big)
                |> Tuple.mapFirst (bigL.setIn big)
    , subscriptions = bigL.get >> subscriptions >> Sub.map toMsg
    }


type Msg
    = Changed Int Int
    | GotViewPort Viewport


update : (Int -> Int -> Cmd msg) -> Msg -> Screen -> ( Screen, Cmd msg )
update onSize message model =
    case message of
        Changed w h ->
            let
                newModel =
                    { model | width = w, height = h }
            in
            ( newModel, onSize newModel.width newModel.height )

        GotViewPort { scene } ->
            let
                newModel =
                    { model | width = round scene.width, height = round scene.height }
            in
            ( newModel, onSize newModel.width newModel.height )


subscriptions : Screen -> Sub Msg
subscriptions _ =
    Sub.batch [ onResize Changed ]
