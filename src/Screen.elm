module Screen exposing (Msg, Screen, system)

import Browser.Dom exposing (Element, Viewport, getViewport)
import Browser.Events exposing (onResize)
import Task exposing (attempt, perform, succeed)


type alias Screen =
    { width : Int, height : Int }


initial : Screen
initial =
    Screen 600 600


init : ( Screen, Cmd Msg )
init =
    ( initial, getViewport |> perform GotViewPort )


type alias System msg =
    { model : Screen
    , init : ( Screen, Cmd Msg )
    , update : Msg -> Screen -> ( Screen, Cmd msg )
    , subscriptions : Screen -> Sub msg
    }


system : (Msg -> msg) -> (Int -> Int -> msg) -> System msg
system toMsg onSize =
    { model = initial
    , init = init
    , update = update (\w h -> onSize w h |> succeed |> perform identity)
    , subscriptions = subscriptions >> Sub.map toMsg
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


subscriptions _ =
    Sub.batch [ onResize Changed ]
