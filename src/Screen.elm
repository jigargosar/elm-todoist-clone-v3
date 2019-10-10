module Screen exposing (Msg, Screen, System, system)

import Browser.Dom exposing (Element, Viewport, getViewport)
import Browser.Events exposing (onResize)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    { model : Screen
    , init : ( Screen, Cmd Msg )
    , update : Msg -> big -> ( big, Cmd msg )
    , view : List (Html msg) -> List (Html msg) -> List (Html msg) -> big -> Html msg
    , subscriptions : big -> Sub msg
    }


system : Lens.System Screen big -> (Msg -> msg) -> (Int -> Int -> msg) -> System msg big
system bigL toMsg onSize =
    { model = initial
    , init = init
    , view = \a b c big -> view a b c (bigL.get big)
    , update =
        \msg big ->
            update (\w h -> onSize w h |> succeed |> perform identity)
                msg
                (bigL.get big)
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


view : List (Html msg) -> List (Html msg) -> List (Html msg) -> Screen -> Html msg
view top side main _ =
    div [ class "bg-body" ]
        [ header [ class "fixed top-0 bg-light-red white w-100 h-header" ]
            [ div
                ([ class "center w-100 max-w-app ph2" ]
                    ++ [ class "h-100", class "flex items-center" ]
                )
                top
            ]
        , div [ class "center w-100 max-w-app ", class "flex-grow-1" ]
            [ aside
                [ class "dn db-ns fixed top-sidebar bottom-0 w-sidebar hover-overflow-y  br-ns b--main"
                ]
                side
            , div
                [ class "ml0 ml-main-ns pt-main min-vh-100 flex-grow-1 flex"
                ]
                [ main_ [ class "flex-grow-1 bg-white br-ns b--main" ] main ]
            ]
        ]
