module DnD exposing (DnD)

import Browser.Dom as Dom
import Lens


type DnD
    = DnD Internal


type alias Internal =
    Maybe State


type alias Position =
    { x : Float, y : Float }


type alias State =
    { startPosition : Position
    , currentPosition : Position
    , dragElement : Maybe Dom.Element
    , dropElement : Maybe Dom.Element
    , dragElementId : String
    , dropElementId : String
    }


unwrap : DnD -> Internal
unwrap (DnD internal) =
    internal


map : (Internal -> Internal) -> DnD -> DnD
map func =
    unwrap >> func >> DnD


mapState : (State -> State) -> DnD -> DnD
mapState func =
    map (Maybe.map func)


initial : DnD
initial =
    DnD Nothing


type alias ElementResult =
    Result Dom.Error Dom.Element


type Msg
    = DragStart String Position
    | Drag Position
    | DragOver String
    | GotDragElement ElementResult
    | GotDropElement ElementResult


update message model =
    case message of
        DragStart dragElementId xy ->
            { startPosition = xy
            , currentPosition = xy
            , dragElementId = dragElementId
            , dropElementId = dragElementId
            , dragElement = Nothing
            , dropElement = Nothing
            }
                |> Just
                |> DnD

        Drag xy ->
            mapState (\s -> { s | currentPosition = xy }) model

        DragOver dropElementId ->
            mapState (\s -> { s | dropElementId = dropElementId }) model

        GotDragElement (Err _) ->
            model

        GotDragElement (Ok dragElement) ->
            mapState (\s -> { s | dragElement = Just dragElement, dropElement = Just dragElement }) model

        GotDropElement (Err _) ->
            model

        GotDropElement (Ok dropElement) ->
            mapState (\s -> { s | dropElement = Just dropElement }) model
