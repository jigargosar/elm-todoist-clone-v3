module DnD exposing (DnD)

import Browser.Dom as Dom
import Browser.Events
import Json.Decode as JD
import Task


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
    | DragEnd
    | GotDragElement ElementResult
    | GotDropElement ElementResult


pageXDecoder : JD.Decoder Float
pageXDecoder =
    JD.field "pageX" JD.float


pageYDecoder : JD.Decoder Float
pageYDecoder =
    JD.field "pageY" JD.float


subscriptions : DnD -> Sub Msg
subscriptions (DnD internal) =
    internal
        |> Maybe.map
            (\_ ->
                Sub.batch
                    [ Browser.Events.onMouseMove
                        (JD.map2 Position pageXDecoder pageYDecoder |> JD.map Drag)
                    ]
            )
        |> Maybe.withDefault Sub.none


update : Msg -> DnD -> ( DnD, Cmd Msg )
update message model =
    case message of
        DragStart dragElementId xy ->
            ( { startPosition = xy
              , currentPosition = xy
              , dragElementId = dragElementId
              , dropElementId = dragElementId
              , dragElement = Nothing
              , dropElement = Nothing
              }
                |> Just
                |> DnD
            , Dom.getElement dragElementId |> Task.attempt GotDragElement
            )

        Drag xy ->
            ( mapState (\s -> { s | currentPosition = xy }) model, Cmd.none )

        DragOver dropElementId ->
            ( mapState (\s -> { s | dropElementId = dropElementId }) model
            , Dom.getElement dropElementId |> Task.attempt GotDragElement
            )

        GotDragElement (Err _) ->
            ( model, Cmd.none )

        GotDragElement (Ok dragElement) ->
            ( mapState
                (\s ->
                    { s
                        | dragElement = Just dragElement
                        , dropElement = Just dragElement
                    }
                )
                model
            , Cmd.none
            )

        GotDropElement (Err _) ->
            ( model, Cmd.none )

        GotDropElement (Ok dropElement) ->
            ( mapState (\s -> { s | dropElement = Just dropElement }) model, Cmd.none )

        DragEnd ->
            ( DnD Nothing, Cmd.none )
