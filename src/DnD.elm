module DnD exposing (DnD, Msg, System, create)

import Basics.More exposing (flip)
import Browser.Dom as Dom
import Browser.Events
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD
import Lens
import Task


type alias System msg big =
    { initial : DnD
    , update : Msg -> big -> ( big, Cmd msg )
    , subscriptions : big -> Sub msg
    , dragEvents : String -> List (H.Attribute msg)
    , dropEvents : String -> List (H.Attribute msg)
    , ghostStyles : big -> List (H.Attribute msg)
    }


create :
    (Msg -> msg)
    -> Lens.Lens DnD big
    -> System msg big
create toMsg bigL =
    let
        mapEvents =
            List.map (A.map toMsg)
    in
    { initial = initial
    , update = \msg -> Lens.update bigL (update toMsg msg)
    , subscriptions = bigL.get >> subscriptions >> Sub.map toMsg
    , dragEvents = dragEvents >> mapEvents
    , dropEvents = dropEvents >> mapEvents
    , ghostStyles = bigL.get >> ghostStyles
    }


dragEvents : String -> List (H.Attribute Msg)
dragEvents domId =
    [ E.preventDefaultOn "mousedown" (positionDecoder |> JD.map (DragStart domId) |> preventDefault)
    ]


preventDefault =
    JD.map (flip Tuple.pair True)


dropEvents : String -> List (H.Attribute Msg)
dropEvents domId =
    [ E.onMouseOver (DragOver domId)
    ]


ghostStyles : DnD -> List (H.Attribute msg)
ghostStyles (DnD internal) =
    internal
        |> Maybe.map (\_ -> [])
        |> Maybe.withDefault []


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


positionDecoder : JD.Decoder Position
positionDecoder =
    JD.map2 Position pageXDecoder pageYDecoder


subscriptions : DnD -> Sub Msg
subscriptions (DnD internal) =
    internal
        |> Maybe.map
            (\_ ->
                Sub.batch
                    [ Browser.Events.onMouseMove
                        (positionDecoder |> JD.map Drag)
                    ]
            )
        |> Maybe.withDefault Sub.none


update : (Msg -> msg) -> Msg -> DnD -> ( DnD, Cmd msg )
update toMsg message model =
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
            , Dom.getElement dragElementId |> Task.attempt (toMsg << GotDragElement)
            )

        Drag xy ->
            ( mapState (\s -> { s | currentPosition = xy }) model, Cmd.none )

        DragOver dropElementId ->
            ( mapState (\s -> { s | dropElementId = dropElementId }) model
            , Dom.getElement dropElementId |> Task.attempt (toMsg << GotDragElement)
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
