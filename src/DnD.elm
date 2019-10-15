module DnD exposing (DnD, Msg, System, create)

import Basics.More exposing (flip)
import Browser.Dom as Dom
import Browser.Events
import Css
import Html.Styled as H
import Html.Styled.Attributes as A exposing (css)
import Html.Styled.Events as E
import Json.Decode as JD
import Lens
import Styles
import Task


type alias System msg big =
    { initial : DnD
    , update : Msg -> big -> ( big, Cmd msg )
    , subscriptions : big -> Sub msg
    , dragEvents : String -> List (H.Attribute msg)
    , dropEvents : String -> List (H.Attribute msg)
    , ghostStyles : big -> Css.Style
    , info : big -> Maybe Info
    }


create :
    (Msg -> msg)
    -> Lens.Lens DnD big
    -> System msg big
create toMsg bigL =
    let
        mapAttrs =
            List.map (A.map toMsg)
    in
    { initial = initial
    , update = \msg -> Lens.update bigL (update toMsg msg)
    , subscriptions = bigL.get >> subscriptions >> Sub.map toMsg
    , dragEvents = dragEvents >> mapAttrs
    , dropEvents = dropEvents >> mapAttrs
    , ghostStyles = bigL.get >> ghostStyles
    , info = bigL.get >> info
    }


dragEvents : String -> List (H.Attribute Msg)
dragEvents domId =
    [ E.preventDefaultOn "mousedown"
        (JD.map2 (DragStart domId)
            positionDecoder
            elementOffsetDecoder
            |> preventDefault
        )
    ]


preventDefault =
    JD.map (flip Tuple.pair True)


dropEvents : String -> List (H.Attribute Msg)
dropEvents domId =
    [ E.onMouseOver (DragOver domId)
    ]


positionSubtract : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Position
positionSubtract a b =
    Position (a.x - b.x) (a.y - b.y)


positionAdd a b =
    Position (a.x + b.x) (a.y + b.y)


ghostStyles : DnD -> Css.Style
ghostStyles =
    info
        >> Maybe.map
            (\{ dragElement, currentPosition, startPosition } ->
                let
                    { x, y } =
                        positionAdd (positionSubtract currentPosition startPosition)
                            (positionSubtract dragElement.element dragElement.viewport)
                in
                [ Styles.absolute
                , Styles.top_0
                , Styles.left_0
                , Css.transform (Css.translate2 (Css.px x) (Css.px y))
                ]
            )
        >> Maybe.withDefault []
        >> Css.batch


info : DnD -> Maybe Info
info =
    unwrap
        >> Maybe.andThen
            (\state ->
                Maybe.map2
                    (\dragElement dropElement ->
                        Info
                            state.startPosition
                            state.currentPosition
                            dragElement
                            dropElement
                            state.dragElementId
                            state.dropElementId
                    )
                    state.dragElement
                    state.dropElement
            )


type DnD
    = DnD Internal


type alias Internal =
    Maybe State


type alias Position =
    { x : Float, y : Float }


type alias ElementOffset =
    { offsetLeft : Float, offsetTop : Float }


type alias State =
    { startPosition : Position
    , currentPosition : Position
    , dragElement : Maybe Dom.Element
    , dropElement : Maybe Dom.Element
    , dragElementId : String
    , dropElementId : String
    }


type alias Info =
    { startPosition : Position
    , currentPosition : Position
    , dragElement : Dom.Element
    , dropElement : Dom.Element
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


type alias ViewPortResult =
    Result Dom.Error Dom.Viewport


type Msg
    = DragStart String Position ElementOffset
    | Drag Position
    | DragOver String
    | DragEnd
    | GotDragElement ElementOffset (Result Dom.Error ( Dom.Element, Dom.Viewport ))
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


elementOffsetDecoder : JD.Decoder ElementOffset
elementOffsetDecoder =
    JD.map2 ElementOffset
        (JD.at [ "target", "offsetLeft" ] JD.float)
        (JD.at [ "target", "offsetTop" ] JD.float)


subscriptions : DnD -> Sub Msg
subscriptions (DnD internal) =
    internal
        |> Maybe.map
            (\_ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (positionDecoder |> JD.map Drag)
                    , Browser.Events.onMouseUp (JD.succeed DragEnd)
                    ]
            )
        |> Maybe.withDefault Sub.none


update : (Msg -> msg) -> Msg -> DnD -> ( DnD, Cmd msg )
update toMsg message model =
    case message of
        DragStart dragElementId xy offset ->
            ( { startPosition = xy
              , currentPosition = xy
              , dragElementId = dragElementId
              , dropElementId = dragElementId
              , dragElement = Nothing
              , dropElement = Nothing
              }
                |> Just
                |> DnD
            , -- Dom.getElement dragElementId |>  Task.attempt (toMsg << GotDragElement)
              Task.map2 Tuple.pair
                (Dom.getElement dragElementId)
                (Dom.getViewportOf dragElementId)
                |> Task.attempt (toMsg << GotDragElement offset)
            )

        Drag xy ->
            ( mapState (\s -> { s | currentPosition = xy }) model, Cmd.none )

        DragOver dropElementId ->
            ( mapState (\s -> { s | dropElementId = dropElementId }) model
            , Dom.getElement dropElementId |> Task.attempt (toMsg << GotDropElement)
            )

        GotDragElement offset (Ok ( dragElement, _ )) ->
            let
                _ =
                    Debug.log "(dragElement, offset)" ( dragElement, offset )
            in
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

        GotDragElement _ _ ->
            ( model, Cmd.none )

        GotDropElement (Err _) ->
            ( model, Cmd.none )

        GotDropElement (Ok dropElement) ->
            ( mapState (\s -> { s | dropElement = Just dropElement }) model, Cmd.none )

        DragEnd ->
            ( DnD Nothing, Cmd.none )
