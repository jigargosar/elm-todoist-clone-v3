module DnD exposing (DnD, Msg, System, create)

import Basics.More exposing (flip)
import Browser.Dom as Dom
import Browser.Events
import Css
import Html.Styled as H
import Html.Styled.Attributes as A
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
        (JD.map (DragStart domId)
            positionDecoder
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
                            state.dragElement.domId
                            state.dropElement.domId
                    )
                    state.dragElement.domElement
                    state.dropElement.domElement
            )


type DnD
    = DnD Internal


type alias Internal =
    Maybe State


type alias Position =
    { x : Float, y : Float }


type alias Element =
    { idx : Int, domId : String, domElement : Maybe Dom.Element }


type alias State =
    { startPosition : Position
    , currentPosition : Position
    , dragElement : Element
    , dropElement : Element
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


mapDropElement : (Element -> Element) -> DnD -> DnD
mapDropElement func =
    mapState (\s -> { s | dropElement = func s.dropElement })


mapDragElement : (Element -> Element) -> DnD -> DnD
mapDragElement func =
    mapState (\s -> { s | dragElement = func s.dragElement })


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
                    [ Browser.Events.onMouseMove (positionDecoder |> JD.map Drag)
                    , Browser.Events.onMouseUp (JD.succeed DragEnd)
                    ]
            )
        |> Maybe.withDefault Sub.none


update : (Msg -> msg) -> Msg -> DnD -> ( DnD, Cmd msg )
update toMsg message model =
    case message of
        DragStart dragElementId xy ->
            let
                element =
                    Element 0 dragElementId Nothing
            in
            ( { startPosition = xy
              , currentPosition = xy
              , dragElement = element
              , dropElement = element
              }
                |> Just
                |> DnD
            , Dom.getElement dragElementId |> Task.attempt (toMsg << GotDragElement)
            )

        Drag xy ->
            ( mapState (\s -> { s | currentPosition = xy }) model, Cmd.none )

        DragOver dropElementId ->
            ( mapDropElement (\s -> { s | domId = dropElementId }) model
            , Dom.getElement dropElementId |> Task.attempt (toMsg << GotDropElement)
            )

        GotDragElement (Ok dragElement) ->
            ( model
                |> mapDragElement (\s -> { s | domElement = Just dragElement })
                |> mapDropElement (\s -> { s | domElement = Just dragElement })
            , Cmd.none
            )

        GotDragElement _ ->
            ( model, Cmd.none )

        GotDropElement (Err _) ->
            ( model, Cmd.none )

        GotDropElement (Ok dropElement) ->
            ( model
                |> mapDropElement (\s -> { s | domElement = Just dropElement })
            , Cmd.none
            )

        DragEnd ->
            ( DnD Nothing, Cmd.none )
