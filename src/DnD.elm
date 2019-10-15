module DnD exposing (DnD, Info, Msg, System, create, rotate)

import Basics.More exposing (flip)
import Browser.Dom as Dom
import Browser.Events
import Css
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Json.Decode as JD
import Lens
import SelectList
import Styles
import Task


type alias System msg big =
    { initial : DnD
    , update : Msg -> big -> ( big, Cmd msg )
    , subscriptions : big -> Sub msg
    , dragEvents : Int -> String -> List (H.Attribute msg)
    , dropEvents : Int -> String -> List (H.Attribute msg)
    , ghostStyles : big -> Css.Style
    , info : big -> Maybe Info
    }


rotate : Info -> List a -> List a
rotate { drag, drop } list =
    SelectList.fromList list
        |> Maybe.andThen (SelectList.selectBy drag.index)
        |> Maybe.map (SelectList.moveBy (drop.index - drag.index) >> SelectList.toList)
        |> Maybe.withDefault list


type alias Callbacks msg =
    { onCommit : Info -> msg }


create :
    (Msg -> msg)
    -> Callbacks msg
    -> Lens.Lens DnD big
    -> System msg big
create toMsg callbacks bigL =
    let
        mapAttrs =
            List.map (A.map toMsg)
    in
    { initial = initial
    , update = \msg -> Lens.update bigL (update toMsg callbacks msg)
    , subscriptions = bigL.get >> subscriptions >> Sub.map toMsg
    , dragEvents = \a -> dragEvents a >> mapAttrs
    , dropEvents = \a -> dropEvents a >> mapAttrs
    , ghostStyles = bigL.get >> ghostStyles
    , info = bigL.get >> info
    }


dragEvents : Int -> String -> List (H.Attribute Msg)
dragEvents index domId =
    [ E.preventDefaultOn "mousedown"
        (JD.map (DragStart index domId)
            positionDecoder
            |> preventDefault
        )
    ]


dropEvents : Int -> String -> List (H.Attribute Msg)
dropEvents index domId =
    [ E.onMouseOver (DragOver index domId)
    ]


preventDefault =
    JD.map (flip Tuple.pair True)


positionSubtract : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Position
positionSubtract a b =
    Position (a.x - b.x) (a.y - b.y)


positionAdd a b =
    Position (a.x + b.x) (a.y + b.y)


ghostStyles : DnD -> Css.Style
ghostStyles =
    info
        >> Maybe.map
            (\{ drag, currentPosition, startPosition } ->
                let
                    { x, y } =
                        positionAdd (positionSubtract currentPosition startPosition)
                            (positionSubtract drag.domElement.element drag.domElement.viewport)
                in
                [ Styles.absolute
                , Styles.top_0
                , Styles.left_0
                , Css.transform (Css.translate2 (Css.px x) (Css.px y))
                , Css.pointerEvents Css.none
                ]
            )
        >> Maybe.withDefault []
        >> Css.batch


info : DnD -> Maybe Info
info =
    unwrap
        >> Maybe.map
            (\state ->
                Info
                    state.startPosition
                    state.currentPosition
                    state.dragElement
                    state.dropElement
            )


type DnD
    = DnD Internal


type alias Internal =
    Maybe State


type alias Position =
    { x : Float, y : Float }


type alias Element =
    { index : Int, domId : String, domElement : Dom.Element }


type alias State =
    { startPosition : Position
    , currentPosition : Position
    , dragElement : Element
    , dropElement : Element
    }


type alias Info =
    { startPosition : Position
    , currentPosition : Position
    , drag : Element
    , drop : Element
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


initial : DnD
initial =
    DnD Nothing


type alias ElementResult =
    Result Dom.Error Dom.Element


type Msg
    = DragStart Int String Position
    | Drag Position
    | DragOver Int String
    | DragEnd
    | GotDragElement Int String Position ElementResult
    | GotDropElement Int String ElementResult


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


update : (Msg -> msg) -> Callbacks msg -> Msg -> DnD -> ( DnD, Cmd msg )
update toMsg callbacks message model =
    case message of
        DragStart index dragElementId xy ->
            ( DnD Nothing
            , Dom.getElement dragElementId |> Task.attempt (toMsg << GotDragElement index dragElementId xy)
            )

        Drag xy ->
            ( mapState (\s -> { s | currentPosition = xy }) model, Cmd.none )

        DragOver index domId ->
            let
                _ =
                    Debug.log "Mouse Over Fired" ( index, domId )
            in
            ( model
            , Dom.getElement domId |> Task.attempt (toMsg << GotDropElement index domId)
            )

        GotDragElement index domId xy (Ok domElement) ->
            let
                element =
                    Element index domId domElement
            in
            ( State xy xy element element
                |> Just
                |> DnD
            , Cmd.none
            )

        GotDragElement _ _ _ _ ->
            ( DnD Nothing, Cmd.none )

        GotDropElement index dropElementId (Ok domElement) ->
            let
                element =
                    Element index dropElementId domElement
            in
            ( model |> mapDropElement (always element)
            , Cmd.none
            )

        GotDropElement _ _ _ ->
            ( model, Cmd.none )

        DragEnd ->
            ( DnD Nothing
            , info model
                |> Maybe.map (callbacks.onCommit >> perform)
                |> Maybe.withDefault Cmd.none
            )


perform =
    Task.succeed >> Task.perform identity
