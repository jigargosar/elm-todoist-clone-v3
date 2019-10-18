port module Main exposing (main)

import Appbar
import Basics.More exposing (flip)
import Browser
import Browser.Dom as Dom exposing (Element, getElement)
import Browser.Events
import Css
import Drag exposing (Drag)
import Drawer
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes as A exposing (css)
import Html.Styled.Events as E
import Json.Decode as JD
import Json.Encode exposing (Value)
import Layout
import Project exposing (Project)
import ProjectCollection exposing (ProjectCollection)
import ProjectId
import Return
import SelectList
import Styles
import Task
import TodoDict exposing (TodoDict)
import TodoId exposing (TodoId)


port logError : String -> Cmd msg



-- Flags


type alias Flags =
    { todoList : Value
    , projectList : Value
    }



-- MODEL


type alias XY =
    { x : Float, y : Float }


subtractXY : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> XY
subtractXY a b =
    XY (a.x - b.x) (a.y - b.y)


addXY a b =
    XY (a.x + b.x) (a.y + b.y)


type alias PanelItemDnD =
    { panel : Drawer.Panel
    , idx : Int
    , id : String
    , el : Element
    , startXY : XY
    , currentXY : XY
    , over : Maybe { idx : Int, id : String }
    }


type alias ListDrag =
    Drag Int Int


type alias PanelsDragState =
    { projects : ListDrag
    , labels : ListDrag
    , filters : ListDrag
    }


type alias Model =
    { todoDict : TodoDict
    , projectCollection : ProjectCollection
    , isDrawerModalOpen : Bool
    , drawerExpansionPanels : Drawer.ExpansionPanels
    , drawerDnD : Maybe PanelItemDnD
    , drag : Drag ( Drawer.Panel, Int ) ( Drawer.Panel, Int )
    , projectsDrag : ListDrag
    , labelsDrag : ListDrag
    , filtersDrag : ListDrag
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initial : Model
        initial =
            { todoDict = TodoDict.initial
            , projectCollection = ProjectCollection.initial
            , isDrawerModalOpen = False
            , drawerExpansionPanels = Drawer.initialExpansionPanels
            , drawerDnD = Nothing
            , drag = Drag.initial
            , projectsDrag = Drag.initial
            , labelsDrag = Drag.initial
            , filtersDrag = Drag.initial
            }
    in
    Return.singleton initial
        |> Return.andThen
            (Return.pipelK
                [ initTodoDict flags.todoList
                , initProjectCollection flags.projectList
                ]
            )


initProjectCollection :
    JD.Value
    -> { a | projectCollection : ProjectCollection }
    -> ( { a | projectCollection : ProjectCollection }, Cmd msg )
initProjectCollection encodedProjectList model =
    let
        ( newProjectCollection, cmd ) =
            case ProjectCollection.fromEncodedList encodedProjectList of
                Ok new ->
                    ( new, Cmd.none )

                Err e ->
                    ( model.projectCollection, logError <| JD.errorToString e )
    in
    ( { model | projectCollection = newProjectCollection }, cmd )


initTodoDict :
    JD.Value
    -> { a | todoDict : TodoDict }
    -> ( { a | todoDict : TodoDict }, Cmd msg )
initTodoDict encodedTodoList model =
    let
        ( newTodoDict, cmd ) =
            case TodoDict.fromEncodedList encodedTodoList of
                Ok new ->
                    ( new, Cmd.none )

                Err e ->
                    ( model.todoDict, logError <| JD.errorToString e )
    in
    ( { model | todoDict = newTodoDict }, cmd )



-- Subscriptions


pageXYDecoder =
    JD.map2 XY (JD.field "pageX" JD.float) (JD.field "pageY" JD.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.drawerDnD of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (pageXYDecoder |> JD.map BrowserMouseMove)
                    , Browser.Events.onMouseUp (JD.succeed BrowserMouseUp)
                    ]

            Nothing ->
                Sub.none
        , Drag.subscriptions Drag model.drag
        ]



-- UPDATE


type Msg
    = NoOp
    | ToggleTodoCompleted TodoId
    | OpenDrawerModal
    | CloseDrawerModal
    | ToggleDrawerExpansionPanel Drawer.Panel
    | DrawerPanelItemMouseDown Drawer.Panel Int String XY
    | DrawerPanelItemMouseOver Drawer.Panel Int String
    | GotDrawerPanelItemDragElement Drawer.Panel Int String XY Element
    | GotDrawerPanelItemDomError Dom.Error
    | BrowserMouseMove XY
    | BrowserMouseUp
    | Drag (Drag.Msg ( Drawer.Panel, Int ) ( Drawer.Panel, Int ))
    | PanelDragList Drawer.Panel (Drag.Msg Int Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            Return.singleton model

        ToggleTodoCompleted todoId ->
            let
                newTodoDict =
                    TodoDict.toggleCompleted todoId model.todoDict
            in
            ( { model | todoDict = newTodoDict }, Cmd.none )

        OpenDrawerModal ->
            ( { model | isDrawerModalOpen = True }, Cmd.none )

        CloseDrawerModal ->
            ( { model | isDrawerModalOpen = False }, Cmd.none )

        ToggleDrawerExpansionPanel panel ->
            ( { model
                | drawerExpansionPanels =
                    Drawer.toggleExpansionPanel panel model.drawerExpansionPanels
              }
            , Cmd.none
            )

        DrawerPanelItemMouseDown panel idx domId xy ->
            ( { model | drawerDnD = Nothing }
            , getElement domId
                |> Task.attempt
                    (\elResult ->
                        case elResult of
                            Ok el ->
                                GotDrawerPanelItemDragElement panel idx domId xy el

                            Err err ->
                                GotDrawerPanelItemDomError err
                    )
            )

        DrawerPanelItemMouseOver panel idx domId ->
            case model.drawerDnD of
                Nothing ->
                    ( model, Cmd.none )

                Just dnd ->
                    if dnd.panel == panel then
                        ( { model | drawerDnD = Just { dnd | over = Just { idx = idx, id = domId } } }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        GotDrawerPanelItemDragElement panel idx domId xy el ->
            ( { model
                | drawerDnD = PanelItemDnD panel idx domId el xy xy Nothing |> Just
              }
            , Cmd.none
            )

        GotDrawerPanelItemDomError (Dom.NotFound domId) ->
            ( { model | drawerDnD = Nothing }, logError ("GotDrawerPanelItemDomError: " ++ domId) )

        BrowserMouseMove xy ->
            case model.drawerDnD of
                Just dnd ->
                    ( { model | drawerDnD = Just { dnd | currentXY = xy } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        BrowserMouseUp ->
            ( { model | drawerDnD = Nothing }, Cmd.none )

        Drag msg ->
            Drag.update Drag { canAccept = \( p1, _ ) ( p2, _ ) -> p1 == p2 } msg model.drag
                |> Tuple.mapFirst (\drag -> { model | drag = drag })

        PanelDragList panel msg ->
            let
                updateHelp =
                    Drag.update (PanelDragList panel) { canAccept = \_ _ -> True } msg
            in
            case panel of
                Drawer.Projects ->
                    updateHelp model.projectsDrag
                        |> Tuple.mapFirst (\drag -> { model | projectsDrag = drag })

                Drawer.Labels ->
                    updateHelp model.labelsDrag
                        |> Tuple.mapFirst (\drag -> { model | labelsDrag = drag })

                Drawer.Filters ->
                    updateHelp model.filtersDrag
                        |> Tuple.mapFirst (\drag -> { model | filtersDrag = drag })



-- VIEW


rotate : Int -> Int -> List a -> List a
rotate dragIdx dropIdx list =
    SelectList.fromList list
        |> Maybe.andThen (SelectList.selectBy dragIdx)
        |> Maybe.map (SelectList.moveBy (dropIdx - dragIdx) >> SelectList.toList)
        |> Maybe.withDefault list


sort : Drawer.Panel -> List a -> Maybe PanelItemDnD -> List a
sort panel items =
    Maybe.andThen
        (\dnd ->
            if dnd.panel == panel then
                dnd.over
                    |> Maybe.map
                        (\over ->
                            rotate dnd.idx over.idx items
                        )

            else
                Nothing
        )
        >> Maybe.withDefault items


dragEvents : Drawer.Panel -> Int -> String -> List (Html.Styled.Attribute Msg)
dragEvents panel idx domId =
    [ E.preventDefaultOn "mousedown"
        (pageXYDecoder
            |> JD.map (DrawerPanelItemMouseDown panel idx domId)
            |> JD.map (flip Tuple.pair True)
        )
    ]


dropEvents : Drawer.Panel -> Int -> String -> List (Html.Styled.Attribute Msg)
dropEvents panel idx domId =
    [ E.onMouseOver (DrawerPanelItemMouseOver panel idx domId)
    ]


dragInfoFor : Drawer.Panel -> Maybe PanelItemDnD -> Drawer.DragInfo
dragInfoFor panel_ =
    Maybe.andThen
        (\{ panel, idx, startXY, currentXY, el, over } ->
            if panel == panel_ then
                Just
                    { panel = panel
                    , dragIdx = idx
                    , dropIdx = over |> Maybe.map .idx |> Maybe.withDefault idx
                    , ghostStyles =
                        let
                            { x, y } =
                                addXY (subtractXY currentXY startXY)
                                    (subtractXY el.element el.viewport)
                        in
                        Css.batch
                            [ Styles.absolute
                            , Styles.top_0
                            , Styles.left_0
                            , Css.transform (Css.translate2 (Css.px 0) (Css.px y))
                            , Css.pointerEvents Css.none
                            ]
                    }

            else
                Nothing
        )


view : Model -> Html Msg
view model =
    Layout.view { closeDrawerModal = CloseDrawerModal }
        { appbar = Appbar.view { menuClicked = OpenDrawerModal }
        , drawer = drawerView model
        , main = mainView2 model
        }
        model.isDrawerModalOpen


drawerView : Model -> { content : List (Html Msg), portal : List (Html Msg) }
drawerView model =
    let
        drawerConfig : Drawer.Config Msg
        drawerConfig =
            { onToggleExpansionPanel = ToggleDrawerExpansionPanel
            , dragEvents = dragEvents
            , dropEvents = dropEvents
            , isPanelExpanded = \panel -> Drawer.isPanelExpanded panel model.drawerExpansionPanels
            , dragInfo = \panel -> dragInfoFor panel model.drawerDnD
            , projectList = ProjectCollection.sorted model.projectCollection
            , sort = \panel list -> sort panel list model.drawerDnD
            }
    in
    Drawer.view drawerConfig


rotateList : Int -> Int -> List a -> List a
rotateList dragIdx dragOverIdx list =
    SelectList.fromList list
        |> Maybe.andThen (SelectList.selectBy dragIdx)
        |> Maybe.map (SelectList.moveBy (dragOverIdx - dragIdx) >> SelectList.toList)
        |> Maybe.withDefault list


mainView2 : Model -> { content : List (Html Msg), portal : List (Html Msg) }
mainView2 model =
    let
        projectList =
            ProjectCollection.sorted model.projectCollection

        rotateProjectList : List a -> List a
        rotateProjectList =
            Drag.info model.drag
                |> Maybe.map
                    (\{ drag, dragOver } ->
                        case ( drag, dragOver ) of
                            ( ( Drawer.Projects, dragIdx ), ( Drawer.Projects, dragOverIdx ) ) ->
                                rotateList dragIdx dragOverIdx

                            _ ->
                                identity
                    )
                |> Maybe.withDefault identity

        eqDragOverIdx : Int -> Bool
        eqDragOverIdx idx =
            Drag.info model.drag
                |> Maybe.map
                    (\{ drag, dragOver } ->
                        case ( drag, dragOver ) of
                            ( ( Drawer.Projects, _ ), ( Drawer.Projects, dragOverIdx ) ) ->
                                idx == dragOverIdx

                            _ ->
                                False
                    )
                |> Maybe.withDefault False

        ghostItem =
            Drag.ghostStyles model.drag
                |> Maybe.andThen
                    (\( ( panel, idx ), styles ) ->
                        if panel == Drawer.Projects then
                            List.drop idx projectList |> List.head |> Maybe.map (Tuple.pair styles)

                        else
                            Nothing
                    )
                |> Maybe.map
                    (\( styles, project ) ->
                        div
                            [ css [ styles ] ]
                            [ text <| Project.title project ]
                    )
                |> Maybe.withDefault (text "")
    in
    { content =
        projectList
            |> rotateProjectList
            |> List.indexedMap
                (\idx project ->
                    let
                        domId =
                            project
                                |> Project.id
                                >> ProjectId.toString
                                >> (++) "project-dnd-item"

                        dragOverStyles =
                            Styles.styleIf (eqDragOverIdx idx) [ Css.opacity <| Css.zero ]
                    in
                    div
                        (A.id domId
                            :: css [ Styles.noSelection, Styles.commonTransitions, dragOverStyles ]
                            :: Drag.dragEvents Drag ( Drawer.Projects, idx ) domId model.drag
                            ++ Drag.dropEvents Drag ( Drawer.Projects, idx ) model.drag
                        )
                        [ text <| Project.title project ]
                )
    , portal = [ ghostItem ]
    }



--mainView : TodoDict -> List (Html Msg)
--mainView todoDict =
--    [ Todo.viewList { toggle = ToggleTodoCompleted } (TodoDict.sortedByIdx todoDict)
--    ]
--
--
-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
