port module Main exposing (main)

import Appbar
import Basics.More exposing (flip)
import Browser
import Browser.Dom as Dom exposing (Element, getElement)
import Browser.Events
import Css
import Drawer
import Html.Styled exposing (Html, toUnstyled)
import Html.Styled.Events as E
import Json.Decode as JD
import Json.Encode exposing (Value)
import Layout
import ProjectCollection exposing (ProjectCollection)
import Return
import SelectList
import Styles
import Task
import Todo exposing (Todo)
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


type alias Model =
    { todoDict : TodoDict
    , projectCollection : ProjectCollection
    , isDrawerModalOpen : Bool
    , drawerExpansionPanels : Drawer.ExpansionPanels
    , panelItemDnD : Maybe PanelItemDnD
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
            , panelItemDnD = Nothing
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
        [ case model.panelItemDnD of
            Just _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (pageXYDecoder |> JD.map BrowserMouseMove)
                    , Browser.Events.onMouseUp (JD.succeed BrowserMouseUp)
                    ]

            Nothing ->
                Sub.none
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
            ( { model | panelItemDnD = Nothing }
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
            case model.panelItemDnD of
                Nothing ->
                    ( model, Cmd.none )

                Just dnd ->
                    if dnd.panel == panel then
                        ( { model | panelItemDnD = Just { dnd | over = Just { idx = idx, id = domId } } }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        GotDrawerPanelItemDragElement panel idx domId xy el ->
            ( { model
                | panelItemDnD = PanelItemDnD panel idx domId el xy xy Nothing |> Just
              }
            , Cmd.none
            )

        GotDrawerPanelItemDomError (Dom.NotFound domId) ->
            ( { model | panelItemDnD = Nothing }, logError ("GotDrawerPanelItemDomError: " ++ domId) )

        BrowserMouseMove xy ->
            case model.panelItemDnD of
                Just dnd ->
                    ( { model | panelItemDnD = Just { dnd | currentXY = xy } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        BrowserMouseUp ->
            ( { model | panelItemDnD = Nothing }, Cmd.none )



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
        , main = mainView model.todoDict
        }
        model.isDrawerModalOpen


drawerView model =
    let
        drawerConfig : Drawer.Config Msg
        drawerConfig =
            { onToggleExpansionPanel = ToggleDrawerExpansionPanel
            , dragEvents = dragEvents
            , dropEvents = dropEvents
            , isPanelExpanded = \panel -> Drawer.isPanelExpanded panel model.drawerExpansionPanels
            , dragInfo = \panel -> dragInfoFor panel model.panelItemDnD
            , projectList = ProjectCollection.sorted model.projectCollection
            , sort = \p l -> sort p l model.panelItemDnD
            }
    in
    Drawer.view drawerConfig


mainView : TodoDict -> List (Html Msg)
mainView todoDict =
    [ Todo.viewList { toggle = ToggleTodoCompleted } (TodoDict.sortedByIdx todoDict)
    ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
