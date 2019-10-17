port module Main exposing (main)

import Appbar
import Browser
import Browser.Dom as Dom exposing (Element, getElement)
import Browser.Events
import Drawer
import Html.Styled exposing (Html, toUnstyled)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Layout
import ProjectCollection exposing (ProjectCollection)
import Return
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


initTodoDict : JD.Value -> { a | todoDict : TodoDict } -> ( { a | todoDict : TodoDict }, Cmd msg )
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
                    [ Browser.Events.onMouseMove (pageXYDecoder |> JD.map GlobalMouseMove)
                    , Browser.Events.onMouseUp (JD.succeed GlobalMouseUp)
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
    | GotDrawerPanelItemDragElement Drawer.Panel Int String XY Element
    | GotDrawerPanelItemDomError Dom.Error
    | GlobalMouseMove XY
    | GlobalMouseUp


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

        GotDrawerPanelItemDragElement panel idx domId xy el ->
            ( { model
                | panelItemDnD = PanelItemDnD panel idx domId el xy xy Nothing |> Just
              }
            , Cmd.none
            )

        GotDrawerPanelItemDomError (Dom.NotFound domId) ->
            ( { model | panelItemDnD = Nothing }, logError ("GotDrawerPanelItemDomError: " ++ domId) )

        GlobalMouseMove xy ->
            case model.panelItemDnD of
                Just dnd ->
                    ( { model | panelItemDnD = Just { dnd | currentXY = xy } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GlobalMouseUp ->
            ( { model | panelItemDnD = Nothing }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Layout.view { closeDrawerModal = CloseDrawerModal }
        { appbar = Appbar.view { menuClicked = OpenDrawerModal }
        , drawer =
            Drawer.view { onToggleExpansionPanel = ToggleDrawerExpansionPanel }
                (ProjectCollection.sorted model.projectCollection)
                model.drawerExpansionPanels
        , main = mainView model.todoDict
        }
        model.isDrawerModalOpen


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
