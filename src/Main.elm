port module Main exposing (main)

import Appbar
import Browser
import Browser.Dom as Dom exposing (Element, getElement)
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


type alias PanelItemDrag =
    { panel : Drawer.Panel
    , idx : Int
    , id : String
    , el : Element
    , startXY : XY
    , currentXY : XY
    }


type alias Model =
    { todoDict : TodoDict
    , projectCollection : ProjectCollection
    , isDrawerModalOpen : Bool
    , drawerExpansionPanels : Drawer.ExpansionPanels
    , panelItemDrag : Maybe PanelItemDrag
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
            , panelItemDrag = Nothing
            }
    in
    Return.singleton initial
        |> Return.andThen
            (Return.pipelK
                [ initTodoDict flags.todoList
                , initProjectCollection flags.projectList
                ]
            )


initProjectCollection : JD.Value -> { a | projectCollection : ProjectCollection } -> ( { a | projectCollection : ProjectCollection }, Cmd msg )
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []



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
            ( { model | panelItemDrag = Nothing }
            , getElement domId
                |> Task.attempt
                    (\r ->
                        case r of
                            Ok el ->
                                GotDrawerPanelItemDragElement panel idx domId xy el

                            Err err ->
                                GotDrawerPanelItemDomError err
                    )
            )

        GotDrawerPanelItemDragElement panel idx domId xy el ->
            ( { model | panelItemDrag = Just <| PanelItemDrag panel idx domId el xy xy }, Cmd.none )

        GotDrawerPanelItemDomError (Dom.NotFound domId) ->
            ( { model | panelItemDrag = Nothing }, logError ("GotDrawerPanelItemDomError: " ++ domId) )



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
