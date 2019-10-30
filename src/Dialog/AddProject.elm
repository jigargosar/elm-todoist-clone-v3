module Dialog.AddProject exposing (Config, Model, Msg, SavedWith, init, update, view)

import Basics.More exposing (msgToCmd)
import Browser.Dom as Dom
import Dialog.SelectColor
import Dialog.UI
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A exposing (autofocus)
import Log exposing (logError)
import Task


type alias Model =
    { title : String
    , color : String
    , favorite : Bool
    , selectColor : Dialog.SelectColor.Model
    }


type alias SavedWith =
    { title : String, color : String, favorite : Bool }


initial : Model
initial =
    Model "" "" False Dialog.SelectColor.initial


init : Config msg -> ( Model, Cmd msg )
init { toMsg } =
    ( initial
    , Dom.focus autofocusDomId
        |> Task.attempt AutoFocus
        |> Cmd.map toMsg
    )


type Msg
    = Save
    | Cancel
    | Title String
    | Color String
    | SelectColor Dialog.SelectColor.Msg
    | Favorite Bool
    | AutoFocus (Result Dom.Error ())


type alias Config msg =
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update { saved, canceled, toMsg } message model =
    case message of
        Save ->
            ( model
            , SavedWith model.title model.color False
                |> saved
                |> msgToCmd
            )

        Cancel ->
            ( model, msgToCmd canceled )

        Title title ->
            ( { model | title = title }, Cmd.none )

        Color color ->
            ( { model | color = color }, Cmd.none )

        SelectColor msg ->
            SelectColor.update

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )

        AutoFocus result ->
            case result of
                Err (Dom.NotFound domId) ->
                    ( model, logError <| "autofocus failed: " ++ domId )

                Ok () ->
                    ( model, Cmd.none )


autofocusDomId =
    "add-project-dialog-autofocus"


selectColorConfig : Dialog.SelectColor.Config Msg
selectColorConfig =
    { toMsg = SelectColor }


view : Config msg -> Model -> Html msg
view { toMsg } model =
    Dialog.UI.viewForm
        { submit = Save
        , cancel = Cancel
        , title = "Add Project"
        , submitTitle = "add"
        , content =
            [ Dialog.UI.input
                { labelText = "Project name"
                , value = model.title
                , changed = Title
                , attrs = [ A.id autofocusDomId, autofocus True ]
                }
            , Dialog.UI.input
                { labelText = "Project color"
                , value = model.color
                , changed = Color
                , attrs = []
                }
            , Dialog.SelectColor.view selectColorConfig model.selectColor
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = Favorite
                }
            ]
        }
        |> H.map toMsg
