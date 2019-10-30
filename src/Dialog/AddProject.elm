module Dialog.AddProject exposing (Config, Model, Msg, SavedWith, init, update, view)

import Basics.More exposing (msgToCmd)
import Css
import Dialog.UI
import Html.Styled as H exposing (Attribute, Html, div, form, span, text)
import Html.Styled.Attributes as A exposing (autofocus, css)
import Html.Styled.Events exposing (onSubmit)
import Key
import Px as PX
import Styles exposing (..)
import Theme


type alias Model =
    { title : String
    , color : String
    , favorite : Bool
    }


type alias SavedWith =
    { title : String, color : String, favorite : Bool }


initial : Model
initial =
    Model "" "" False


init : Config msg -> ( Model, Cmd msg )
init _ =
    ( initial, Cmd.none )


type Msg
    = Save
    | Cancel
    | Title String
    | Color String
    | Favorite Bool


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

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )


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
                , attrs = [ A.id "add-project-dialog-autofocus", autofocus True ]
                }
            , Dialog.UI.input
                { labelText = "Project color"
                , value = model.color
                , changed = Color
                , attrs = []
                }
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = Favorite
                }
            ]
        }
        |> H.map toMsg
