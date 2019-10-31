module Dialog.AddProject exposing (Config, Model, Msg, SavedWith, init, subscriptions, update, view)

import Basics.More exposing (msgToCmd)
import Browser.Dom as Dom
import Dialog.SelectColor as SelectColor exposing (CColor)
import Dialog.UI
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A exposing (autofocus)
import Log exposing (logError)
import Task


type alias Model =
    { title : String
    , color : String
    , favorite : Bool
    , selectColor : SelectColor.Model
    }


type alias SavedWith =
    { title : String
    , color : String
    , favorite : Bool
    , cColor : CColor
    }


initial : Model
initial =
    Model "" "" False SelectColor.initial


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
    | SelectColor SelectColor.Msg
    | CColorChanged SelectColor.CColor
    | Favorite Bool
    | AutoFocus (Result Dom.Error ())


type alias Config msg =
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    }


subscriptions : Config msg -> Model -> Sub msg
subscriptions { toMsg } model =
    SelectColor.subscriptions selectColorConfig model.selectColor
        |> Sub.map toMsg


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update { saved, canceled, toMsg } message model =
    case message of
        Save ->
            ( model
            , SavedWith model.title model.color model.favorite (SelectColor.selected model.selectColor)
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
            SelectColor.update selectColorConfig msg model.selectColor
                |> Tuple.mapBoth (\selectColor -> { model | selectColor = selectColor })
                    (Cmd.map toMsg)

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )

        AutoFocus result ->
            case result of
                Err (Dom.NotFound domId) ->
                    ( model, logError <| "autofocus failed: " ++ domId )

                Ok () ->
                    ( model, Cmd.none )

        CColorChanged cColor ->
            ( model, Cmd.none )


autofocusDomId =
    "add-project-dialog-autofocus"


selectColorConfig : SelectColor.Config Msg
selectColorConfig =
    { toMsg = SelectColor
    , domIdPrefix = "add-project-dialog"
    , changed = CColorChanged
    }


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
            , SelectColor.view selectColorConfig model.selectColor
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = Favorite
                }
            ]
        }
        |> H.map toMsg
