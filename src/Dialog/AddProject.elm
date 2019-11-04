module Dialog.AddProject exposing (Config, Model, Msg, SavedWith, initAt, subscriptions, update, view)

import Basics.More exposing (msgToCmd)
import Browser.Dom as Dom
import CColor exposing (CColor)
import Dialog.SelectColor as SelectColor
import Dialog.UI
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A exposing (autofocus)
import Log exposing (logError)
import Task


type alias Model =
    { title : String
    , favorite : Bool
    , selectColor : SelectColor.Model
    , cColor : CColor
    , idx : Int
    }


type alias SavedWith =
    { title : String
    , favorite : Bool
    , cColor : CColor
    , idx : Int
    }


initAt : Config msg -> Int -> ( Model, Cmd msg )
initAt { toMsg } idx =
    ( Model "" False SelectColor.initial CColor.default idx
    , Dom.focus autofocusDomId
        |> Task.attempt AutoFocus
        |> Cmd.map toMsg
    )


type Msg
    = Save
    | Cancel
    | Title String
    | SelectColor SelectColor.Msg
    | CColorChanged CColor
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
            , SavedWith model.title model.favorite model.cColor model.idx
                |> saved
                |> msgToCmd
            )

        Cancel ->
            ( model, msgToCmd canceled )

        Title title ->
            ( { model | title = title }, Cmd.none )

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
            ( { model | cColor = cColor }, Cmd.none )


autofocusDomId : String
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
        , submitTitle = "Add"
        , content =
            [ Dialog.UI.input
                { labelText = "Project name"
                , value = model.title
                , changed = Title
                , attrs = [ A.id autofocusDomId, autofocus True ]
                }
            , Dialog.UI.labeled "Project color"
                (SelectColor.view selectColorConfig model.cColor model.selectColor)
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = Favorite
                }
            ]
        }
        |> H.map toMsg
