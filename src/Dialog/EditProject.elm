module Dialog.EditProject exposing (Config, EditProject, Msg, SavedWith, System, init, subscriptions, system, update, view)

import Basics.More exposing (msgToCmd)
import Browser.Dom as Dom
import CColor exposing (CColor)
import Dialog.SelectColor as SelectColor
import Dialog.UI
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A exposing (autofocus)
import Lens
import Log exposing (logError)
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Ret exposing (RetF)
import Task


type alias System msg =
    { init : Project -> ( EditProject, Cmd msg )
    , subscriptions : EditProject -> Sub msg
    , update : Msg -> EditProject -> ( EditProject, Cmd msg )
    , updateF : Msg -> RetF EditProject msg
    , view : EditProject -> Html msg
    }


system : Config msg -> System msg
system config =
    { init = init config
    , subscriptions = subscriptions config
    , update = update config
    , updateF = Ret.toUpdateF (update config)
    , view = view config
    }


type alias EditProject =
    { projectId : ProjectId
    , title : String
    , favorite : Bool
    , selectColor : SelectColor.Model
    , cColor : CColor
    }


fields =
    { projectId = Lens.fromTuple ( .projectId, \s b -> { b | projectId = s } )
    , title = Lens.fromTuple ( .title, \s b -> { b | title = s } )
    , favorite = Lens.fromTuple ( .favorite, \s b -> { b | favorite = s } )
    , selectColor = Lens.fromTuple ( .selectColor, \s b -> { b | selectColor = s } )
    , cColor = Lens.fromTuple ( .cColor, \s b -> { b | cColor = s } )
    }


type alias SavedWith =
    { projectId : ProjectId
    , title : String
    , favorite : Bool
    , cColor : CColor
    }


init : Config msg -> Project -> ( EditProject, Cmd msg )
init { toMsg } project =
    ( EditProject (Project.id project) (Project.title project) False SelectColor.initial (Project.cColor project)
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


subscriptions : Config msg -> EditProject -> Sub msg
subscriptions { toMsg } model =
    SelectColor.subscriptions selectColorConfig model.selectColor
        |> Sub.map toMsg


toSavedWith : EditProject -> SavedWith
toSavedWith model =
    SavedWith model.projectId model.title model.favorite model.cColor


updateF : Config msg -> Msg -> RetF EditProject msg
updateF { saved, canceled, toMsg } message =
    case message of
        Save ->
            Ret.addMsgEffect (toSavedWith >> saved)

        Cancel ->
            Ret.addMsg canceled

        Title title ->
            Ret.setSub fields.title title

        SelectColor msg ->
            Ret.andThen
                (\model ->
                    SelectColor.update selectColorConfig msg model.selectColor
                        |> Tuple.mapBoth (\selectColor -> { model | selectColor = selectColor })
                            (Cmd.map toMsg)
                )

        Favorite favorite ->
            Ret.setSub fields.favorite favorite

        AutoFocus result ->
            Ret.add
                (case result of
                    Err (Dom.NotFound domId) ->
                        logError <| "autofocus failed: " ++ domId

                    Ok () ->
                        Cmd.none
                )

        CColorChanged cColor ->
            Ret.map (\model -> { model | cColor = cColor })


update : Config msg -> Msg -> EditProject -> ( EditProject, Cmd msg )
update { saved, canceled, toMsg } message model =
    case message of
        Save ->
            ( model
            , SavedWith model.projectId model.title model.favorite model.cColor
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
    "edit-project-dialog-autofocus"


selectColorConfig : SelectColor.Config Msg
selectColorConfig =
    { toMsg = SelectColor
    , domIdPrefix = "edit-project-dialog"
    , changed = CColorChanged
    }


view : Config msg -> EditProject -> Html msg
view { toMsg } model =
    Dialog.UI.viewForm
        { submit = Save
        , cancel = Cancel
        , title = "Edit Project"
        , submitTitle = "Save"
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
