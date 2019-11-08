module Dialog.EditProject exposing (Config, EditProject, Msg, SavedWith, System, init, subscriptions, system, update, view)

import Browser.Dom as Dom
import CColor exposing (CColor)
import Dialog.SelectColor as SelectColor
import Dialog.UI
import Html.Styled exposing (Attribute, Html)
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


type alias Config msg =
    { saved : SavedWith -> msg
    , canceled : msg
    , toMsg : Msg -> msg
    , selectColor : SelectColor.Config msg
    , title : String -> msg
    , favorite : Bool -> msg
    }


system :
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    }
    -> System msg
system ({ saved, canceled, toMsg } as cc) =
    let
        selectColorConfig : SelectColor.Config msg
        selectColorConfig =
            { toMsg = toMsg << SelectColor
            , domIdPrefix = "edit-project-dialog"
            , changed = toMsg << CColor
            }

        config : Config msg
        config =
            { saved = saved
            , canceled = canceled
            , toMsg = toMsg
            , selectColor = selectColorConfig
            , title = toMsg << Title
            , favorite = toMsg << Favorite
            }
    in
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
    ( EditProject (Project.id project)
        (Project.title project)
        False
        SelectColor.initial
        (Project.cColor project)
    , Dom.focus autofocusDomId
        |> Task.attempt AutoFocus
        |> Cmd.map toMsg
    )


type Msg
    = Save
    | Title String
    | SelectColor SelectColor.Msg
    | CColor CColor
    | Favorite Bool
    | AutoFocus (Result Dom.Error ())


subscriptions : Config msg -> EditProject -> Sub msg
subscriptions config model =
    SelectColor.subscriptions config.selectColor model.selectColor


toSavedWith : EditProject -> SavedWith
toSavedWith model =
    SavedWith model.projectId model.title model.favorite model.cColor


updateF : Config msg -> Msg -> RetF EditProject msg
updateF config message =
    let
        { saved, canceled } =
            config
    in
    case message of
        Save ->
            Ret.addMsgEffect (toSavedWith >> saved)

        Title title ->
            Ret.setSub fields.title title

        CColor cColor ->
            Ret.setSub fields.cColor cColor

        Favorite favorite ->
            Ret.setSub fields.favorite favorite

        SelectColor msg ->
            Ret.andThen
                (Ret.updateSub fields.selectColor (SelectColor.update config.selectColor) msg)

        AutoFocus result ->
            Ret.add
                (case result of
                    Err (Dom.NotFound domId) ->
                        logError <| "autofocus failed: " ++ domId

                    Ok () ->
                        Cmd.none
                )


update : Config msg -> Msg -> EditProject -> ( EditProject, Cmd msg )
update config =
    Ret.fromUpdateF (updateF config)


autofocusDomId : String
autofocusDomId =
    "edit-project-dialog-autofocus"


view : Config msg -> EditProject -> Html msg
view { selectColor, canceled, saved, title, favorite } model =
    Dialog.UI.viewForm
        { submit = saved (toSavedWith model)
        , cancel = canceled
        , title = "Edit Project"
        , submitTitle = "Save"
        , content =
            [ Dialog.UI.input
                { labelText = "Project name"
                , value = model.title
                , changed = title
                , attrs = [ A.id autofocusDomId, autofocus True ]
                }
            , Dialog.UI.labeled "Project color"
                (SelectColor.view selectColor model.cColor model.selectColor)
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = favorite
                }
            ]
        }
