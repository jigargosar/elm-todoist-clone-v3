module Dialog.EditProject exposing
    ( Config
    , EditProject
    , Msg
    , SavedWith
    , System
    , createConfig
    , init
    , subscriptions
    , system
    , view
    )

import Browser.Dom as Dom
import CColor exposing (CColor)
import Cmds
import Dialog.SelectColor as SelectColor
import Dialog.UI
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Attributes as A exposing (autofocus)
import Lens
import Log
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Ret exposing (RetF)


type alias System msg =
    { init : Project -> ( EditProject, Cmd msg )
    , subscriptions : EditProject -> Sub msg
    , update : Msg -> EditProject -> ( EditProject, Cmd msg )
    , updateF : Msg -> RetF EditProject msg
    , view : EditProject -> Html msg
    }


system : { toMsg : Msg -> msg, saved : SavedWith -> msg, canceled : msg } -> System msg
system configParams =
    let
        config : Config msg
        config =
            createConfig configParams
    in
    { init = init config
    , subscriptions = subscriptions config
    , update = Ret.fromUpdateF (updateF config)
    , updateF = updateF config
    , view = view config
    }


type alias Config msg =
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    , selectColor : SelectColor.System msg
    }


createConfig : { toMsg : Msg -> msg, saved : SavedWith -> msg, canceled : msg } -> Config msg
createConfig { toMsg, saved, canceled } =
    let
        selectColorConfig : SelectColor.Config msg
        selectColorConfig =
            { toMsg = toMsg << SelectColor
            , domIdPrefix = "edit-project-dialog"
            , changed = toMsg << CColor
            }
    in
    { toMsg = toMsg
    , saved = saved
    , canceled = canceled
    , selectColor = SelectColor.system selectColorConfig
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
    , Cmds.focus autofocusDomId (toMsg << AutoFocus)
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
    config.selectColor.subscriptions model.selectColor


toSavedWith : EditProject -> SavedWith
toSavedWith model =
    SavedWith model.projectId model.title model.favorite model.cColor


updateF : Config msg -> Msg -> RetF EditProject msg
updateF config message =
    let
        { saved, canceled, selectColor } =
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
            Ret.updateSubF fields.selectColor selectColor.updateF msg

        AutoFocus result ->
            Ret.addError Log.focusError result


autofocusDomId : String
autofocusDomId =
    "edit-project-dialog-autofocus"


view : Config msg -> EditProject -> Html msg
view { selectColor, canceled, saved, toMsg } model =
    Dialog.UI.viewForm
        { submit = saved (toSavedWith model)
        , cancel = canceled
        , title = "Edit Project"
        , submitTitle = "Save"
        , content =
            [ Dialog.UI.input
                { labelText = "Project name"
                , value = model.title
                , changed = toMsg << Title
                , attrs = [ A.id autofocusDomId, autofocus True ]
                }
            , Dialog.UI.labeled "Project color"
                (selectColor.view model.cColor model.selectColor)
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = toMsg << Favorite
                }
            ]
        }
