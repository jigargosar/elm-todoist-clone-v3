module Dialog.EditProject exposing
    ( Config
    , Model
    , Msg
    , SavedWith
    , System
    , createConfig
    , subscriptions
    , system
    , view
    )

import CColor exposing (CColor)
import Cmds
import Dialog.SelectColor as SelectColor
import Dialog.UI
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Attributes as A exposing (autofocus)
import Lens
import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Ret exposing (RetF)


type alias System msg =
    { init : Project -> ( Model, String )
    , subscriptions : Model -> Sub msg
    , update : Msg -> Model -> ( Model, Cmd msg )
    , view : Model -> Html msg
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
    , view = view config
    }


type alias Config msg =
    { selectColor : SelectColor.System msg
    , onCancel : msg
    , onSubmit : msg
    , onTitle : String -> msg
    , onFav : Bool -> msg
    , emitSaved : SavedWith -> Cmd msg
    , onSavedWith : SavedWith -> msg
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
    { selectColor = SelectColor.system selectColorConfig
    , onCancel = canceled
    , onSubmit = toMsg Submit
    , onTitle = toMsg << Title
    , onFav = toMsg << Favorite
    , emitSaved = Cmds.fromMsg << saved
    , onSavedWith = saved
    }


type alias Model =
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


init : Config msg -> Project -> ( Model, String )
init _ project =
    ( Model (Project.id project)
        (Project.title project)
        False
        SelectColor.initial
        (Project.cColor project)
    , autofocusDomId
    )


type Msg
    = Submit
    | Title String
    | SelectColor SelectColor.Msg
    | CColor CColor
    | Favorite Bool


subscriptions : Config msg -> Model -> Sub msg
subscriptions config model =
    config.selectColor.subscriptions model.selectColor


toSavedWith : Model -> SavedWith
toSavedWith model =
    SavedWith model.projectId model.title model.favorite model.cColor


updateF : Config msg -> Msg -> RetF Model msg
updateF c message =
    case message of
        Submit ->
            Ret.addEffect (toSavedWith >> c.emitSaved)

        Title title ->
            Ret.setSub fields.title title

        CColor cColor ->
            Ret.setSub fields.cColor cColor

        Favorite favorite ->
            Ret.setSub fields.favorite favorite

        SelectColor msg ->
            Ret.updateSubF fields.selectColor c.selectColor.updateF msg


autofocusDomId : String
autofocusDomId =
    "edit-project-dialog-autofocus"


view : Config msg -> Model -> Html msg
view c model =
    Dialog.UI.viewForm
        { submit = c.onSubmit
        , cancel = c.onCancel
        , title = "Edit Project"
        , submitTitle = "Save"
        , content =
            [ Dialog.UI.input
                { labelText = "Project name"
                , value = model.title
                , changed = c.onTitle
                , attrs = [ A.id autofocusDomId, autofocus True ]
                }
            , Dialog.UI.labeled "Project color"
                (c.selectColor.view model.cColor model.selectColor)
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = c.onFav
                }
            ]
        }
