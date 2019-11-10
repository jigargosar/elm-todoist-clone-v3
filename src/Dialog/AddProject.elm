module Dialog.AddProject exposing
    ( AddProject
    , Config
    , Msg
    , SavedWith
    , createConfig
    , initAt
    , subscriptions
    , update
    , view
    )

import CColor exposing (CColor)
import Dialog.SelectColor as SelectColor
import Dialog.UI
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A
import Lens
import Ret exposing (Ret, RetF)


type alias Config msg =
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    , selectColor : SelectColor.System msg
    }


createConfig : { toMsg : Msg -> msg, saved : SavedWith -> msg, canceled : msg } -> Config msg
createConfig { saved, canceled, toMsg } =
    let
        selectColorConfig : SelectColor.Config msg
        selectColorConfig =
            { toMsg = toMsg << SelectColor
            , domIdPrefix = "add-project-dialog"
            , changed = toMsg << CColor
            }
    in
    { toMsg = toMsg
    , saved = saved
    , canceled = canceled
    , selectColor = SelectColor.system selectColorConfig
    }


type alias AddProject =
    { title : String
    , favorite : Bool
    , selectColor : SelectColor.Model
    , cColor : CColor
    , idx : Int
    }


fields =
    { selectColor = Lens.fromTuple ( .selectColor, \s b -> { b | selectColor = s } )
    }


type alias SavedWith =
    { title : String
    , favorite : Bool
    , cColor : CColor
    , idx : Int
    }


initAt : Int -> ( AddProject, String )
initAt idx =
    ( AddProject "" False SelectColor.initial CColor.default idx
    , autofocusDomId
    )


type Msg
    = Title String
    | SelectColor SelectColor.Msg
    | CColor CColor
    | Favorite Bool
    | Save


subscriptions : Config msg -> AddProject -> Sub msg
subscriptions { toMsg, selectColor } model =
    selectColor.subscriptions model.selectColor


toSavedWith model =
    SavedWith model.title model.favorite model.cColor model.idx


update : Config msg -> Msg -> AddProject -> ( AddProject, Cmd msg )
update { saved, selectColor } message model =
    case message of
        Title title ->
            ( { model | title = title }, Cmd.none )

        SelectColor msg ->
            Ret.updateSub fields.selectColor selectColor.update msg model

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )

        CColor cColor ->
            ( { model | cColor = cColor }, Cmd.none )

        Save ->
            ( model, Ret.toCmd (toSavedWith model |> saved) )


autofocusDomId : String
autofocusDomId =
    "add-project-dialog-autofocus"


view : Config msg -> AddProject -> Html msg
view { toMsg, canceled, selectColor } model =
    Dialog.UI.viewForm
        { submit = toMsg Save
        , cancel = canceled
        , title = "Add Project"
        , submitTitle = "Add"
        , content =
            [ Dialog.UI.input
                { labelText = "Project name"
                , value = model.title
                , changed = toMsg << Title
                , attrs = [ A.id autofocusDomId ]
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
