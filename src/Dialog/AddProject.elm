module Dialog.AddProject exposing
    ( AddProject
    , Config
    , Msg
    , SavedWith
    , System
    , initAt
    , subscriptions
    , system
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


type alias System msg =
    { initAt : Int -> ( AddProject, String )
    , subscriptions : AddProject -> Sub msg
    , update : Msg -> AddProject -> ( AddProject, Cmd msg )
    , view : AddProject -> Html msg
    }


system : Config msg -> System msg
system config =
    { initAt = initAt
    , subscriptions = subscriptions config
    , update = update config
    , view = view config
    }


type alias Config msg =
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    }


selectColor : SelectColor.System Msg
selectColor =
    SelectColor.system
        { toMsg = SelectColor
        , domIdPrefix = "add-project-dialog"
        , changed = CColor
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
    | Cancel


subscriptions : Config msg -> AddProject -> Sub msg
subscriptions { toMsg } model =
    selectColor.subscriptions model.selectColor |> Sub.map toMsg


toSavedWith model =
    SavedWith model.title model.favorite model.cColor model.idx


update : Config msg -> Msg -> AddProject -> ( AddProject, Cmd msg )
update { toMsg, saved, canceled } message model =
    case message of
        Title title ->
            ( { model | title = title }, Cmd.none )

        SelectColor msg ->
            Ret.updateSub fields.selectColor selectColor.update msg model
                |> Ret.mapCmd toMsg

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )

        CColor cColor ->
            ( { model | cColor = cColor }, Cmd.none )

        Save ->
            ( model, Ret.toCmd (toSavedWith model |> saved) )

        Cancel ->
            ( model, Ret.toCmd canceled )


autofocusDomId : String
autofocusDomId =
    "add-project-dialog-autofocus"


view : Config msg -> AddProject -> Html msg
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
                , attrs = [ A.id autofocusDomId ]
                }
            , Dialog.UI.labeled "Project color"
                (selectColor.view model.cColor model.selectColor)
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = Favorite
                }
            ]
        }
        |> H.map toMsg
