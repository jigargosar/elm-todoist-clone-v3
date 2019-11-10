module Dialog.AddProject exposing
    ( AddProject
    , Config
    , Msg
    , SavedWith
    , System
    , subscriptions
    , system
    , update
    , view
    )

import CColor exposing (CColor)
import Dialog.SelectColor as SelectColor
import Dialog.UI
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Attributes as A exposing (autofocus)
import Lens
import Ret exposing (Ret, RetF)


type alias System msg =
    { initAt : Int -> ( AddProject, String )
    , subscriptions : AddProject -> Sub msg
    , update : Msg -> AddProject -> Ret AddProject msg
    , view : AddProject -> Html msg
    }


type alias Config msg =
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    , selectColorSys : SelectColor.System msg
    }


system :
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    }
    -> System msg
system c =
    let
        config : Config msg
        config =
            { toMsg = c.toMsg
            , saved = c.saved
            , canceled = c.canceled
            , selectColorSys =
                SelectColor.system
                    { toMsg = c.toMsg << SelectColor
                    , domIdPrefix = "add-project-dialog"
                    , changed = c.toMsg << CColorChanged
                    }
            }
    in
    { initAt = initAt
    , subscriptions = subscriptions config
    , update = update config
    , view = view config
    }


type alias AddProject =
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


initAt : Int -> ( AddProject, String )
initAt idx =
    ( AddProject "" False SelectColor.initial CColor.default idx
    , autofocusDomId
    )


type Msg
    = Title String
    | SelectColor SelectColor.Msg
    | CColorChanged CColor
    | Favorite Bool


subscriptions : Config msg -> AddProject -> Sub msg
subscriptions { toMsg, selectColorSys } model =
    selectColorSys.subscriptions model.selectColor


fields =
    { selectColor = Lens.fromTuple ( .selectColor, \s b -> { b | selectColor = s } )
    }


update : Config msg -> Msg -> AddProject -> ( AddProject, Cmd msg )
update { toMsg, selectColorSys } message model =
    case message of
        Title title ->
            ( { model | title = title }, Cmd.none )

        SelectColor msg ->
            Ret.updateSub fields.selectColor selectColorSys.update msg model

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )

        CColorChanged cColor ->
            ( { model | cColor = cColor }, Cmd.none )


autofocusDomId : String
autofocusDomId =
    "add-project-dialog-autofocus"


view : Config msg -> AddProject -> Html msg
view { toMsg, saved, canceled, selectColorSys } model =
    Dialog.UI.viewForm
        { submit =
            SavedWith model.title model.favorite model.cColor model.idx
                |> saved
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
                (selectColorSys.view model.cColor model.selectColor)
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = toMsg << Favorite
                }
            ]
        }
