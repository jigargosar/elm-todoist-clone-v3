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
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A exposing (autofocus)
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
subscriptions { toMsg } model =
    SelectColor.subscriptions selectColorConfig model.selectColor
        |> Sub.map toMsg


update : Config msg -> Msg -> AddProject -> ( AddProject, Cmd msg )
update { toMsg } message model =
    case message of
        Title title ->
            ( { model | title = title }, Cmd.none )

        SelectColor msg ->
            SelectColor.update selectColorConfig msg model.selectColor
                |> Tuple.mapBoth (\selectColor -> { model | selectColor = selectColor })
                    (Cmd.map toMsg)

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )

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


view : Config msg -> AddProject -> Html msg
view { toMsg, saved, canceled } model =
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
                , attrs = [ A.id autofocusDomId, autofocus True ]
                }
            , Dialog.UI.labeled "Project color"
                (SelectColor.view selectColorConfig model.cColor model.selectColor
                    |> H.map toMsg
                )
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = toMsg << Favorite
                }
            ]
        }
