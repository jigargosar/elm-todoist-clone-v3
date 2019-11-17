module Dialog.AddProject exposing
    ( Config
    , Model
    , Msg
    , SavedWith
    , System
    , createConfig
    , system
    )

import CColor exposing (CColor)
import Cmds
import Dialog.SelectColor as SelectColor
import Dialog.UI
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Attributes as A
import Lens
import Ret exposing (Ret, RetF)


type alias System msg =
    { initAt : Int -> ( Model, String )
    , subscriptions : Model -> Sub msg
    , update : Msg -> Model -> ( Model, Cmd msg )
    , view : Model -> Html msg
    }


system :
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    }
    -> System msg
system cp =
    let
        config =
            createConfig cp
    in
    { initAt = initAt
    , subscriptions = subscriptions config
    , update = update config
    , view = view config
    }


type alias Config msg =
    { onSubmit : msg
    , onCancel : msg
    , onTitle : String -> msg
    , onFav : Bool -> msg
    , selectColor : SelectColor.System msg
    , emitSaved : SavedWith -> Cmd msg
    }


createConfig :
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    }
    -> Config msg
createConfig { toMsg, saved, canceled } =
    { onSubmit = toMsg Save
    , onCancel = canceled
    , onTitle = toMsg << Title
    , onFav = toMsg << Favorite
    , selectColor =
        SelectColor.system
            { toMsg = toMsg << SelectColor
            , domIdPrefix = "add-project-dialog"
            , changed = toMsg << CColor
            }
    , emitSaved = Cmds.fromMsg << saved
    }


type alias Model =
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


toSavedWith : Model -> SavedWith
toSavedWith model =
    SavedWith model.title model.favorite model.cColor model.idx


initAt : Int -> ( Model, String )
initAt idx =
    ( Model "" False SelectColor.initial CColor.default idx
    , autofocusDomId
    )


type Msg
    = Title String
    | SelectColor SelectColor.Msg
    | CColor CColor
    | Favorite Bool
    | Save


subscriptions : Config msg -> Model -> Sub msg
subscriptions c model =
    c.selectColor.subscriptions model.selectColor


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update c message model =
    case message of
        Title title ->
            ( { model | title = title }, Cmd.none )

        SelectColor msg ->
            Ret.updateSub fields.selectColor c.selectColor.update msg model

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )

        CColor cColor ->
            ( { model | cColor = cColor }, Cmd.none )

        Save ->
            ( model, toSavedWith model |> c.emitSaved )


autofocusDomId : String
autofocusDomId =
    "add-project-dialog-autofocus"


view : Config msg -> Model -> Html msg
view c model =
    Dialog.UI.viewForm
        { submit = c.onSubmit
        , cancel = c.onCancel
        , title = "Add Project"
        , submitTitle = "Add"
        , content =
            [ Dialog.UI.input
                { labelText = "Project name"
                , value = model.title
                , changed = c.onTitle
                , attrs = [ A.id autofocusDomId ]
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
