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
import Html.Styled.Attributes as A
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
    }


type alias Context msg =
    { config : Config msg
    , sys : { selectColor : SelectColor.System msg }
    }


system :
    Config msg
    -> System msg
system config =
    let
        ctx : Context msg
        ctx =
            { config = config
            , sys =
                { selectColor =
                    SelectColor.system
                        { toMsg = config.toMsg << SelectColor
                        , domIdPrefix = "add-project-dialog"
                        , changed = config.toMsg << CColorChanged
                        }
                }
            }
    in
    { initAt = initAt
    , subscriptions = subscriptions ctx
    , update = update ctx
    , view = view ctx
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
    | CColorChanged CColor
    | Favorite Bool
    | Saved


subscriptions : Context msg -> AddProject -> Sub msg
subscriptions { sys } model =
    sys.selectColor.subscriptions model.selectColor


update : Context msg -> Msg -> AddProject -> ( AddProject, Cmd msg )
update { config, sys } message model =
    case message of
        Title title ->
            ( { model | title = title }, Cmd.none )

        SelectColor msg ->
            Ret.updateSub fields.selectColor sys.selectColor.update msg model

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )

        CColorChanged cColor ->
            ( { model | cColor = cColor }, Cmd.none )

        Saved ->
            ( model
            , Ret.toCmd
                (SavedWith model.title model.favorite model.cColor model.idx
                    |> config.saved
                )
            )


autofocusDomId : String
autofocusDomId =
    "add-project-dialog-autofocus"


view : Context msg -> AddProject -> Html msg
view { config, sys } model =
    let
        { toMsg, canceled } =
            config
    in
    Dialog.UI.viewForm
        { submit = toMsg Saved
        , cancel = config.canceled
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
                (sys.selectColor.view model.cColor model.selectColor)
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = toMsg << Favorite
                }
            ]
        }
