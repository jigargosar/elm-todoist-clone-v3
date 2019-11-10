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

import Basics.More exposing (msgToCmd)
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
    { titleChanged : String -> msg
    , favoriteChanged : Bool -> msg
    , cColorChanged : CColor -> msg
    , saved : msg
    , canceled : msg
    , savedWithCmd : SavedWith -> Cmd msg
    , selectColorSys : SelectColor.System msg
    }


system : Config msg -> System msg
system config =
    let
        ctx : Context msg
        ctx =
            { canceled = config.canceled
            , titleChanged = config.toMsg << Title
            , favoriteChanged = config.toMsg << Favorite
            , cColorChanged = config.toMsg << CColorChanged
            , saved = config.toMsg Saved
            , savedWithCmd = config.saved >> msgToCmd
            , selectColorSys =
                SelectColor.system
                    { toMsg = config.toMsg << SelectColor
                    , domIdPrefix = "add-project-dialog"
                    , changed = config.toMsg << CColorChanged
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
subscriptions { selectColorSys } model =
    selectColorSys.subscriptions model.selectColor


update : Context msg -> Msg -> AddProject -> ( AddProject, Cmd msg )
update { savedWithCmd, selectColorSys } message model =
    case message of
        Title title ->
            ( { model | title = title }, Cmd.none )

        SelectColor msg ->
            Ret.updateSub fields.selectColor selectColorSys.update msg model

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )

        CColorChanged cColor ->
            ( { model | cColor = cColor }, Cmd.none )

        Saved ->
            ( model
            , SavedWith model.title model.favorite model.cColor model.idx
                |> savedWithCmd
            )


autofocusDomId : String
autofocusDomId =
    "add-project-dialog-autofocus"


view :
    { a
        | saved : msg
        , canceled : msg
        , titleChanged : String -> msg
        , selectColorSys : SelectColor.System msg
        , favoriteChanged : Bool -> msg
    }
    -> AddProject
    -> Html msg
view ctx model =
    Dialog.UI.viewForm
        { submit = ctx.saved
        , cancel = ctx.canceled
        , title = "Add Project"
        , submitTitle = "Add"
        , content =
            [ Dialog.UI.input
                { labelText = "Project name"
                , value = model.title
                , changed = ctx.titleChanged
                , attrs = [ A.id autofocusDomId ]
                }
            , Dialog.UI.labeled "Project color"
                (ctx.selectColorSys.view model.cColor model.selectColor)
            , Dialog.UI.checkbox
                { labelText = "Add to favorites"
                , value = model.favorite
                , changed = ctx.favoriteChanged
                }
            ]
        }
