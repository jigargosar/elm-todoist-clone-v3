module Dialog.AddProject exposing (Config, Model, Msg, SavedWith, init, update, view)

import Basics.More exposing (msgToCmd)
import Css
import Dialog.UI
import Html.Styled as H exposing (Attribute, Html, div, form, span, text)
import Html.Styled.Attributes as A exposing (autofocus, css)
import Html.Styled.Events exposing (onSubmit)
import Key
import Px as PX
import Styles exposing (..)
import Theme


type alias Model =
    { title : String
    , color : String
    , favorite : Bool
    }


type alias SavedWith =
    { title : String, color : String, favorite : Bool }


initial : Model
initial =
    Model "" "" False


init : Config msg -> ( Model, Cmd msg )
init _ =
    ( initial, Cmd.none )


type Msg
    = Save
    | Cancel
    | Title String
    | Color String
    | Favorite Bool


type alias Config msg =
    { toMsg : Msg -> msg
    , saved : SavedWith -> msg
    , canceled : msg
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update { saved, canceled, toMsg } message model =
    case message of
        Save ->
            ( model
            , SavedWith model.title model.color False
                |> saved
                |> msgToCmd
            )

        Cancel ->
            ( model, msgToCmd canceled )

        Title title ->
            ( { model | title = title }, Cmd.none )

        Color color ->
            ( { model | color = color }, Cmd.none )

        Favorite favorite ->
            ( { model | favorite = favorite }, Cmd.none )


view : Config msg -> Model -> Html msg
view { toMsg } model =
    let
        formAttrs =
            [ css
                [ bgWhite
                , Styles.bor 3
                , w_ 300
                , max_w_pct 100
                ]
            , A.class "shadow-1"
            , Key.onKeyDown [ Key.escape Cancel ]
            , onSubmit Save
            ]

        formChildren =
            [ div
                [ css
                    [ Css.fontSize Css.larger
                    , pa 3
                    , bo_b
                    , boc <| Theme.borderGray
                    ]
                ]
                [ text "Add Project" ]
            , div [ css [ ph 3 ] ]
                [ Dialog.UI.input
                    { labelText = "Project name"
                    , value = model.title
                    , changed = Title
                    , attrs = [ A.id "add-project-dialog-autofocus", autofocus True ]
                    }
                , Dialog.UI.input
                    { labelText = "Project color"
                    , value = model.color
                    , changed = Color
                    , attrs = []
                    }
                , Dialog.UI.checkbox
                    { labelText = "Add to favorites"
                    , value = model.favorite
                    , changed = Favorite
                    }
                ]
            , Dialog.UI.actions { submitTitle = "add", submitted = Save, canceled = Cancel }
            ]
    in
    Dialog.UI.container { submitted = Save, canceled = Cancel, title = "Add Project" } formChildren
        |> H.map toMsg


overlayStyles =
    batch
        [ fixed
        , absFill
        , flex
        , itemsCenter
        , justifyCenter
        , bg (Css.hsla 0 0 0 0.2)

        --                 , bg (Css.hsla 0 1 1 0.6)
        , z_ 10
        ]
