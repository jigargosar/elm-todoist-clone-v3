module Dialog exposing
    ( Config
    , Dialog
    , Msg
    , System
    , createConfig
    , subscriptions
    , system
    , update
    , view
    )

-- DIALOG

import Browser.Dom as Dom
import Dialog.AddProject as AddProject exposing (AddProject)
import Dialog.EditProject as EditProject exposing (EditProject)
import Focus exposing (FocusResult)
import Html.Styled as H exposing (Html)
import Log
import Project exposing (Project)
import Ret exposing (Ret, RetF)


type alias System msg =
    { initial : Dialog
    , subscriptions : Dialog -> Sub msg
    , openAddProject : Int -> msg
    , openEditProject : Project -> msg
    , updateF : Msg -> Ret Dialog msg -> Ret Dialog msg
    , update : Msg -> Dialog -> Ret Dialog msg
    , view : Dialog -> Html msg
    }


system :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> System msg
system ({ toMsg } as configParams) =
    let
        openMsg msg =
            toMsg << OpenMsg << msg

        config : Config msg
        config =
            createConfig configParams
    in
    { initial = Closed
    , subscriptions = subscriptions config
    , openAddProject = openMsg OpenAddProject
    , openEditProject = openMsg OpenEditProject
    , updateF = Ret.toUpdateF (update config)
    , update = update config
    , view = view config
    }


type alias Config msg =
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }


createConfig :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> Config msg
createConfig =
    identity


editProject =
    EditProject.system
        { toMsg = SubMsg << EditProjectMsg
        , canceled = Canceled
        , saved = SavedMsg << EditProjectSaved
        }


addProject =
    AddProject.system
        { toMsg = SubMsg << AddProjectMsg
        , canceled = Canceled
        , saved = SavedMsg << AddProjectSaved
        }


type Dialog
    = AddProject AddProject
    | EditProject EditProject
      {- | AddLabelDialog
         | EditLabelDialog LabelId
         | AddFilterDialog
         | EditFilterDialog FilterId
      -}
    | Closed


type SavedMsg
    = AddProjectSaved AddProject.SavedWith
    | EditProjectSaved EditProject.SavedWith


type SubMsg
    = AddProjectMsg AddProject.Msg
    | EditProjectMsg EditProject.Msg


type OpenMsg
    = OpenAddProject Int
    | OpenEditProject Project


type Msg
    = SubMsg SubMsg
    | OpenMsg OpenMsg
    | SavedMsg SavedMsg
    | Canceled
    | Focused FocusResult


subscriptions : Config msg -> Dialog -> Sub msg
subscriptions config dialog =
    let
        { toMsg } =
            config
    in
    (case dialog of
        AddProject model ->
            addProject.subscriptions model

        EditProject model ->
            editProject.subscriptions model

        Closed ->
            Sub.none
    )
        |> Sub.map toMsg


update : Config msg -> Msg -> Dialog -> Ret Dialog msg
update ({ toMsg } as config) message model =
    case message of
        SubMsg subMsg ->
            (case ( subMsg, model ) of
                ( AddProjectMsg msg, AddProject sub ) ->
                    addProject.update msg sub |> Ret.map AddProject

                ( EditProjectMsg msg, EditProject sub ) ->
                    editProject.update msg sub |> Ret.map EditProject

                _ ->
                    Ret.only model
            )
                |> Ret.mapCmd toMsg

        OpenMsg msg ->
            let
                focusCmd =
                    Focus.cmd (toMsg << Focused)
            in
            case msg of
                OpenAddProject idx ->
                    AddProject.initAt idx |> Tuple.mapBoth AddProject focusCmd

                OpenEditProject project ->
                    editProject.init project |> Tuple.mapBoth EditProject focusCmd

        Focused result ->
            case result of
                Err (Dom.NotFound domId) ->
                    ( model, Log.logError <| "dialog focus failed: " ++ domId )

                Ok () ->
                    ( model, Cmd.none )

        Canceled ->
            Ret.only Closed

        SavedMsg savedMsg ->
            ( Closed
            , Ret.toCmd
                (case savedMsg of
                    AddProjectSaved savedWith ->
                        config.projectAdded savedWith

                    EditProjectSaved savedWith ->
                        config.projectEdited savedWith
                )
            )


view : Config msg -> Dialog -> Html msg
view { toMsg } dialog =
    (case dialog of
        AddProject model ->
            addProject.view model

        EditProject model ->
            editProject.view model

        Closed ->
            H.text ""
    )
        |> H.map toMsg
