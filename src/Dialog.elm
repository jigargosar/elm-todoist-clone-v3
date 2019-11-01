module Dialog exposing
    ( Config
    , Dialog
    , DialogMsg
    , createConfig
    , dialogSubscriptions
    , initAddProjectDialogAt
    , initEditProjectDialog
    , none
    , update
    , viewDialog
    )

-- DIALOG

import Dialog.AddProject
import Dialog.EditProject
import FilterId exposing (FilterId)
import Html.Styled exposing (Html)
import LabelId exposing (LabelId)
import Project exposing (Project)


type Dialog
    = AddProjectDialog Dialog.AddProject.Model
    | EditProjectDialog Dialog.EditProject.Model
    | AddLabelDialog
    | EditLabelDialog LabelId
    | AddFilterDialog
    | EditFilterDialog FilterId
    | NoDialog


type alias Config msg =
    { addProject : Dialog.AddProject.Config msg
    , editProject : Dialog.EditProject.Config msg
    }


createConfig :
    { toMsg : DialogMsg -> msg
    , canceled : msg
    , projectAdded : Dialog.AddProject.SavedWith -> msg
    , projectEdited : Dialog.EditProject.SavedWith -> msg
    }
    -> Config msg
createConfig c =
    { addProject =
        { toMsg = c.toMsg << AddProjectDialogMsg
        , canceled = c.canceled
        , saved = c.projectAdded
        }
    , editProject =
        { toMsg = c.toMsg << EditProjectDialogMsg
        , canceled = c.canceled
        , saved = c.projectEdited
        }
    }


type DialogMsg
    = AddProjectDialogMsg Dialog.AddProject.Msg
    | EditProjectDialogMsg Dialog.EditProject.Msg


initAddProjectDialogAt : Config msg -> Int -> ( Dialog, Cmd msg )
initAddProjectDialogAt config idx =
    Dialog.AddProject.initAt config.addProject idx
        |> Tuple.mapFirst AddProjectDialog


initEditProjectDialog : Config msg -> Project -> ( Dialog, Cmd msg )
initEditProjectDialog config project =
    Dialog.EditProject.init config.editProject project
        |> Tuple.mapFirst EditProjectDialog


none =
    NoDialog


dialogSubscriptions : Config msg -> Dialog -> Sub msg
dialogSubscriptions config dialog =
    case dialog of
        AddProjectDialog model ->
            Dialog.AddProject.subscriptions config.addProject model

        EditProjectDialog model ->
            Dialog.EditProject.subscriptions config.editProject model

        _ ->
            Sub.none


update : Config msg -> DialogMsg -> Dialog -> ( Dialog, Cmd msg )
update config message dialogModel =
    let
        ret : ( Dialog, Cmd msg )
        ret =
            ( dialogModel, Cmd.none )
    in
    case ( dialogModel, message ) of
        ( AddProjectDialog model, AddProjectDialogMsg msg ) ->
            Dialog.AddProject.update config.addProject msg model
                |> Tuple.mapFirst AddProjectDialog

        _ ->
            ret


viewDialog : Config msg -> Dialog -> List (Html msg)
viewDialog config dialog =
    case dialog of
        AddProjectDialog model ->
            [ Dialog.AddProject.view config.addProject model ]

        EditProjectDialog model ->
            [ Dialog.EditProject.view config.editProject model ]

        _ ->
            []
