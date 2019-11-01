module Dialog exposing
    ( Config
    , Dialog
    , Msg
    , close
    , createConfig
    , initial
    , openAddProject
    , openEditProject
    , subscriptions
    , update
    , view
    )

-- DIALOG

import Dialog.AddProject as AddProject
import Dialog.EditProject as EditProject
import Html.Styled exposing (Html)
import Project exposing (Project)


type Dialog
    = AddProjectDialog AddProject.Model
    | EditProjectDialog EditProject.Model
      {- | AddLabelDialog
         | EditLabelDialog LabelId
         | AddFilterDialog
         | EditFilterDialog FilterId
      -}
    | Closed


type alias Config msg =
    { addProject : AddProject.Config msg
    , editProject : EditProject.Config msg
    }


createConfig :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> Config msg
createConfig c =
    let
        canceled =
            c.toMsg Close
    in
    { addProject =
        { toMsg = c.toMsg << AddProjectDialogMsg
        , canceled = canceled
        , saved = c.projectAdded
        }
    , editProject =
        { toMsg = c.toMsg << EditProjectDialogMsg
        , canceled = canceled
        , saved = c.projectEdited
        }
    }


type Msg
    = AddProjectDialogMsg AddProject.Msg
    | EditProjectDialogMsg EditProject.Msg
    | OpenAddProjectDialog Int
    | OpenEditProject Project
    | Close


openAddProject : Int -> Msg
openAddProject =
    OpenAddProjectDialog


openEditProject : Project -> Msg
openEditProject =
    OpenEditProject


initial =
    Closed


close : Msg
close =
    Close


subscriptions : Config msg -> Dialog -> Sub msg
subscriptions config dialog =
    case dialog of
        AddProjectDialog model ->
            AddProject.subscriptions config.addProject model

        EditProjectDialog model ->
            EditProject.subscriptions config.editProject model

        _ ->
            Sub.none


update : Config msg -> Msg -> Dialog -> ( Dialog, Cmd msg )
update config message dialogModel =
    let
        ret : ( Dialog, Cmd msg )
        ret =
            ( dialogModel, Cmd.none )
    in
    case message of
        AddProjectDialogMsg msg ->
            case dialogModel of
                AddProjectDialog model ->
                    AddProject.update config.addProject msg model
                        |> Tuple.mapFirst AddProjectDialog

                _ ->
                    ret

        EditProjectDialogMsg msg ->
            case dialogModel of
                EditProjectDialog model ->
                    EditProject.update config.editProject msg model
                        |> Tuple.mapFirst EditProjectDialog

                _ ->
                    ret

        OpenAddProjectDialog idx ->
            AddProject.initAt config.addProject idx
                |> Tuple.mapFirst AddProjectDialog

        OpenEditProject project ->
            EditProject.init config.editProject project
                |> Tuple.mapFirst EditProjectDialog

        Close ->
            ( Closed, Cmd.none )


view : Config msg -> Dialog -> List (Html msg)
view config dialog =
    case dialog of
        AddProjectDialog model ->
            [ AddProject.view config.addProject model ]

        EditProjectDialog model ->
            [ EditProject.view config.editProject model ]

        Closed ->
            []
