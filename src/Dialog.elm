module Dialog exposing
    ( Config
    , Dialog
    , Msg
    , createConfig
    , initial
    , openAddProject
    , openEditProject
    , subscriptions
    , update
    , view
    )

-- DIALOG

import Basics.More exposing (msgToCmd)
import Dialog.AddProject as AddProject exposing (AddProject)
import Dialog.EditProject as EditProject exposing (EditProject)
import Html.Styled exposing (Html)
import Project exposing (Project)


type Dialog
    = AddProject AddProject
    | EditProject EditProject
      {- | AddLabelDialog
         | EditLabelDialog LabelId
         | AddFilterDialog
         | EditFilterDialog FilterId
      -}
    | Closed


type alias Config msg =
    { addProject : AddProject.Config msg
    , addProjectSystem : { update : AddProject.Msg -> AddProject.AddProject -> ( AddProject.AddProject, Cmd msg ) }
    , editProject : EditProject.Config msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
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
            c.toMsg Canceled

        saved =
            c.toMsg << SavedMsg

        addProjectConfig =
            { toMsg = c.toMsg << AddProjectMsg
            , canceled = canceled
            , saved = saved << AddProjectSaved
            }
    in
    { addProject = addProjectConfig
    , addProjectSystem =
        { update = AddProject.update addProjectConfig
        }
    , editProject =
        { toMsg = c.toMsg << EditProjectMsg
        , canceled = canceled
        , saved = saved << EditProjectSaved
        }
    , projectAdded = c.projectAdded
    , projectEdited = c.projectEdited
    }


type SavedMsg
    = AddProjectSaved AddProject.SavedWith
    | EditProjectSaved EditProject.SavedWith


type Msg
    = AddProjectMsg AddProject.Msg
    | EditProjectMsg EditProject.Msg
    | OpenAddProject Int
    | OpenEditProject Project
    | SavedMsg SavedMsg
    | Canceled


openAddProject : Int -> Msg
openAddProject =
    OpenAddProject


openEditProject : Project -> Msg
openEditProject =
    OpenEditProject


initial =
    Closed


subscriptions : Config msg -> Dialog -> Sub msg
subscriptions config dialog =
    case dialog of
        AddProject model ->
            AddProject.subscriptions config.addProject model

        EditProject model ->
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
        AddProjectMsg msg ->
            case dialogModel of
                AddProject model ->
                    AddProject.update config.addProject msg model
                        |> Tuple.mapFirst AddProject

                _ ->
                    ret

        EditProjectMsg msg ->
            case dialogModel of
                EditProject model ->
                    EditProject.update config.editProject msg model
                        |> Tuple.mapFirst EditProject

                _ ->
                    ret

        OpenAddProject idx ->
            AddProject.initAt config.addProject idx
                |> Tuple.mapFirst AddProject

        OpenEditProject project ->
            EditProject.init config.editProject project
                |> Tuple.mapFirst EditProject

        Canceled ->
            ( Closed, Cmd.none )

        SavedMsg savedMsg ->
            case savedMsg of
                AddProjectSaved savedWith ->
                    ( Closed, config.projectAdded savedWith |> msgToCmd )

                EditProjectSaved savedWith ->
                    ( Closed, config.projectEdited savedWith |> msgToCmd )


view : Config msg -> Dialog -> List (Html msg)
view config dialog =
    case dialog of
        AddProject model ->
            [ AddProject.view config.addProject model ]

        EditProject model ->
            [ EditProject.view config.editProject model ]

        Closed ->
            []
