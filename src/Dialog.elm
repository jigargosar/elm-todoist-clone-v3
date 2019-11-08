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
import Optional
import Project exposing (Project)
import Ret exposing (RetCmd)


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
    { addProject : AddProject.System msg
    , editProject : EditProject.System msg
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
    in
    { addProject =
        AddProject.system
            { toMsg = c.toMsg << AddProjectMsg
            , canceled = canceled
            , saved = saved << AddProjectSaved
            }
    , editProject =
        EditProject.system
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
            config.addProject.subscriptions model

        EditProject model ->
            config.editProject.subscriptions model

        _ ->
            Sub.none


update : Config msg -> Msg -> Dialog -> ( Dialog, Cmd msg )
update config message model =
    let
        retT : ( Dialog, Cmd msg )
        retT =
            ( model, Cmd.none )

        ret : RetCmd Dialog msg
        ret =
            Ret.only model
    in
    case message of
        AddProjectMsg msg ->
            case model of
                AddProject sub ->
                    config.addProject.update msg sub
                        |> Tuple.mapFirst AddProject

                _ ->
                    retT

        EditProjectMsg msg ->
            ret
                |> Ret.updateOptional fields.editProject
                    (Ret.liftUpdate config.editProject.update)
                    msg
                |> Ret.batch

        OpenAddProject idx ->
            config.addProject.initAt idx
                |> Tuple.mapFirst AddProject

        OpenEditProject project ->
            config.editProject.init project
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
            [ config.addProject.view model ]

        EditProject model ->
            [ config.editProject.view model ]

        Closed ->
            []


fields =
    { addProject =
        Optional.fromTuple
            ( \b ->
                case b of
                    AddProject s ->
                        Just s

                    _ ->
                        Nothing
            , \s _ -> AddProject s
            )
    , editProject =
        Optional.fromTuple
            ( \b ->
                case b of
                    EditProject s ->
                        Just s

                    _ ->
                        Nothing
            , \s _ -> EditProject s
            )
    }
