module Dialog exposing
    ( Dialog
    , Msg
    , System
    , system
    )

-- DIALOG

import Dialog.AddProject as AddProject exposing (AddProject)
import Dialog.EditProject as EditProject exposing (EditProject)
import Html.Styled exposing (Html)
import Optional
import Project exposing (Project)
import Ret exposing (Ret)


type alias System msg =
    { initial : Dialog
    , subscriptions : Dialog -> Sub msg
    , openAddProject : Int -> msg
    , openEditProject : Project -> msg
    , updateF : Msg -> Ret Dialog msg -> Ret Dialog msg
    , update : Msg -> Dialog -> Ret Dialog msg
    , view : Dialog -> List (Html msg)
    }


system :
    { toMsg : Msg -> msg
    , projectAdded : AddProject.SavedWith -> msg
    , projectEdited : EditProject.SavedWith -> msg
    }
    -> System msg
system c =
    let
        config =
            createConfig c
    in
    { initial = initial
    , subscriptions = subscriptions config
    , openAddProject = c.toMsg << OpenAddProject
    , openEditProject = c.toMsg << OpenEditProject
    , updateF = updateF config
    , update = Ret.fromUpdateF (updateF config)
    , view = view config
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
        subSys : (a -> SubMsg) -> (b -> SavedMsg) -> { toMsg : a -> msg, canceled : msg, saved : b -> msg }
        subSys subM saveM =
            { toMsg = c.toMsg << SubMsg << subM
            , canceled = c.toMsg Canceled
            , saved = c.toMsg << SavedMsg << saveM
            }
    in
    { addProject = AddProject.system <| subSys AddProjectMsg AddProjectSaved
    , editProject = EditProject.system <| subSys EditProjectMsg EditProjectSaved
    , projectAdded = c.projectAdded
    , projectEdited = c.projectEdited
    }


type SavedMsg
    = AddProjectSaved AddProject.SavedWith
    | EditProjectSaved EditProject.SavedWith


type SubMsg
    = AddProjectMsg AddProject.Msg
    | EditProjectMsg EditProject.Msg


type Msg
    = SubMsg SubMsg
    | OpenAddProject Int
    | OpenEditProject Project
    | SavedMsg SavedMsg
    | Canceled


initial : Dialog
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


updateF : Config msg -> Msg -> Ret Dialog msg -> Ret Dialog msg
updateF config message =
    case message of
        SubMsg subMsg ->
            updateSubF config subMsg

        OpenAddProject idx ->
            Ret.andThenAlways (config.addProject.initAt idx)
                >> Ret.map AddProject

        OpenEditProject project ->
            Ret.andThenAlways (config.editProject.init project)
                >> Ret.map EditProject

        Canceled ->
            Ret.always Closed

        SavedMsg savedMsg ->
            Ret.always Closed
                >> Ret.addMsg
                    (case savedMsg of
                        AddProjectSaved savedWith ->
                            config.projectAdded savedWith

                        EditProjectSaved savedWith ->
                            config.projectEdited savedWith
                    )


updateSubF : Config msg -> SubMsg -> Ret Dialog msg -> Ret Dialog msg
updateSubF config subMsg =
    case subMsg of
        AddProjectMsg msg ->
            Ret.updateOptionalF fields.addProject
                config.addProject.updateF
                msg

        EditProjectMsg msg ->
            Ret.updateOptionalF fields.editProject
                config.editProject.updateF
                msg


update : Config msg -> Msg -> Dialog -> Ret Dialog msg
update config message model =
    case message of
        SubMsg subMsg ->
            updateSub config subMsg model

        OpenAddProject idx ->
            config.addProject.initAt idx
                |> Ret.map AddProject

        OpenEditProject project ->
            config.editProject.init project
                |> Ret.map EditProject

        Canceled ->
            Ret.only Closed

        SavedMsg savedMsg ->
            updateSaved config savedMsg


updateSaved : Config msg -> SavedMsg -> Ret Dialog msg
updateSaved { projectAdded, projectEdited } savedMsg =
    Ret.only Closed
        |> Ret.addMsg
            (case savedMsg of
                AddProjectSaved savedWith ->
                    projectAdded savedWith

                EditProjectSaved savedWith ->
                    projectEdited savedWith
            )


updateSub : Config msg -> SubMsg -> Dialog -> Ret Dialog msg
updateSub { addProject, editProject } subMsg model =
    case ( subMsg, model ) of
        ( AddProjectMsg msg, AddProject sub ) ->
            addProject.update msg sub |> Ret.map AddProject

        ( EditProjectMsg msg, EditProject sub ) ->
            editProject.update msg sub |> Ret.map EditProject

        _ ->
            Ret.only model


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
