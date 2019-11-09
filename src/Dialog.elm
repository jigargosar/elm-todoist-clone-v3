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
        subSys : (a -> SubMsg) -> (b -> SavedMsg) -> { toMsg : a -> msg, canceled : msg, saved : b -> msg }
        subSys subM saveM =
            { toMsg = c.toMsg << SubMsg << subM
            , canceled = c.toMsg Canceled
            , saved = c.toMsg << SavedMsg << saveM
            }

        addProject =
            AddProject.system <| subSys AddProjectMsg AddProjectSaved

        editProject =
            EditProject.system <| subSys EditProjectMsg EditProjectSaved

        config : Config msg
        config =
            { addProject = addProject
            , editProject = editProject
            , projectAdded = c.projectAdded
            , projectEdited = c.projectEdited
            }

        subscriptions : Dialog -> Sub msg
        subscriptions dialog =
            case dialog of
                AddProject model ->
                    addProject.subscriptions model

                EditProject model ->
                    editProject.subscriptions model

                Closed ->
                    Sub.none

        update : Msg -> Dialog -> Ret Dialog msg
        update message model =
            let
                { projectAdded, projectEdited } =
                    config
            in
            case message of
                SubMsg subMsg ->
                    case ( subMsg, model ) of
                        ( AddProjectMsg msg, AddProject sub ) ->
                            addProject.update msg sub |> Ret.map AddProject

                        ( EditProjectMsg msg, EditProject sub ) ->
                            editProject.update msg sub |> Ret.map EditProject

                        _ ->
                            Ret.only model

                OpenMsg msg ->
                    case msg of
                        OpenAddProject idx ->
                            addProject.initAt idx |> Ret.map AddProject

                        OpenEditProject project ->
                            editProject.init project |> Ret.map EditProject

                Canceled ->
                    Ret.only Closed

                SavedMsg savedMsg ->
                    ( Closed
                    , Ret.toCmd
                        (case savedMsg of
                            AddProjectSaved savedWith ->
                                projectAdded savedWith

                            EditProjectSaved savedWith ->
                                projectEdited savedWith
                        )
                    )

        openMsg msg =
            c.toMsg << OpenMsg << msg
    in
    { initial = Closed
    , subscriptions = subscriptions
    , openAddProject = openMsg OpenAddProject
    , openEditProject = openMsg OpenEditProject
    , updateF = Ret.toUpdateF update
    , update = update
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
