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
        addProject : AddProject.System msg
        addProject =
            AddProject.system
                { toMsg = c.toMsg << SubMsg << AddProjectMsg
                , canceled = c.toMsg Canceled
                , saved = c.toMsg << SavedMsg << AddProjectSaved
                }

        editProject : EditProject.System msg
        editProject =
            EditProject.system
                { toMsg = c.toMsg << SubMsg << EditProjectMsg
                , canceled = c.toMsg Canceled
                , saved = c.toMsg << SavedMsg << EditProjectSaved
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
                                c.projectAdded savedWith

                            EditProjectSaved savedWith ->
                                c.projectEdited savedWith
                        )
                    )

        view : Dialog -> List (Html msg)
        view dialog =
            case dialog of
                AddProject model ->
                    [ addProject.view model ]

                EditProject model ->
                    [ editProject.view model ]

                Closed ->
                    []

        openMsg msg =
            c.toMsg << OpenMsg << msg
    in
    { initial = Closed
    , subscriptions = subscriptions
    , openAddProject = openMsg OpenAddProject
    , openEditProject = openMsg OpenEditProject
    , updateF = Ret.toUpdateF update
    , update = update
    , view = view
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
