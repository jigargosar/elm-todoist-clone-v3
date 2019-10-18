module TodoProject exposing (..)

import Basics.More exposing (flip)
import Css
import Project
import ProjectCollection
import ProjectId exposing (ProjectId)


type alias TodoProject =
    { id : Maybe ProjectId
    , title : String
    , color : Css.Color
    }


fromProject project =
    TodoProject (Just (Project.id project)) (Project.title project) (Project.cssColor project)


fromMaybeProjectId projectCollection =
    Maybe.andThen (flip ProjectCollection.byId projectCollection)
        >> Maybe.map fromProject
        >> Maybe.withDefault inbox
