module Collections exposing (..)

import FilterCollection exposing (FilterCollection)
import Json.Encode exposing (Value)
import LabelCollection exposing (LabelCollection)
import ProjectCollection exposing (ProjectCollection)
import TodoDict exposing (TodoDict)


type alias Collections a =
    { a
        | todoDict : TodoDict
        , projectCollection : ProjectCollection
        , labelCollection : LabelCollection
        , filterCollection : FilterCollection
    }


type alias Flags a =
    { a
        | todoList : Value
        , projectList : Value
        , labelList : Value
        , filterList : Value
    }
