module Collections exposing (..)

import Basics.More exposing (apply)
import FilterCollection exposing (FilterCollection)
import Json.Decode as JD
import Json.Encode exposing (Value)
import LabelCollection exposing (LabelCollection)
import ProjectCollection exposing (ProjectCollection)
import Result.Extra
import Return
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


mapTodoDict func model =
    { model | todoDict = func model.todoDict }


mapProjectCollection func model =
    { model | projectCollection = func model.projectCollection }


mapLabelCollection func model =
    { model | labelCollection = func model.labelCollection }


mapFilterCollection func model =
    { model | filterCollection = func model.filterCollection }


initCollections : Flags x -> Collections a -> ( Collections a, List JD.Error )
initCollections flags model =
    let
        todoDictResult : Result JD.Error (Collections a -> Collections a)
        todoDictResult =
            TodoDict.fromEncodedList flags.todoList
                |> Result.map (always >> mapTodoDict)

        projectCollectionResult : Result JD.Error (Collections a -> Collections a)
        projectCollectionResult =
            ProjectCollection.fromEncodedList flags.projectList
                |> Result.map (always >> mapProjectCollection)

        labelCollectionResult : Result JD.Error (Collections a -> Collections a)
        labelCollectionResult =
            LabelCollection.fromEncodedList flags.labelList
                |> Result.map (always >> mapLabelCollection)

        filterCollectionResult : Result JD.Error (Collections a -> Collections a)
        filterCollectionResult =
            FilterCollection.fromEncodedList flags.filterList
                |> Result.map (always >> mapFilterCollection)

        results : List (Result JD.Error (Collections a -> Collections a))
        results =
            [ todoDictResult
            , projectCollectionResult
            , labelCollectionResult
            , filterCollectionResult
            ]

        foo =
            Result.Extra.unpack (Tuple.mapSecond (::))
                (Tuple.mapFirst identity)
    in
    results
        |> List.foldl
            (\result ->
                case result of
                    Ok func ->
                        Tuple.mapFirst func

                    Err error ->
                        Tuple.mapSecond ((::) error)
            )
            ( model, [] )
