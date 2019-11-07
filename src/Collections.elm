module Collections exposing (Collections, Flags, initCollections)

import FilterCollection exposing (FilterCollection)
import Json.Decode as JD
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


type alias Lens s b =
    { get : b -> s, set : s -> b -> b }


createLens : ( b -> s, s -> b -> b ) -> Lens s b
createLens ( get, set ) =
    { get = get, set = set }


todoDictL =
    createLens ( .todoDict, \s b -> { b | todoDict = s } )


projectCollectionL =
    { get = .projectCollection, set = \s b -> { b | projectCollection = s } }


labelCollectionL =
    createLens ( .labelCollection, \s b -> { b | labelCollection = s } )


filterCollectionL =
    { get = .filterCollection, set = \s b -> { b | filterCollection = s } }


initCollections : Flags x -> Collections a -> ( Collections a, List JD.Error )
initCollections flags model =
    let
        results : List (Result JD.Error (Collections a -> Collections a))
        results =
            [ TodoDict.fromEncodedList flags.todoList
                |> Result.map todoDictL.set
            , ProjectCollection.fromEncodedList flags.projectList
                |> Result.map projectCollectionL.set
            , LabelCollection.fromEncodedList flags.labelList
                |> Result.map labelCollectionL.set
            , FilterCollection.fromEncodedList flags.filterList
                |> Result.map filterCollectionL.set
            ]
    in
    results
        |> List.foldl
            (\result ( newModel, errorList ) ->
                case result of
                    Ok func ->
                        ( func newModel, errorList )

                    Err error ->
                        ( newModel, error :: errorList )
            )
            ( model, [] )
