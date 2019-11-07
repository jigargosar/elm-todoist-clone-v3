module DB exposing (DB, Flags, init, mapPC)

import FilterCollection exposing (FilterCollection)
import Json.Decode as JD
import Json.Encode exposing (Value)
import LabelCollection exposing (LabelCollection)
import ProjectCollection exposing (ProjectCollection)
import TodoCollection exposing (TodoCollection)


type alias DB a =
    { a
        | todoCollection : TodoCollection
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


tc =
    createLens ( .todoCollection, \s b -> { b | todoCollection = s } )


pc : Lens a { b | projectCollection : a }
pc =
    createLens ( .projectCollection, \s b -> { b | projectCollection = s } )


lc =
    createLens ( .labelCollection, \s b -> { b | labelCollection = s } )


fc =
    createLens ( .filterCollection, \s b -> { b | filterCollection = s } )


mapPC : (a -> a) -> { b | projectCollection : a } -> { b | projectCollection : a }
mapPC =
    over pc


init : Flags x -> DB a -> ( DB a, List JD.Error )
init flags model =
    let
        results : List (Result JD.Error (DB a -> DB a))
        results =
            [ TodoCollection.fromEncodedList flags.todoList
                |> Result.map tc.set
            , ProjectCollection.fromEncodedList flags.projectList
                |> Result.map pc.set
            , LabelCollection.fromEncodedList flags.labelList
                |> Result.map lc.set
            , FilterCollection.fromEncodedList flags.filterList
                |> Result.map fc.set
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


type alias Lens s b =
    { get : b -> s, set : s -> b -> b }


createLens : ( b -> s, s -> b -> b ) -> Lens s b
createLens ( get, set ) =
    { get = get, set = set }


over : Lens s b -> (s -> s) -> b -> b
over lens func val =
    lens.set (func <| lens.get val) val
