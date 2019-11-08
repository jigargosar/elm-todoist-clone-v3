module DB exposing (DB, Flags, init, mapFC, mapLC, mapPC, mapTC)

import FilterCollection as FC exposing (FilterCollection)
import Json.Decode as JD
import Json.Encode exposing (Value)
import LabelCollection as LC exposing (LabelCollection)
import ProjectCollection as PC exposing (ProjectCollection)
import TodoCollection as TC exposing (TodoCollection)
import Tuple2


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


mapTC : (a -> a) -> { b | todoCollection : a } -> { b | todoCollection : a }
mapTC =
    over tc


mapPC : (a -> a) -> { b | projectCollection : a } -> { b | projectCollection : a }
mapPC =
    over pc


mapLC : (a -> a) -> { b | labelCollection : a } -> { b | labelCollection : a }
mapLC =
    over lc


mapFC : (a -> a) -> { b | filterCollection : a } -> { b | filterCollection : a }
mapFC =
    over fc


init : Flags x -> DB a -> ( DB a, List JD.Error )
init flags =
    Tuple2.pairTo []
        >> handleDecodeResult tc (TC.fromEncodedList flags.todoList)
        >> handleDecodeResult pc (PC.fromEncodedList flags.projectList)
        >> handleDecodeResult lc (LC.fromEncodedList flags.labelList)
        >> handleDecodeResult fc (FC.fromEncodedList flags.filterList)


handleDecodeResult :
    Lens s big
    -> Result JD.Error s
    -> ( big, List JD.Error )
    -> ( big, List JD.Error )
handleDecodeResult lens result ( big, errors ) =
    case result of
        Ok small ->
            ( lens.set small big, errors )

        Err error ->
            ( big, error :: errors )


type alias Lens s b =
    { get : b -> s, set : s -> b -> b }


createLens : ( b -> s, s -> b -> b ) -> Lens s b
createLens ( get, set ) =
    { get = get, set = set }


over : Lens s b -> (s -> s) -> b -> b
over lens func val =
    lens.set (func <| lens.get val) val
