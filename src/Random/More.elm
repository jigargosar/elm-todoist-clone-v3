module Random.More exposing (..)

import Random exposing (Generator)


mostlyFalse : Generator Bool
mostlyFalse =
    let
        falseWeight =
            60

        trueWeight =
            100 - falseWeight
    in
    Random.weighted ( falseWeight, False ) [ ( trueWeight, True ) ]


fromList : List (Generator a) -> Generator (List a)
fromList =
    List.foldr (Random.map2 (::)) (Random.constant [])
