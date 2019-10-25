module Basics.More exposing (..)

import SelectList


flip : (c -> b -> a) -> b -> c -> a
flip func b a =
    func a b


memberAt : Int -> List a -> Maybe a
memberAt idx =
    List.drop idx >> List.head


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


rotateListByIndices : Int -> Int -> List a -> Maybe (List a)
rotateListByIndices from to list =
    list
        |> SelectList.fromList
        >> Maybe.andThen (SelectList.selectBy from)
        >> Maybe.map (SelectList.moveBy (to - from) >> SelectList.toList)
