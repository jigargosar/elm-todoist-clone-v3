module Basics.More exposing (..)


flip : (c -> b -> a) -> b -> c -> a
flip func b a =
    func a b


memberAt : Int -> List a -> Maybe a
memberAt idx =
    List.drop idx >> List.head


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )
