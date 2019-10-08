module Basics.More exposing (..)


flip : (c -> b -> a) -> b -> c -> a
flip func b a =
    func a b
