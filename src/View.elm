module View exposing (View, content, singleton)


type alias View a =
    { content : List a, portal : List a }


singleton : a -> View a
singleton el =
    View [ el ] []


content : List a -> View a
content c =
    View c []
