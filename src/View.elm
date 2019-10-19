module View exposing (ContentPortal, content, singleton)


type alias ContentPortal a =
    { content : List a, portal : List a }


singleton : a -> ContentPortal a
singleton el =
    ContentPortal [ el ] []


content : List a -> ContentPortal a
content c =
    ContentPortal c []
