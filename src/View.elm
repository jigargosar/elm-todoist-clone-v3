module View exposing (View, concat, content, portal, singleton)


type alias View a =
    { content : List a, portal : List a }


singleton : a -> View a
singleton el =
    View [ el ] []


content : List a -> View a
content c =
    View c []


portal : List a -> View a
portal p =
    View [] p


concat : List (View a) -> View a
concat =
    List.foldl
        (\cp acc -> { acc | content = acc.content ++ cp.content, portal = acc.portal ++ cp.portal })
        { content = [], portal = [] }
