module View exposing (View, concat, content, fromTuple, maybePortal, none, portal, singleton)


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


maybePortal : Maybe (List a) -> View a
maybePortal =
    Maybe.map portal >> Maybe.withDefault none


fromTuple : ( List a, List a ) -> View a
fromTuple ( c, p ) =
    View c p


concat : List (View a) -> View a
concat =
    List.foldl
        (\cp acc -> { acc | content = acc.content ++ cp.content, portal = acc.portal ++ cp.portal })
        { content = [], portal = [] }


none : View a
none =
    content []
