module Basics.More exposing (..)

import Browser.Dom as Dom
import Html.Styled exposing (Attribute, Html, text)
import Html.Styled.Attributes exposing (class)
import Json.Decode as JD exposing (Decoder)
import List.Extra as List
import SelectList
import Task exposing (Task)


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


rotateListByElem : a -> a -> List a -> Maybe (List a)
rotateListByElem fromEl toEl list =
    Maybe.map2 (\from to -> rotateListByIndices from to list)
        (List.elemIndex fromEl list)
        (List.elemIndex toEl list)
        |> Maybe.andThen identity


msgToCmd : a -> Cmd a
msgToCmd =
    Task.succeed >> Task.perform identity


onDomErrorRecover : String -> (String -> msg) -> Task Dom.Error msg -> Task x msg
onDomErrorRecover logPrefix logMsg =
    Task.onError
        (\(Dom.NotFound id) ->
            logPrefix
                ++ ": Dom.NotFound \""
                ++ id
                ++ "\""
                |> logMsg
                |> Task.succeed
        )


eq_ =
    (==)


impl =
    Debug.todo "impl"


type alias Position =
    { x : Float, y : Float }


pageXYAsPositionDecoder : Decoder Position
pageXYAsPositionDecoder =
    JD.map2 Position
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


positionDiff : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Position
positionDiff p1 p2 =
    Position (p1.x - p2.x) (p1.y - p2.y)


positionAdd : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Position
positionAdd p1 p2 =
    Position (p1.x + p2.x) (p1.y + p2.y)


attrIf : Bool -> Attribute msg -> Attribute msg
attrIf bool attr =
    if bool then
        attr

    else
        class ""


viewIf : Bool -> (() -> Html msg) -> Html msg
viewIf bool view_ =
    if bool then
        view_ ()

    else
        text ""


apply =
    (|>)
