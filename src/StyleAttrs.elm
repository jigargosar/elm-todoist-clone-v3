module StyleAttrs exposing (StyleAttrs, concat, toAttrs)

import Css exposing (Style)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


type alias StyleAttrs msg =
    { styles : List Style, attrs : List (Attribute msg) }


concat : StyleAttrs msg -> StyleAttrs msg -> StyleAttrs msg
concat sa1 sa2 =
    StyleAttrs (sa1.styles ++ sa2.styles) (sa1.attrs ++ sa2.attrs)


toAttrs : StyleAttrs msg -> List (Attribute msg)
toAttrs { styles, attrs } =
    css styles :: attrs
