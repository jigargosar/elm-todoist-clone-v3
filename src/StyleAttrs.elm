module StyleAttrs exposing (StyleAttrs, concat, none, styles, toAttrs, toAttrsWithBase)

import Css exposing (Style)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


type alias StyleAttrs msg =
    { styles : List Style, attrs : List (Attribute msg) }


concat : StyleAttrs msg -> StyleAttrs msg -> StyleAttrs msg
concat sa1 sa2 =
    StyleAttrs (sa1.styles ++ sa2.styles) (sa1.attrs ++ sa2.attrs)


toAttrs : StyleAttrs msg -> List (Attribute msg)
toAttrs model =
    css model.styles :: model.attrs


toAttrsWithBase : List Style -> List (Attribute msg) -> StyleAttrs msg -> List (Attribute msg)
toAttrsWithBase baseStyles baseAttrs model =
    concat (StyleAttrs baseStyles baseAttrs) model
        |> toAttrs


none : StyleAttrs msg
none =
    StyleAttrs [] []


styles : List Style -> StyleAttrs msg
styles styles_ =
    StyleAttrs styles_ []
