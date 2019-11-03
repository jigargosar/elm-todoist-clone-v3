module UI.Icon exposing (Icon(..), view)

import Html.Styled exposing (Html, i, text)
import Html.Styled.Attributes exposing (class)


type Icon
    = Add
    | Label
    | Folder
    | FilterList
    | Expanded
    | Collapsed
    | CircleOutline
    | CheckCircleOutline
    | MoreHorizontal
    | Comment
    | PersonAdd
    | Edit


view : Icon -> Html msg
view icon =
    i [ class "material-icons" ] [ text <| iconName icon ]


iconName : Icon -> String
iconName icon =
    case icon of
        Add ->
            "add"

        Label ->
            "label"

        Folder ->
            "folder"

        FilterList ->
            "filter_list"

        Expanded ->
            "arrow_downward"

        Collapsed ->
            "chevron_right"

        CircleOutline ->
            "radio_button_unchecked"

        CheckCircleOutline ->
            "check_circle_outline"

        MoreHorizontal ->
            "more_horiz"

        Comment ->
            "comment"

        PersonAdd ->
            "person_add"

        Edit ->
            "edit"