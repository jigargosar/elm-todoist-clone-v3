module TodoView exposing (Config, viewList)

import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import LabelCollection exposing (LabelCollection)
import ProjectCollection exposing (ProjectCollection)
import Styles exposing (..)
import Todo exposing (Todo)
import TodoId exposing (TodoId)
import TodoProject exposing (TodoProject)
import TodoUI


type alias Config msg =
    { toggle : TodoId -> msg }


viewList : Config msg -> ProjectCollection -> LabelCollection -> List Todo -> Html msg
viewList config pc lc =
    listContainer << List.map (viewListItem config pc lc)


listContainer =
    ol [ class "list pl0 ma0" ]


viewListItem : Config msg -> ProjectCollection -> LabelCollection -> Todo -> Html msg
viewListItem config pc lc todo =
    li
        [ div [ css [ flex, itemsCenter ] ]
            [ viewIsCompleted config todo
            , viewTitle todo
            , viewProject pc todo
            ]
        , div [ css [ flex ] ] (viewLabels lc todo)
        ]


li : List (Html msg) -> Html msg
li =
    Html.li [ class "ph2 pv1 ba bl-0 bt-0 br-0 b--dotted b--black-30" ]


viewIsCompleted : { a | toggle : TodoId -> msg } -> Todo -> Html msg
viewIsCompleted config =
    TodoUI.viewCheck config.toggle


viewTitle : Todo -> Html msg
viewTitle todo =
    div [ class "pa2 flex-grow-1" ] [ text <| Todo.title todo ]


viewProject : ProjectCollection -> Todo -> Html msg
viewProject pc todo =
    TodoUI.viewProject <| TodoProject.fromTodo pc todo


viewLabels lc todo =
    List.filterMap (\lid -> LabelCollection.byId lid lc |> Maybe.map TodoUI.viewLabel) (Todo.labelIdList todo)
