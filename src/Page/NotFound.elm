module Page.NotFound exposing (view)

import Html.Styled exposing (div, text)
import Url
import View


view : Url.Url -> View.View (Html.Styled.Html msg)
view url =
    View.singleton <| div [] [ text <| "NotFound: " ++ Url.toString url ]
