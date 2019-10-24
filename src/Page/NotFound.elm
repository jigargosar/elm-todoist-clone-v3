module Page.NotFound exposing (view)

import Html.Styled exposing (Html, div, text)
import Url


view : Url.Url -> List (Html msg)
view url =
    [ div [] [ text <| "NotFound: " ++ Url.toString url ] ]
