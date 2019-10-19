module Page.NotFound exposing (view)

import Html.Styled exposing (div, text)
import Url


view : Url.Url -> { content : List (Html.Styled.Html msg), portal : List a }
view url =
    { content = [ div [] [ text <| "NotFound: " ++ Url.toString url ] ], portal = [] }
