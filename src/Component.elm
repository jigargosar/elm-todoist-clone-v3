module Component exposing (..)

import Html.Styled exposing (Html)


type alias Component sub subMsg msg =
    { subscriptions : sub -> Sub msg
    , view : sub -> Html msg
    , update : subMsg -> sub -> ( sub, Cmd msg )
    }
