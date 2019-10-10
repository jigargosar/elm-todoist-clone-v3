module Layout exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


view :
    List (Html msg)
    -> List (Html msg)
    -> List (Html msg)
    -> Html msg
view top side main =
    div [ class "bg-body" ]
        [ header [ class "fixed top-0 bg-light-red white w-100 h-header" ]
            [ div
                ([ class "center w-100 max-w-app ph2" ]
                    ++ [ class "h-100", class "flex items-center" ]
                )
                top
            ]
        , div [ class "center w-100 max-w-app ", class "flex-grow-1" ]
            [ aside
                [ class "dn db-ns fixed top-sidebar bottom-0 w-sidebar hover-overflow-y  br-ns b--main"
                ]
                side
            , div
                [ class "ml0 ml-main-ns pt-main min-vh-100 flex-grow-1 flex"
                ]
                [ main_ [ class "flex-grow-1 bg-white br-ns b--main" ] main ]
            ]
        ]
