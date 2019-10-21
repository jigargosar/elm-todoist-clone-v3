module PopupView exposing (..)

import Css
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes as A exposing (css)
import Html.Styled.Events as E
import Json.Decode as JD
import Popper exposing (Popper)
import Styles
import View exposing (View)


mockPopupView : { onClose : msg, noOp : msg } -> Popper -> View (Html msg)
mockPopupView config popper =
    View.portal
        [ div
            [ css
                [ Styles.fixed
                , Styles.absFill
                , Styles.flex
                , Styles.itemsCenter
                , Styles.justifyCenter
                , Styles.bg (Css.hsla 0 0 0 0.2)

                --                 , Styles.bg (Css.hsla 0 1 1 0.6)
                , Styles.z_ 10
                ]
            , E.onClick config.onClose
            ]
            [ div
                [ let
                    popperStyles =
                        Popper.styles popper
                  in
                  css
                    [ Styles.bgWhite
                    , Styles.pa 3
                    , Styles.bor 3
                    , Styles.batch popperStyles
                    ]
                , A.id "rootPopup"
                , E.stopPropagationOn "click" (JD.succeed ( config.noOp, True ))
                , A.class "shadow-1"
                ]
                [ div [ css [ Styles.pv 2 ] ] [ H.text "popup title" ]
                , div [ css [ Styles.pv 2 ] ] [ H.text "popup content" ]
                ]
            ]
        ]
