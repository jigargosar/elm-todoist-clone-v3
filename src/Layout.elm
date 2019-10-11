module Layout exposing (Parts, view)

import Css
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class)
import Lens
import Styles exposing (..)


type Layout
    = Layout Private


type alias Private =
    { drawerModal : Bool }


type Msg
    = OpenModalDrawer


unwrap (Layout p) =
    p


map func =
    unwrap >> func >> Layout


drawerModalL : Lens.System Bool Layout
drawerModalL =
    Lens.system { get = unwrap >> .drawerModal, set = \s -> map (\p -> { p | drawerModal = s }) }


update message model =
    case message of
        OpenModalDrawer ->
            ()


type alias Parts msg =
    { appbar : List (Html msg)
    , drawer : List (Html msg)
    , content : List (Html msg)
    }



-- Custom Styles


bgBody : Style
bgBody =
    bgGrayN 0.98


fgWhite =
    fg white


b__main =
    b__ (grayN 0.95)


headerHeightPx =
    50


maxAppWidthPx =
    922


sidebarWidthPx =
    266


h_header =
    h_ headerHeightPx


w_sidebar =
    w_ sidebarWidthPx


max_w_app =
    max_w maxAppWidthPx


view : Parts msg -> Html msg
view { appbar, drawer, content } =
    styled div
        [ bgBody ]
        []
        [ styled div
            [ fgWhite
            , bgLightRed
            , batch [ fixed, top_0, w_100, h_header ]
            , flex
            , z_ 10
            ]
            []
            [ styled header
                [ batch [ center, w_100, max_w_app ]
                , batch [ ph 2, flex, itemsCenter ]
                ]
                []
                appbar
            ]
        , styled div
            [ center, w_100, max_w_app ]
            []
            [ styled aside
                [ batch [ left_ -sidebarWidthPx, ns [ left_auto ], transition [ Transitions.left 200 ] ]
                , batch [ fixed, top_0, bottom_0, pt_ headerHeightPx, w_sidebar ]
                , autoHideScrollY
                ]
                []
                --                [ styled div [ Css.height (Css.vh 200) ] [] side ] -- TEST OVERFLOW SCROLL
                drawer
            , styled div
                [ batch [ ml0, ns [ ml_ sidebarWidthPx ], transition [ Transitions.marginLeft 200 ] ]
                , pt_ headerHeightPx
                , min_vh 100
                , bgWhite
                , ns [ br_, bl, b__main ]
                , flex
                ]
                []
                [ styled main_ [ flexGrow1 ] [] content ]
            ]
        , styled div
            [ ns [ dn ], z_ 10, fixed ]
            []
            [ styled div
                [ batch [ fixed, absFill ]
                , bg (Css.hsla 0 0 0 0.3)
                ]
                []
                []
            , styled div
                [ batch [ fixed, top_0, bottom_0, w_sidebar ]
                , bgWhite
                ]
                [ class "shadow-1" ]
                drawer
            ]
        ]
