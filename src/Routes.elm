module Routes exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attr
import Json.Decode as JD
import ProjectId exposing (ProjectId)
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, int, map, parse, s, string, top)


type Route
    = Root
    | Inbox
    | ProjectTodoList ProjectId


parser : Parser (Route -> c) c
parser =
    Url.Parser.oneOf
        [ map Root top
        , map Inbox (s "inbox")
        , map ProjectTodoList (s "project" </> parseProjectId)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    parse parser url


parseProjectId : Parser (ProjectId -> b) b
parseProjectId =
    Url.Parser.custom "PROJECT_ID" <|
        (JD.decodeString ProjectId.decoder
            >> Result.toMaybe
        )


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Root ->
                    []

                Inbox ->
                    [ "inbox" ]

                ProjectTodoList projectId ->
                    [ "project", ProjectId.toString projectId ]
    in
    "#/" ++ String.join "/" pieces
