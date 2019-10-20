module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import FilterId exposing (FilterId)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attr
import Json.Decode as JD
import Json.Encode as JE
import LabelId exposing (LabelId)
import ProjectId exposing (ProjectId)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, int, map, parse, s, string, top)


type Route
    = Root
    | Inbox
    | Project ProjectId
    | Label LabelId
    | Filter FilterId


parser : Parser (Route -> c) c
parser =
    Url.Parser.oneOf
        [ map Root top
        , map Inbox (s "inbox")
        , map Project (s "project" </> parseProjectId)
        , map Label (s "label" </> parseLabelId)
        , map Filter (s "filter" </> parseFilterId)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    parse parser url


parseProjectId : Parser (ProjectId -> b) b
parseProjectId =
    Url.Parser.custom "PROJECT_ID" <|
        (JE.string
            >> JD.decodeValue ProjectId.decoder
            >> Result.toMaybe
        )


parseLabelId : Parser (LabelId -> b) b
parseLabelId =
    Url.Parser.custom "LABEL_ID" <|
        (JE.string
            >> JD.decodeValue LabelId.decoder
            >> Result.toMaybe
        )


parseFilterId : Parser (FilterId -> b) b
parseFilterId =
    Url.Parser.custom "FILTER_ID" <|
        (JE.string
            >> JD.decodeValue FilterId.decoder
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
        pathSegments =
            case route of
                Root ->
                    []

                Inbox ->
                    [ "inbox" ]

                Project projectId ->
                    [ "project", ProjectId.toString projectId ]

                Label labelId ->
                    [ "label", LabelId.toString labelId ]

                Filter filterId ->
                    [ "filter", FilterId.toString filterId ]
    in
    Url.Builder.absolute pathSegments []
