module Px exposing (..)

import Css exposing (margin, margin2, padding, padding2, paddingLeft, paddingRight, paddingTop, px)


ma =
    margin << px


pa =
    padding << px


m2 tb lr =
    margin2 (px tb) (px lr)


p2 tb lr =
    padding2 (px tb) (px lr)


pl =
    paddingLeft << px


pt =
    paddingTop << px


pr =
    paddingRight << px
