module DND exposing (..)

import Basics.More exposing (Position)
import Browser.Dom as Dom
import Project exposing (Project)


type Model
    = None
    | DraggingProject
    | DraggingLabel
    | DraggingFilter
