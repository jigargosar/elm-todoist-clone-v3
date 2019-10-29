module Dialog.AddProject exposing (..)


type alias Model =
    { title : String
    , color : String
    }


initial : Model
initial =
    Model "" ""
