module Types exposing (..)


type CellState
    = Alive
    | Dead


type alias Row =
    List CellState


type alias Grid =
    List (List CellState)
