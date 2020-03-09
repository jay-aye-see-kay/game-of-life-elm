module Grid exposing (..)

import Array


type CellState
    = Alive
    | Dead


type alias Row =
    Array.Array CellState


type alias Grid =
    Array.Array (Array.Array CellState)
