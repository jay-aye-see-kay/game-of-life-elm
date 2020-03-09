module Grid exposing (CellState(..), Grid, Row, incrementSize, makeNextGrid)

import Array


type CellState
    = Alive
    | Dead


type alias Row =
    Array.Array CellState


type alias Grid =
    Array.Array (Array.Array CellState)


type alias Size =
    ( Int, Int )


isAlive : Int -> Int -> Grid -> CellState
isAlive xPos yPos grid =
    let
        rowAbove =
            Maybe.withDefault Array.empty (Array.get (yPos - 1) grid)

        row =
            Maybe.withDefault Array.empty (Array.get yPos grid)

        rowBelow =
            Maybe.withDefault Array.empty (Array.get (yPos + 1) grid)

        cellTL =
            Maybe.withDefault Dead (Array.get (xPos - 1) rowAbove)

        cellTM =
            Maybe.withDefault Dead (Array.get xPos rowAbove)

        cellTR =
            Maybe.withDefault Dead (Array.get (xPos + 1) rowAbove)

        cellML =
            Maybe.withDefault Dead (Array.get (xPos - 1) row)

        ownState =
            Maybe.withDefault Dead (Array.get xPos row)

        cellMR =
            Maybe.withDefault Dead (Array.get (xPos + 1) row)

        cellBL =
            Maybe.withDefault Dead (Array.get (xPos - 1) rowBelow)

        cellBM =
            Maybe.withDefault Dead (Array.get xPos rowBelow)

        cellBR =
            Maybe.withDefault Dead (Array.get (xPos + 1) rowBelow)

        livingNeighborCount =
            List.foldl
                (\cell sum ->
                    if cell == Alive then
                        sum + 1

                    else
                        sum
                )
                0
                [ cellTL, cellTM, cellTR, cellML, cellMR, cellBL, cellBM, cellBR ]
    in
    if livingNeighborCount == 3 then
        Alive

    else if livingNeighborCount == 2 && ownState == Alive then
        Alive

    else
        Dead


makeNextGrid : Grid -> Grid
makeNextGrid prevGrid =
    let
        ( xSize, ySize ) =
            getSize prevGrid

        makeNextRow : Int -> Row
        makeNextRow yPos =
            Array.initialize xSize (\xPos -> isAlive xPos yPos prevGrid)
    in
    Array.initialize ySize (\yPos -> makeNextRow yPos)


getSize : Grid -> Size
getSize grid =
    ( Array.length grid
    , Array.length (Maybe.withDefault Array.empty (Array.get 0 grid))
    )


incrementSize : Size -> Grid -> Size
incrementSize ( xChange, yChange ) grid =
    let
        ( xSize, ySize ) =
            getSize grid
    in
    ( xSize + xChange, ySize + yChange )
