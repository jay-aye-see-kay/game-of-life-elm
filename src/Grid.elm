module Grid exposing (..)


type alias Coord =
    ( Int, Int )


type alias Grid =
    { size : Coord
    , livingList : List Coord
    }


isAlive : Coord -> Grid -> Bool
isAlive ( xPos, yPos ) grid =
    let
        cellTL =
            List.member ( xPos - 1, yPos - 1 ) grid.livingList

        cellTM =
            List.member ( xPos, yPos - 1 ) grid.livingList

        cellTR =
            List.member ( xPos + 1, yPos - 1 ) grid.livingList

        cellML =
            List.member ( xPos - 1, yPos ) grid.livingList

        ownState =
            List.member ( xPos, yPos ) grid.livingList

        cellMR =
            List.member ( xPos + 1, yPos ) grid.livingList

        cellBL =
            List.member ( xPos - 1, yPos + 1 ) grid.livingList

        cellBM =
            List.member ( xPos, yPos + 1 ) grid.livingList

        cellBR =
            List.member ( xPos + 1, yPos + 1 ) grid.livingList

        livingNeighborCount =
            List.foldl
                (\cell sum ->
                    if cell then
                        sum + 1

                    else
                        sum
                )
                0
                [ cellTL, cellTM, cellTR, cellML, cellMR, cellBL, cellBM, cellBR ]
    in
    if livingNeighborCount == 3 then
        True

    else if livingNeighborCount == 2 && ownState then
        True

    else
        False


makeNextLivingList : Coord -> Grid -> List Coord -> List Coord
makeNextLivingList position grid livingList =
    let
        ( xSize, ySize ) =
            grid.size

        ( xPos, yPos ) =
            position

        isPosAlive =
            isAlive position grid

        newLivingList =
            if isPosAlive then
                position :: livingList

            else
                livingList
    in
    if xPos > xSize && yPos > ySize then
        -- we're at the end return livingList
        livingList

    else if xPos > xSize then
        -- we're at y edge, reset x and increment y
        makeNextLivingList ( 0, yPos + 1 ) grid newLivingList

    else
        -- we're not at the edge, increment x and call
        makeNextLivingList ( xPos + 1, yPos ) grid newLivingList


makeNextGrid : Grid -> Grid
makeNextGrid grid =
    { grid | livingList = makeNextLivingList ( 0, 0 ) grid [] }


incrementSize : Grid -> Coord -> Grid
incrementSize grid newSize =
    { grid | size = newSize }
