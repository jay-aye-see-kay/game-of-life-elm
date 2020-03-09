module GridBenchmarks exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Grid



-- draws a spinner at 0,0 and 5,5


livingSample : Int -> List Grid.Coord
livingSample base =
    [ ( base + 0, base + 1 )
    , ( base + 1, base + 1 )
    , ( base + 2, base + 1 )
    , ( base + 5, base + 6 )
    , ( base + 6, base + 6 )
    , ( base + 7, base + 6 )
    ]



-- makes a "realistic" list by adding spinners along the diagonal


makeLivingList : Int -> List Grid.Coord -> List Grid.Coord
makeLivingList size livingList =
    if size <= 0 then
        livingList

    else
        livingSample (size - 10) ++ makeLivingList (size - 10) livingList



-- make living list with 50% fill


makeDenseLivingList : Grid.Coord -> Grid.Coord -> List Grid.Coord -> List Grid.Coord
makeDenseLivingList position size livingList =
    let
        ( xSize, ySize ) =
            size

        ( xPos, yPos ) =
            position

        isPosAlive =
            modBy 2 xPos + yPos == 0

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
        makeDenseLivingList ( 0, yPos + 1 ) size newLivingList

    else
        -- we're not at the edge, increment x and call
        makeDenseLivingList ( xPos + 1, yPos ) size newLivingList


makeGrid : Int -> Grid.Grid
makeGrid size =
    Grid.Grid ( size, size ) (makeDenseLivingList ( 0, 0 ) ( size, size ) [])


smallGrid : Grid.Grid
smallGrid =
    makeGrid 10


largeGrid : Grid.Grid
largeGrid =
    makeGrid 10


massiveGrid : Grid.Grid
massiveGrid =
    makeGrid 10


suite : Benchmark
suite =
    describe "isAlive"
        [ benchmark "smallGrid-middle" <|
            \_ -> Grid.isAlive ( 5, 5 ) smallGrid
        , benchmark "largeGrid-middle" <|
            \_ -> Grid.isAlive ( 50, 50 ) largeGrid
        , benchmark "massiveGrid-middle" <|
            \_ -> Grid.isAlive ( 500, 500 ) massiveGrid
        ]


main : BenchmarkProgram
main =
    program suite
