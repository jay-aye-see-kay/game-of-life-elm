module GridBenchmarks exposing (main)

import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Grid exposing (..)


rowSample : Array.Array CellState
rowSample =
    Array.fromList [ Alive, Dead, Alive, Dead, Dead ]


makeGrid : Int -> Grid
makeGrid size =
    Array.repeat size (Array.initialize size (\i -> Maybe.withDefault Alive (Array.get (modBy 5 i) rowSample)))


suite : Benchmark
suite =
    describe "isAlive"
        [ benchmark "smallGrid-start" <|
            \_ -> isAlive 1 1 (makeGrid 10)
        , benchmark "smallGrid-middle" <|
            \_ -> isAlive 5 5 (makeGrid 10)
        , benchmark "largeGrid-start" <|
            \_ -> isAlive 1 1 (makeGrid 100)
        , benchmark "largeGrid-middle" <|
            \_ -> isAlive 50 50 (makeGrid 100)
        , benchmark "massiveGrid-start" <|
            \_ -> isAlive 1 1 (makeGrid 1000)
        , benchmark "massiveGrid-middle" <|
            \_ -> isAlive 500 500 (makeGrid 1000)
        ]


main : BenchmarkProgram
main =
    program suite
