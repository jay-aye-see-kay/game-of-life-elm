module SvgGrid exposing (..)

import Array
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (CellState(..), Grid, Row)


makeRow : Row -> Int -> List (Svg msg)
makeRow row rowCount =
    let
        cellWidth : String
        cellWidth =
            String.fromFloat (100 / toFloat (Array.length row))

        cellXPosition : Int -> String
        cellXPosition i =
            String.fromFloat (toFloat i * 100 / toFloat (Array.length row))

        rowYPosition : String
        rowYPosition =
            String.fromFloat (toFloat rowCount * 100 / toFloat (Array.length row))

        cellFill : CellState -> String
        cellFill cell =
            if cell == Alive then
                "gray"

            else
                "white"

        mapFn : Int -> CellState -> Svg msg
        mapFn i cell =
            rect
                [ x (cellXPosition i)
                , y rowYPosition
                , width cellWidth
                , height cellWidth
                , fill (cellFill cell)
                ]
                []
    in
    Array.toList (Array.indexedMap mapFn row)


makeGrid : Grid -> List (Svg msg)
makeGrid grid =
    let
        mapFn : Int -> Row -> List (Svg msg)
        mapFn i row =
            makeRow row i
    in
    List.concat (Array.toList (Array.indexedMap mapFn grid))


drawSvg : Grid -> Svg msg
drawSvg grid =
    svg
        [ viewBox "0 0 100 100" ]
        (makeGrid grid)
