module SvgGrid exposing (..)

import Grid exposing (Coord, Grid)
import Svg exposing (..)
import Svg.Attributes exposing (..)


makeSvgGrid : Coord -> Grid -> List (Svg msg) -> List (Svg msg)
makeSvgGrid position grid svgElements =
    let
        ( xSize, ySize ) =
            grid.size

        ( xPos, yPos ) =
            position

        cellWidth : String
        cellWidth =
            String.fromFloat (100 / toFloat xSize)

        cellXPosition : String
        cellXPosition =
            String.fromFloat (toFloat xPos * 100 / toFloat xSize)

        cellYPosition : String
        cellYPosition =
            String.fromFloat (toFloat yPos * 100 / toFloat ySize)

        cellFill : String
        cellFill =
            if Grid.isAlive position grid then
                "gray"

            else
                "white"

        newElement =
            rect
                [ x cellXPosition
                , y cellYPosition
                , width cellWidth
                , height cellWidth
                , fill cellFill
                ]
                []

        newSvgElements =
            newElement :: svgElements
    in
    if xPos > xSize && yPos > ySize then
        -- we're at the end return livingList
        svgElements

    else if xPos > xSize then
        -- we're at y edge, reset x and increment y
        makeSvgGrid ( 0, yPos + 1 ) grid newSvgElements

    else
        -- we're not at the edge, increment x and call
        makeSvgGrid ( xPos + 1, yPos ) grid newSvgElements


drawSvg : Grid -> Svg msg
drawSvg grid =
    svg
        [ viewBox "0 0 100 100" ]
        (makeSvgGrid ( 0, 0 ) grid [])
