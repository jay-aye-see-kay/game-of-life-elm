module SvgGrid exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)



-- FIXME CellState, Row, and Grid should probably be in their own file, but
-- need to be here to prevent a circular import (for now)


type CellState
    = Alive
    | Dead


type alias Row =
    List CellState


type alias Grid =
    List (List CellState)


makeRow : Row -> Svg msg
makeRow row =
    let
        getCellState : CellState -> String
        getCellState cell =
            case cell of
                Alive ->
                    "Alive"

                Dead ->
                    "Dead"

        mapFn : Int -> CellState -> Svg msg
        mapFn i cell =
            rect [] [ text (getCellState cell) ]
    in
    node "rect" [] (List.indexedMap mapFn row)


makeGrid : Grid -> Svg msg
makeGrid grid =
    let
        mapFn : Int -> Row -> Svg msg
        mapFn i row =
            makeRow row
    in
    node "rect" [] (List.indexedMap mapFn grid)


testSvg : Grid -> Svg msg
testSvg grid =
    let
        xCount =
            List.length grid

        yCount =
            List.length
                (Maybe.withDefault [] (List.head grid))

        blockWidth =
            toFloat xCount / 100

        blockHeight =
            toFloat yCount / 100

        _ =
            Debug.log "makeRow" (makeRow [ Alive, Dead ])
    in
    svg
        [ viewBox "0 0 100 100" ]
        [ rect
            []
            [ makeGrid grid ]
        ]
