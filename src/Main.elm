module Main exposing (..)

import Array
import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Random
import SvgGrid exposing (drawSvg)
import Time
import Types exposing (CellState(..), Grid, Row)



-- 1. Any live cell with two or three neighbors survives.
-- 2. Any dead cell with three live neighbors becomes a live cell.
-- 3. All other live cells die in the next generation.
-- 4. All other dead cells stay dead.
--
---- MODEL ----


type alias Model =
    { grid : Grid
    , size : ( Int, Int )
    , interval : Float
    }


makeRandomGrid : ( Int, Int ) -> Random.Generator (List (List CellState))
makeRandomGrid ( xCount, yCount ) =
    Random.list yCount
        (Random.list xCount
            (Random.uniform Alive [ Dead ])
        )


gridToArray : List (List CellState) -> Grid
gridToArray list =
    Array.map (\row -> Array.fromList row) (Array.fromList list)


initialSize : ( Int, Int )
initialSize =
    ( 48, 48 )


init : ( Model, Cmd Msg )
init =
    let
        ( xSize, ySize ) =
            initialSize
    in
    ( { grid = Array.repeat xSize (Array.repeat ySize Dead)
      , size = initialSize
      , interval = 100
      }
    , Random.generate NewRandomGrid (makeRandomGrid initialSize)
    )



---- UPDATE ----


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
        gridHeight =
            Array.length prevGrid

        rowWidth =
            Array.length (Maybe.withDefault Array.empty (Array.get 0 prevGrid))

        makeNextRow : Int -> Row
        makeNextRow yPos =
            Array.initialize rowWidth (\xPos -> isAlive xPos yPos prevGrid)
    in
    Array.initialize gridHeight (\yPos -> makeNextRow yPos)


type Msg
    = NewRandomGrid (List (List CellState))
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandomGrid newGrid ->
            ( { model | grid = gridToArray newGrid }, Cmd.none )

        Tick _ ->
            ( { model | grid = makeNextGrid model.grid }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [] [ drawSvg model.grid ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.interval Tick



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
