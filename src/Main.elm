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
    ( 5, 5 )


init : ( Model, Cmd Msg )
init =
    let
        ( xSize, ySize ) =
            initialSize
    in
    ( { grid = Array.repeat xSize (Array.repeat ySize Dead)
      , size = initialSize
      , interval = 1000
      }
    , Random.generate NewRandomGrid (makeRandomGrid initialSize)
    )



---- UPDATE ----


makeNextGrid : Grid -> Grid
makeNextGrid oldGrid =
    oldGrid


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
