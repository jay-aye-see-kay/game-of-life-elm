module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Random



-- 1. Any live cell with two or three neighbors survives.
-- 2. Any dead cell with three live neighbors becomes a live cell.
-- 3. All other live cells die in the next generation.
-- 4. All other dead cells stay dead.
--
-- I'm trying to make random numbers work / understand them, I I'm trying to generate one bool and print it to screen (then I can move onto generating a whole grid of random numbers). I just can't print it to the display... if guess I just have to use a ternary becasue there's not String.fromBool
--
---- MODEL ----


type CellState
    = Alive
    | Dead


type alias Grid =
    List (List CellState)


type alias Model =
    { grid : Grid
    , size : ( Int, Int )
    }


makeRandomGrid : ( Int, Int ) -> Random.Generator Grid
makeRandomGrid ( xCount, yCount ) =
    Random.list yCount
        (Random.list xCount
            (Random.uniform Alive [ Dead ])
        )


initialGrid : Grid
initialGrid =
    [ [] ]


initialSize : ( Int, Int )
initialSize =
    ( 48, 48 )


init : ( Model, Cmd Msg )
init =
    ( { grid = initialGrid
      , size = initialSize
      }
    , Random.generate NewRandomGrid (makeRandomGrid initialSize)
    )



---- UPDATE ----


type Msg
    = NoOp
    | NewRandomGrid Grid


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandomGrid newGrid ->
            ( { model | grid = newGrid }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "GRID: " model.grid
    in
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
