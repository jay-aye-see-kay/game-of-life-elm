module Main exposing (..)

import Array
import Browser
import Grid exposing (CellState(..), Grid, Row)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import SvgGrid exposing (drawSvg)
import Time



-- 1. Any live cell with two or three neighbors survives.
-- 2. Any dead cell with three live neighbors becomes a live cell.
-- 3. All other live cells die in the next generation.
-- 4. All other dead cells stay dead.
--
---- MODEL ----


type alias Model =
    { grid : Grid
    , size : ( Int, Int )
    , interval : Int
    , running : Bool
    , stepCount : Int
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
      , running = False
      , stepCount = 0
      }
    , Random.generate NewRandomGrid (makeRandomGrid initialSize)
    )



---- UPDATE ----


type Msg
    = NewRandomGrid (List (List CellState))
    | Tick Time.Posix
    | ToggleRunning
    | UpdateInterval String
    | IncrementInterval Int
    | UpdateSize String
    | IncrementSize Int
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart ->
            ( model, Random.generate NewRandomGrid (makeRandomGrid model.size) )

        NewRandomGrid newGrid ->
            ( { model
                | grid = gridToArray newGrid
                , stepCount = 0
              }
            , Cmd.none
            )

        Tick _ ->
            if model.running then
                ( { model
                    | grid = Grid.makeNextGrid model.grid
                    , stepCount = model.stepCount + 1
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        UpdateInterval newIntervalString ->
            let
                newInterval =
                    Maybe.withDefault 0 (String.toInt newIntervalString)

                safeNewInterval =
                    if newInterval < 16 then
                        16

                    else
                        newInterval
            in
            ( { model | interval = safeNewInterval }, Cmd.none )

        IncrementInterval intervalChange ->
            let
                newInterval =
                    model.interval + intervalChange

                safeNewInterval =
                    if newInterval < 16 then
                        16

                    else
                        newInterval
            in
            ( { model | interval = safeNewInterval }, Cmd.none )

        UpdateSize newSizeString ->
            let
                newSize =
                    ( Maybe.withDefault 0 (String.toInt newSizeString)
                    , Maybe.withDefault 0 (String.toInt newSizeString)
                    )
            in
            ( { model | size = newSize }, Random.generate NewRandomGrid (makeRandomGrid newSize) )

        IncrementSize sizeChange ->
            let
                newSize =
                    Grid.incrementSize ( sizeChange, sizeChange ) model.grid
            in
            ( { model | size = newSize }, Random.generate NewRandomGrid (makeRandomGrid newSize) )



---- VIEW ----


intControlView : String -> Int -> (String -> Msg) -> List Int -> (Int -> Msg) -> Html Msg
intControlView controlLabel controlValue onInputMsg incrementers incrementerMsg =
    let
        negativeIncrementers =
            List.map
                (\inc -> button [ onClick (incrementerMsg inc) ] [ text (String.fromInt inc) ])
                (List.filter (\inc -> inc < 0) incrementers)

        positiveIncrementers =
            List.map
                (\inc -> button [ onClick (incrementerMsg inc) ] [ text ("+" ++ String.fromInt inc) ])
                (List.filter (\inc -> inc > 0) incrementers)

        labelHtml =
            [ label
                []
                [ text controlLabel
                , input
                    [ type_ "text"
                    , value (String.fromInt controlValue)
                    , onInput onInputMsg
                    ]
                    []
                ]
            ]
    in
    div [ class "controls-row" ]
        (negativeIncrementers ++ labelHtml ++ positiveIncrementers)


view : Model -> Html Msg
view model =
    let
        buttonText =
            if model.running then
                "Stop"

            else
                "Start"
    in
    div []
        [ div [ class "controls-container" ]
            [ intControlView "Size: " (Tuple.first model.size) UpdateSize [ -10, -1, 1, 10 ] IncrementSize
            , intControlView "Interval (ms): " model.interval UpdateInterval [ -100, -10, 10, 100 ] IncrementInterval
            , div [ class "controls-row" ]
                [ button [ onClick Restart ] [ text "Restart" ]
                , button [ onClick ToggleRunning ] [ text buttonText ]
                ]
            , text ("Step Count :" ++ String.fromInt model.stepCount)
            ]
        , drawSvg model.grid
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (toFloat model.interval) Tick



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
