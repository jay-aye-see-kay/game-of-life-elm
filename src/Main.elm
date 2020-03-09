module Main exposing (main)

import Browser
import Grid exposing (Coord, Grid)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import SvgGrid exposing (drawSvg)
import Time



---- MODEL ----


type alias Model =
    { grid : Grid
    , interval : Int
    , running : Bool
    , stepCount : Int
    }


makeRandomLivingList : Coord -> Random.Generator (List Coord)
makeRandomLivingList ( xMax, yMax ) =
    let
        count =
            -- 50% of area
            xMax * yMax // 2

        xGenerator =
            Random.int 0 xMax

        yGenerator =
            Random.int 0 yMax

        posGenerator =
            Random.pair xGenerator yGenerator
    in
    Random.list count posGenerator


initialSize : Coord
initialSize =
    ( 4, 4 )


init : ( Model, Cmd Msg )
init =
    ( { grid = Grid initialSize []
      , interval = 1000
      , running = False
      , stepCount = 0
      }
    , Random.generate NewRandomGrid (makeRandomLivingList initialSize)
    )



---- UPDATE ----


type Msg
    = NewRandomGrid (List Coord)
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
            ( model
            , Random.generate NewRandomGrid (makeRandomLivingList model.grid.size)
            )

        NewRandomGrid newLivingList ->
            ( { model
                | grid = Grid model.grid.size newLivingList
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
            ( { model | running = not model.running }
            , Cmd.none
            )

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

                newGrid =
                    Grid newSize model.grid.livingList
            in
            ( { model | grid = newGrid }
            , Random.generate NewRandomGrid (makeRandomLivingList newSize)
            )

        IncrementSize sizeChange ->
            let
                newGrid =
                    Grid.incrementSize model.grid ( sizeChange, sizeChange )
            in
            ( { model | grid = newGrid }
            , Random.generate NewRandomGrid (makeRandomLivingList newGrid.size)
            )



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
            [ intControlView "Size: " (Tuple.first model.grid.size) UpdateSize [ -10, -1, 1, 10 ] IncrementSize
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
