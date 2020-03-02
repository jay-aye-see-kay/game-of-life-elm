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


type alias Grid =
    List (List Bool)


type alias Model =
    { grid : Grid
    , newBool : Bool
    }


randomBool : Random.Generator Bool
randomBool =
    Random.weighted ( 50, True ) [ ( 50, False ) ]


intialGrid : Grid
intialGrid =
    [ [ True, True, False, False ]
    , [ False, True, True, False ]
    , [ True, True, False, True ]
    , [ True, True, True, True ]
    ]


init : ( Model, Cmd Msg )
init =
    ( { grid = intialGrid
      , newBool = False
      }
    , Random.generate NewBool randomBool
    )



---- UPDATE ----


type Msg
    = NoOp
    | NewBool Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewBool newBool ->
            ( { model | newBool = newBool }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        boolString =
            if model.newBool then
                "True"

            else
                "False"
    in
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , h1 [] [ text boolString ]
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
