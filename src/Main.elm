module Main exposing (main)
{-| Conway's Game of Life -}

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Time

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

tickInterval = 1000

------------------------------------------------------------------------------
-- Model
------------------------------------------------------------------------------

{-| Zero-indexed coordinate reference for a two-dimensional grid -}
type alias Coord = (Int, Int)

{-| DOCS MISSING -}
type CellState = Live | Dead

{-| DOCS MISSING -}
type alias Model =
    { cols : Int
    , rows : Int
    , vals : Array (Array CellState)
    }

{-| DOCS MISSING -}
initModel : Model
initModel =
    Debug.todo "initModel()"  -- randomize

{-| DOCS MISSING -}
get : Model -> Coord -> Maybe CellState
get model (col, row) =
    case Array.get col model.vals of
        Just colVals ->
            Array.get row colVals
        _ ->
            Nothing

------------------------------------------------------------------------------
-- Controller
------------------------------------------------------------------------------

{-| DOCS MISSING -}
type Msg = Tick | Nil

{-| DOCS MISSING -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every tickInterval (\_ -> Tick)
        ]

{-| Wrap coordinates around the field -}
wrap : Model -> Coord -> Coord
wrap model (rawCol, rawRow) =
    let
        col =
            if rawCol == model.cols then
                0
            else if rawCol == -1 then
                model.cols - 1
            else
                rawCol
        row =
            if rawRow == model.cols then
                0
            else if rawRow == -1 then
                model.cols - 1
            else
                rawRow
    in
        (col, row)

{-| Get the neighbors of a cell -}
neighbors : Model -> Coord -> Array CellState
neighbors model (col, row) =
    let
        offsets =
            [ (-1, -1), (0, -1), (1, -1), (-1, 0)
            , (1, 0), (-1, 1), (0, 1), (1, 1)
            ]
        fxn (c, r) =
            case get model (wrap model (col + c, row + r)) of
                Just cellState -> cellState
                _ -> Dead
    in
        Array.map fxn (Array.fromList offsets)

{-| Determine the state of a cell for the next cycle -}
nextState : Model -> Coord -> Result String CellState
nextState model coord =
    let
        accumulator cellState sum =
            case cellState of
                Live -> sum + 1
                Dead -> sum
    in
        let
            liveNeighbors = Array.foldr accumulator 0 (neighbors model coord)
        in
            case (get model coord) of
                Just Live ->
                    if liveNeighbors < 2 || liveNeighbors > 3 then
                        Ok Dead
                    else
                        Ok Live
                Just Dead ->
                    if liveNeighbors == 3 then
                        Ok Live
                    else
                        Ok Dead
                _ ->
                    Err "Coordinate out of bounds"

{-| DOCS MISSING -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick ->
            Debug.todo "Tick message"
        _ ->
            (model, Cmd.none)

------------------------------------------------------------------------------
-- View
------------------------------------------------------------------------------

{-| DOCS MISSING -}
view : Model -> Html Msg
view model =
    Debug.todo "view()"

------------------------------------------------------------------------------
-- Entry Point
------------------------------------------------------------------------------

{-| DOCS MISSING -}
type alias Flags = ()

{-| DOCS MISSING -}
init : Flags -> (Model, Cmd Msg)
init () =
    (initModel, Cmd.none)

{-| Shenzhen Solitaire in Elm -}
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
