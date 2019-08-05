module Main exposing (main)
{-| Conway's Game of Life -}

import Array exposing (Array)
import Browser
import Html exposing (Html)

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

------------------------------------------------------------------------------
-- Controller
------------------------------------------------------------------------------

{-| DOCS MISSING -}
type Msg = Nil

{-| DOCS MISSING -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Debug.todo "subscriptions()"

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
    Debug.todo "neighbors()"

{-| Calculate the number of living neighbors to a cell -}
liveNeighbors : Model -> Coord -> Int
liveNeighbors model coord =
    let
        accumulator cellState sum =
            case cellState of
                Live -> sum + 1
                Dead -> sum
    in
        Array.foldr accumulator 0 (neighbors model coord)

{-| Determine the next state of a cell based upon its neighbors -}
nextState : CellState -> Array CellState -> CellState
nextState curState neighborList =
    Debug.todo "nextState()"

{-| DOCS MISSING -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    Debug.todo "update()"

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
