module Main exposing (main)
{-| Conway's Game of Life -}

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Random exposing (Generator)
import Time

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

{-| DOCS MISSING -}
fieldCols = 20

{-| DOCS MISSING -}
fieldRows = 10

{-| DOCS MISSING -}
liveP = 15

{-| DOCS MISSING -}
deadP = 15

{-| DOCS MISSING -}
tickInterval = 1000

------------------------------------------------------------------------------
-- Model
------------------------------------------------------------------------------

{-| Zero-indexed coordinate reference for a two-dimensional grid -}
type alias Coord = (Int, Int)

{-| DOCS MISSING -}
type CellState = Dead | Live

{-| DOCS MISSING -}
type alias CellData =
    { state : CellState
    , coord : Coord
    }

{-| DOCS MISSING -}
type alias CellGrid = Array (Array CellData)

{-| DOCS MISSING -}
type alias Model =
    { cols : Int
    , rows : Int
    , vals : CellGrid
    }

{-| DOCS MISSING -}
initModel : Model
initModel =
    let
        unindexed =
            Array.repeat fieldCols (Array.repeat fieldRows Dead)
        rowIndex cInd column =
            Array.indexedMap (cell cInd) column
        indexed =
            Array.indexedMap rowIndex unindexed
    in
        { cols = fieldCols
        , rows = fieldRows
        , vals = indexed
        }

{-| DOCS MISSING -}
cell : Int -> Int -> CellState -> CellData
cell col row state =
    { coord = (col, row)
    , state = state
    }

{-| DOCS MISSING -}
get : Model -> Coord -> Maybe CellData
get model (col, row) =
    case Array.get col model.vals of
        Just colVals ->
            Array.get row colVals
        _ ->
            Nothing

{-| DOCS MISSING -}
getState : Model -> Coord -> Maybe CellState
getState model coord =
    case get model coord of
        Just cellData -> Just cellData.state
        _ -> Nothing

{-| DOCS MISSING -}
isDead : Model -> Bool
isDead model =
    let
        accumulator val acc =
            case val.state of
                Dead -> True && acc
                Live -> False
        singleFold array =
            Array.foldr accumulator True array
    in
        Array.foldr (\array acc -> acc && (singleFold array)) True model.vals

{-| DOCS MISSING -}
seedGen : Generator (List CellState)
seedGen =
    let
        listLen = fieldCols * fieldRows
        rWeight = Random.weighted (liveP, Live) [ (deadP, Dead) ]
    in
        Random.list listLen rWeight

{-| DOCS MISSING -}
gridFromSeed : List CellState -> CellGrid
gridFromSeed ls =
    let
        helper n remList =
            if n > 0 then
                let
                    taken = Array.fromList (List.take fieldRows remList)
                    left = List.drop fieldRows remList
                in
                    taken::(helper (n - 1) left)
            else
                []
        unindexed =
            Array.fromList (helper fieldCols ls)
        rowIndex cInd column =
            Array.indexedMap (cell cInd) column
    in
        Array.indexedMap rowIndex unindexed

------------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------------

{-| DOCS MISSING -}
type Msg
    = NewSim (List CellState)
    | Tick
    | Nil

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
            case getState model (wrap model (col + c, row + r)) of
                Just cellState -> cellState
                _ -> Dead
    in
        Array.map fxn (Array.fromList offsets)

{-| Determine the next state of a cell -}
nextState : Model -> Coord -> Result String CellState
nextState model coord =
    let
        accumulator cellState sum =
            case cellState of
                Live -> sum + 1
                Dead -> sum
        liveNeighbors =
            Array.foldr accumulator 0 (neighbors model coord)
    in
        case (getState model coord) of
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
            if isDead model then
                let
                    seed = Random.generate NewSim seedGen
                in
                    (model, seed)
            else
                let
                    fxn cellData =
                        case nextState model cellData.coord of
                            Ok newState -> { cellData | state = newState }
                            _ -> cellData
                    newModel =
                        { model | vals = Array.map (Array.map fxn) model.vals }
                in
                    (newModel, Cmd.none)
        NewSim seed ->
            let
                newModel = { model | vals = gridFromSeed seed }
            in
                (newModel, Cmd.none)
        _ ->
            (model, Cmd.none)

------------------------------------------------------------------------------
-- Subscriptions
------------------------------------------------------------------------------

{-| DOCS MISSING -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every tickInterval (\_ -> Tick)
        ]

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
