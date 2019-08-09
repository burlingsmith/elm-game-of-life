module Main exposing (main)
{-| Conway's Game of Life -}

import Array exposing (Array)
import Browser
import Browser.Events
import Css
import Css.Global
import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Styled
import Html.Styled.Attributes as Style
import Json.Decode as Decode exposing (Decoder)
import Random exposing (Generator)
import Time

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

{-| Number of columns in the simulation grid -}
fieldCols = 30

{-| Number of rows in the simulation grid -}
fieldRows = 30

{-| Side length of each cell in the simulation grid -}
sideLen = "15px"

{-| Probability for live cells in randomly-generated grids -}
liveP = 20

{-| Probability for dead cells in randomly-generated grids -}
deadP = 80

{-| Number of milliseconds between simulation grid updates -}
tickInterval = 125

------------------------------------------------------------------------------
-- Convenience Aliases
------------------------------------------------------------------------------

{-| Zero-indexed coordinate reference for a two-dimensional grid -}
type alias Coord = (Int, Int)

------------------------------------------------------------------------------
-- Model
------------------------------------------------------------------------------

{-| Cell state -}
type CellState = Dead | Live

{-| Cell state and position -}
type alias CellData =
    { state : CellState
    , coord : Coord
    }

{-| Finite, two-dimensional grid of cells -}
type alias CellGrid = Array (Array CellData)

{-| Simulation grid and metadata -}
type alias Model =
    { cols : Int
    , rows : Int
    , vals : CellGrid
    }

---- Instantiation -------------------

{-| Initial model -}
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

{-| CellData constructor -}
cell : Int -> Int -> CellState -> CellData
cell col row state =
    { coord = (col, row)
    , state = state
    }

{-| Intermediate step between randomly generated cells and a CellGrid -}
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

---- Random Generation ---------------

{-| Random generator for a new simulation grid's CellState values -}
seedGen : Generator (List CellState)
seedGen =
    let
        listLen = fieldCols * fieldRows
        rWeight = Random.weighted (liveP, Live) [ (deadP, Dead) ]
    in
        Random.list listLen rWeight

---- Value Retrieval -----------------

{-| Get the CellData at a given coordinate -}
get : Model -> Coord -> Maybe CellData
get model (col, row) =
    case Array.get col model.vals of
        Just colVals ->
            Array.get row colVals
        _ ->
            Nothing

{-| Get the CellState of the CellData at a given coordinate -}
getState : Model -> Coord -> Maybe CellState
getState model coord =
    case get model coord of
        Just cellData -> Just cellData.state
        _ -> Nothing

---- Boolean Analysis ----------------

{-| Check if the simulation grid contains no Live cells -}
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

------------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------------

{-| Controller messages -}
type Msg
    = NewSim (List CellState)
    | Reset
    | Tick
    | Nil

---- Determining Cell States ---------

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

---- Controller ----------------------

{-| Update the model based upon controller messages -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewSim seed ->
            let
                newModel = { model | vals = gridFromSeed seed }
            in
                (newModel, Cmd.none)
        Reset ->
            (model, Random.generate NewSim seedGen)
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
                    newVals =
                        Array.map (Array.map fxn) model.vals
                    newModel =
                        { model | vals = newVals }
                in
                    (newModel, Cmd.none)
        _ ->
            (model, Cmd.none)

------------------------------------------------------------------------------
-- Subscriptions
------------------------------------------------------------------------------

{-| Process keyboard inputs -}
keyDecoder : Model -> Decoder Msg
keyDecoder model =
    let
        decoder key =
            if (key == " ") then
                Reset
            else
                Nil
    in
        Decode.map decoder (Decode.field "key" Decode.string)

{-| Subscribe to message-generating events -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder model)
        , Time.every tickInterval (\_ -> Tick)
        ]

------------------------------------------------------------------------------
-- View
------------------------------------------------------------------------------

{-| Construct the page header's HTML -}
viewHeader : Html msg
viewHeader =
    let
        attributes =
            [ Html.Attributes.class "header"
            , Html.Attributes.style "background-color" "#F4E8C1"
            , Html.Attributes.style "border-bottom" "solid black"
            , Html.Attributes.style "padding" "10px 0px 10px 0px"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "0px"
            , Html.Attributes.style "top" "0px"
            , Html.Attributes.style "width" "100%"
            ]
    in
        Html.div attributes
            [ Html.h1
                [ Html.Attributes.style "margin" "0px" ]
                [ Html.text "Conway's Game of Life" ]
            ]

{-| Construct the simulation region's HTML -}
viewSimRegion : Model -> Html msg
viewSimRegion model =
    let
        attributes =
            [ Html.Attributes.class "sim-region"
            , Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "margin-top" "80px"
            , Html.Attributes.style "margin-bottom" "50px"
            , Html.Attributes.style "width" "fit-content"
            ]
    in
        Html.div attributes
            [ viewSimGrid model
            , viewInfoPane model
            ]

{-| Construct the simulation grid's HTML -}
viewSimGrid : Model -> Html msg
viewSimGrid model =
    let
        grid n =
            if n > 0 then
                (viewColumn model n)::(grid (n - 1))
            else
                []
        attributes =
            [ Html.Attributes.class "sim-grid"
            , Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "line-height" "0px"
            ]
    in
        Html.div attributes (grid fieldCols)

{-| Construct the HTML for a single column in the simulation grid -}
viewColumn : Model -> Int -> Html msg
viewColumn model cInd =
    let
        column n =
            if n > 0 then
                (viewCell model (cInd - 1, n - 1))::(column (n - 1))
            else
                []
        attributes =
            [ Html.Attributes.class ("col-" ++ (String.fromInt cInd))
            , Html.Attributes.style "display" "inline-block"
            ]
    in
        Html.div attributes (column fieldRows)

{-| Construct a cell's HTML -}
viewCell : Model -> Coord -> Html msg
viewCell model (col, row) =
    let
        state =
            case getState model (col, row) of
                Just Live ->
                    Html.Attributes.class "live-cell"
                _ ->
                    Html.Attributes.class "dead-cell"
        color =
            case getState model (col, row) of
                Just Live ->
                    Html.Attributes.style "background-color" "#8075FF"
                _ ->
                    Html.Attributes.style "background-color" "#DDF8E8"
        attributes =
            [ Html.Attributes.class "cell"
            , Html.Attributes.class ("row-" ++ (String.fromInt row))
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "height" sideLen
            , Html.Attributes.style "width" sideLen
            ]
    in
        Html.div (state::color::attributes)
            []

{-| Construct a live cell's HTML -}
viewLiveCell : Html msg
viewLiveCell =
    let
        attributes =
            [ Html.Attributes.class "live-cell"
            ]
    in
        Html.div attributes
            []

{-| Construct a dead cell's HTML -}
viewDeadCell : Html msg
viewDeadCell =
    let
        attributes =
            [ Html.Attributes.class "dead-cell"
            ]
    in
        Html.div attributes
            []

{-| Construct the info pane HTML -}
viewInfoPane : Model -> Html msg
viewInfoPane model =
    let
        attributes =
            [ Html.Attributes.class "info-pane"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "margin-top" "25px"
            , Html.Attributes.style "font-weight" "bold"
            ]
    in
        Html.div attributes
            [ Html.text "Press 'SPACE' to generate a new grid" ]

{-| Construct the footer HTML -}
viewFooter : Html msg
viewFooter =
    let
        attributes =
            [ Html.Attributes.class "footer"
            , Html.Attributes.style "background-color" "#F4E8C1"
            , Html.Attributes.style "bottom" "0px"
            , Html.Attributes.style "left" "0px"
            , Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "padding" "5px"
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "font-style" "italic"
            , Html.Attributes.style "font-size" "10pt"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "border-top" "solid black"
            ]
    in
        Html.div attributes
            [ Html.text "Conway's Game of Life, written in Elm - © 2019" ]

{-| Construct the application's HTML -}
view : Model -> Html msg
view model =
    let
        global =
            Css.Global.global
                [ Css.Global.body
                    [ Css.backgroundColor (Css.rgb 155 222 172)
                    , Css.margin (Css.px 0)
                    ]
                ]
            |> Html.Styled.toUnstyled
        attributes =
            [ Html.Attributes.class "container"
            , Html.Attributes.style "padding" "10px"
            ]
    in
        Html.div attributes
            [ global
            , viewHeader
            , viewSimRegion model
            , viewFooter
            ]

------------------------------------------------------------------------------
-- Entry Point
------------------------------------------------------------------------------

{-| Configuration flags -}
type alias Flags = ()

{-| Initialize the app -}
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
