module Main exposing (main)
{-| Conway's Game of Life -}

import Array exposing (Array)
import Browser
import Css
import Css.Global
import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Styled
import Html.Styled.Attributes as Style
import Random exposing (Generator)
import Time

------------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------------

{-| DOCS MISSING -}
fieldCols = 46

{-| DOCS MISSING -}
fieldRows = 50

{-| DOCS MISSING -}
sideLen = "10px"

{-| DOCS MISSING -}
liveP = 20

{-| DOCS MISSING -}
deadP = 80

{-| DOCS MISSING -}
tickInterval = 125

------------------------------------------------------------------------------
-- Convenience Aliases
------------------------------------------------------------------------------

{-| Zero-indexed coordinate reference for a two-dimensional grid -}
type alias Coord = (Int, Int)

------------------------------------------------------------------------------
-- Model
------------------------------------------------------------------------------

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

---- Instantiation -------------------

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

{-| DOCS MISSING -}
seedGen : Generator (List CellState)
seedGen =
    let
        listLen = fieldCols * fieldRows
        rWeight = Random.weighted (liveP, Live) [ (deadP, Dead) ]
    in
        Random.list listLen rWeight

---- Value Retrieval -----------------

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

---- Boolean Analysis ----------------

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

------------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------------

{-| DOCS MISSING -}
type Msg
    = NewSim (List CellState)
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

{-| DOCS MISSING -}
viewSimRegion : Model -> Html msg
viewSimRegion model =
    let
        attributes =
            [ Html.Attributes.class "sim-region"
            , Html.Attributes.style "border" "solid black"
            , Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "margin-top" "107px"
            , Html.Attributes.style "width" "598px"
            ]
    in
        Html.div attributes
            [ viewSimGrid model
            , viewInfoPane model
            ]

{-| DOCS MISSING -}
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
            , Html.Attributes.style "line-height" "0px"
            ]
    in
        Html.div attributes (grid fieldCols)

{-| DOCS MISSING -}
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

{-| DOCS MISSING -}
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
            , Html.Attributes.style "border" "solid black"
            , Html.Attributes.style "height" sideLen
            , Html.Attributes.style "width" sideLen
            ]
    in
        Html.div (state::color::attributes)
            []

{-| DOCS MISSING -}
viewLiveCell : Html msg
viewLiveCell =
    let
        attributes =
            [ Html.Attributes.class "live-cell"
            ]
    in
        Html.div attributes
            []

{-| DOCS MISSING -}
viewDeadCell : Html msg
viewDeadCell =
    let
        attributes =
            [ Html.Attributes.class "dead-cell"
            ]
    in
        Html.div attributes
            []

{-| DOCS MISSING -}
viewInfoPane : Model -> Html msg
viewInfoPane model =
    let
        attributes =
            [ Html.Attributes.class "info-pane"
            ]
    in
        Html.div attributes
            []

{-| DOCS MISSING -}
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

{-| DOCS MISSING -}
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
