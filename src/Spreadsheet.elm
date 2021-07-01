module Spreadsheet exposing
    ( Spreadsheet, TextSpreadsheet
    , eval
    ,  array2DfromListList
      , columnWithDefault
      , evalCell
      , evalFormula
      , evalOnce
      , getCell
      , isEvaluated
      , rowWithDefault
      , spreadSheetFromListList
      , textSpreadSheetFromListList
        -- , parse, eval, evalText, render

    )

{-| This module provides functions to parse, evaluate, and render spreadsheets.

@docs Spreadsheet, TextSpreadsheet

@docs parse, eval, evalText, render

-}

import Array exposing (Array)
import Array2D exposing (Array2D)
import Cell exposing (Cell, Formula(..), Op(..), Operands(..), Value(..))
import CellParser
import Dict exposing (Dict)
import Either exposing (Either(..))


eval : Spreadsheet -> Spreadsheet
eval sheet =
    eval_ { count = 10, sheet = sheet } |> .sheet


eval_ : { count : Int, sheet : Spreadsheet } -> { count : Int, sheet : Spreadsheet }
eval_ { count, sheet } =
    if count == 0 then
        { count = count, sheet = sheet }

    else if isEvaluated sheet then
        { count = count, sheet = sheet }

    else
        eval_ { count = count - 1, sheet = evalOnce sheet }


isEvaluated : Spreadsheet -> Bool
isEvaluated sheet =
    Array2D.map Cell.isValue sheet
        |> Array2D.toFlatArrayRowMajor
        |> Array.foldl
            (\b acc ->
                if b == False then
                    b

                else
                    acc
            )
            True



-- TYPES


type alias Row =
    Int


type alias Col =
    Int


{-| -}
type alias SpreadsheetColumn =
    List Cell


{-| -}
type alias Spreadsheet =
    Array2D Cell


{-| -}
type alias TextSpreadsheet =
    Array2D String


type alias TextColumn =
    List String



-- FUNCTIONS


getCell : Int -> Int -> Spreadsheet -> Maybe Cell
getCell i j sheet =
    Array2D.get i j sheet



-- PARSE


spreadSheetFromListList : List (List String) -> Maybe Spreadsheet
spreadSheetFromListList lists =
    textSpreadSheetFromListList lists |> Maybe.map (Array2D.map CellParser.parse)


textSpreadSheetFromListList : List (List String) -> Maybe TextSpreadsheet
textSpreadSheetFromListList lists =
    array2DfromListList lists


array2DfromListList : List (List a) -> Maybe (Array2D a)
array2DfromListList lists =
    Array.fromList (List.map Array.fromList lists)
        |> Array2D.fromRows


evalOnce : Spreadsheet -> Spreadsheet
evalOnce sheet =
    Array.foldl (\( i, j, cell ) sheet_ -> evalCell i j cell sheet_) sheet (Array2D.indexedMap (\i_ j_ cell_ -> ( i_, j_, cell_ )) sheet |> Array2D.toFlatArrayRowMajor)


evalCell : Int -> Int -> Cell -> Spreadsheet -> Spreadsheet
evalCell i j cell sheet =
    case cell of
        Right _ ->
            sheet

        Left formula ->
            evalFormula i j formula sheet


evalFormula : Int -> Int -> Formula -> Spreadsheet -> Spreadsheet
evalFormula i j formula sheet =
    case formula of
        Formula op (Pair { left, right }) ->
            case op of
                Add ->
                    case cellOp (+) left.row left.col right.row right.col sheet of
                        Nothing ->
                            sheet

                        Just val ->
                            putCell i j (Right (Real val)) sheet

                Mul ->
                    case cellOp (*) left.row left.col right.row right.col sheet of
                        Nothing ->
                            sheet

                        Just val ->
                            putCell i j (Right (Real val)) sheet

                Sub ->
                    case cellOp (-) left.row left.col right.row right.col sheet of
                        Nothing ->
                            sheet

                        Just val ->
                            putCell i j (Right (Real val)) sheet

                Div ->
                    case cellOp (/) left.row left.col right.row right.col sheet of
                        Nothing ->
                            sheet

                        Just val ->
                            putCell i j (Right (Real val)) sheet

                NoOp ->
                    sheet

        _ ->
            sheet


cellOp : (Float -> Float -> Float) -> Int -> Int -> Int -> Int -> Spreadsheet -> Maybe Float
cellOp op i1 j1 i2 j2 sheet =
    Maybe.map2 op (getCell i1 j1 sheet |> Maybe.andThen Cell.realValue) (getCell i2 j2 sheet |> Maybe.andThen Cell.realValue)



--
--{-| -}
--parse : TextSpreadsheet -> Spreadsheet
--parse sheet =
--    Array2D.map CellParser.parse sheet
--
--
--
----
----parseColumn : List String -> Array Cell
----parseColumn cells =
----    List.map Cell.parse cells
----      |> Array.fromList
---- RENDER
--
--
--{-| Convert a List Cell to a List (List String) representation.
---}
--render : Spreadsheet -> TextSpreadsheet
--render sheet =
--    Array2D.map Cell.render sheet
--
--
--
----renderColumn : Array Cell -> List String
----renderColumn cells =
----    List.map Cell.render cells
---- EVAL
--
--
--{-| Evaluate a Spreadsheet using the formula cells.
---}
--eval : Spreadsheet -> Spreadsheet
--eval sheet =
--    sheet |> evalRowOps |> evalColOps
--
--
--evalColOps : Spreadsheet -> Spreadsheet
--evalColOps sheet =
--    List.foldl (\i sheet_ -> applyColOp i sheet_) sheet (List.range 0 (width sheet - 1))
--
--
--applyColOp : Col -> Spreadsheet -> Spreadsheet
--applyColOp col sheet =
--    List.foldl (\i sheet_ -> applyColOp_ i col sheet_) sheet (List.range 0 (height sheet - 1))
--
--
--applyColOp_ : Row -> Col -> Spreadsheet -> Spreadsheet
--applyColOp_ i j sheet =
--    case getCell i j sheet of
--        Nothing ->
--            sheet
--
--        Just (Left (ColOp opSymbol r1 r2)) ->
--            case opSymbol of
--                AddRange ->
--                    putCell i j (sumColumn j r1 r2 sheet) sheet
--
--
--                _ ->
--                    sheet
--
--        _ ->
--            sheet
--
--
--
--sumColumn : Col -> Row -> Row -> Spreadsheet -> Cell
--sumColumn col row1 row2 sheet =
--    columnWithDefault (Right Undefined) col sheet |> sumColumn_ row1 row2
--
--
--
---- sumColumn_ : Row -> Row -> SpreadsheetColumn -> Cell
--
--
--sumColumn_ : Int -> Int -> Array Cell -> Cell
--sumColumn_ row1 row2 cells =
--    Array.foldl (\cell acc -> addCell acc cell) 0 (Array.slice row1 (row2 + 1) cells) |> (\x -> Right (Real x))
--
--
--addCell : Float -> Cell -> Float
--addCell y cell =
--    case cell of
--        Right (Real x) ->
--            x + y
--
--        _ ->
--            0
--
--
--listSlice : Int -> Int -> List a -> List a
--listSlice a b list =
--    list |> List.take (b + 1) |> List.drop a
--
--
--evalRowOps : Spreadsheet -> Spreadsheet
--evalRowOps sheet =
--    List.foldl (\col sheet_ -> applyRowOp col sheet_) sheet (List.range 0 (width sheet - 1))
--
--
--applyRowOp : Col -> Spreadsheet -> Spreadsheet
--applyRowOp col sheet =
--    List.foldl (\i sheet_ -> applyRowOp_ i col sheet_) sheet (List.range 0 (height sheet - 1))
--
--
--applyRowOp_ : Row -> Col -> Spreadsheet -> Spreadsheet
--applyRowOp_ row_ col sheet =
--    case getCell row_ col sheet of
--        Nothing ->
--            sheet
--
--        Just (Left (RowOp opSymbol i j)) ->
--            case ( Dict.get (Cell.stringFromOp opSymbol) opRealDict, getCell row_ i sheet, getCell row_ j sheet ) of
--                ( Just op, Just (Right (Real x)), Just (Right (Real y)) ) ->
--                    putCell row_ col (Right (Real (op x y))) sheet
--
--                _ ->
--                    sheet
--
--        _ ->
--            sheet
--
--
--{-| Use the formulas in the cells to evaluate a TextSpreadsheet.
---}
--evalText : TextSpreadsheet -> TextSpreadsheet
--evalText text =
--    text
--        |> parse
--        |> eval
--        |> render
--
--
-- CELL


putCell : Row -> Col -> Cell -> Spreadsheet -> Spreadsheet
putCell row_ col cell sheet =
    Array2D.set row_ col cell sheet



-- COLUMN
--getColumn : Col -> Spreadsheet -> Maybe SpreadsheetColumn
--getColumn col sheet =
--    List.Extra.getAt col sheet


height : Spreadsheet -> Int
height sheet =
    Array2D.rows sheet


width : Spreadsheet -> Int
width sheet =
    Array2D.columns sheet



-- COMPUTATION


opRealDict : Dict String (Float -> Float -> Float)
opRealDict =
    Dict.fromList
        [ ( "+", (+) )
        , ( "-", (-) )
        , ( "*", (*) )
        , ( "/", (/) )
        ]



-- HELPERS
-- ADDED by jxxcarlson


row : Int -> Array2D a -> Array (Maybe a)
row i array =
    List.foldl (\j acc -> Array2D.get i j array :: acc) [] (List.range 0 (Array2D.columns array - 1))
        |> List.reverse
        |> Array.fromList


rowWithDefault : a -> Int -> Array2D a -> Array a
rowWithDefault default i array =
    List.foldl (\j acc -> (Array2D.get i j array |> Maybe.withDefault default) :: acc) [] (List.range 0 (Array2D.columns array - 1))
        |> List.reverse
        |> Array.fromList


column : Int -> Array2D a -> Array (Maybe a)
column j array =
    List.foldl (\i acc -> Array2D.get i j array :: acc) [] (List.range 0 (Array2D.rows array - 1))
        |> List.reverse
        |> Array.fromList


columnWithDefault : a -> Int -> Array2D a -> Array a
columnWithDefault default j array =
    List.foldl (\i acc -> (Array2D.get i j array |> Maybe.withDefault default) :: acc) [] (List.range 0 (Array2D.rows array - 1))
        |> List.reverse
        |> Array.fromList
