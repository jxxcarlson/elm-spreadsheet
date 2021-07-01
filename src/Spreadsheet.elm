module Spreadsheet exposing ( Spreadsheet, TextSpreadsheet
    , eval
    , array2DfromListList, columnWithDefault, evalCell, evalFormula, evalOnce, getCell, isEvaluated, rowWithDefault, spreadSheetFromListList, textSpreadSheetFromListList
    )

{-| This module provides functions to parse, evaluate, and render spreadsheets.

@docs eval


-}

import Array exposing (Array)
import Array2D exposing (Array2D)
import Cell exposing (Cell, Formula(..), Op(..), Operands(..), Value(..))
import CellParser
import Dict exposing (Dict)
import Either exposing (Either(..))
import Maybe.Extra



{-| Evaluate the formulas in a spreadsheet -}
eval : Spreadsheet -> Spreadsheet
eval sheet =
    eval_ { count = Array2D.length sheet, sheet = sheet } |> .sheet


eval_ : { count : Int, sheet : Spreadsheet } -> { count : Int, sheet : Spreadsheet }
eval_ { count, sheet } =
    let
      _ = Debug.log "(count, notEvaluated)" (count, notEvaluated sheet)
    in
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

notEvaluated : Spreadsheet -> Int
notEvaluated sheet =
    Array2D.map Cell.isValue sheet
        |> Array2D.toFlatArrayRowMajor
        |> Array.filter (\b -> not b)
        |> Array.length


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

        Formula op (Range { left, right }) ->
            case op of
                Add ->
                    if left.row == right.row then
                        let
                            val =
                                rowSlice (Right Undefined) left.row left.col right.col sheet
                                    |> List.map Cell.realValue
                                    |> Maybe.Extra.values
                                    |> List.sum
                        in
                        putCell i j (Right (Real val)) sheet

                    else if left.col == right.col then
                        let
                            val =
                                columnSlice (Right Undefined) left.col left.row right.row sheet
                                    |> List.map Cell.realValue
                                    |> Maybe.Extra.values
                                    |> List.sum
                        in
                        putCell i j (Right (Real val)) sheet

                    else
                        sheet

                _ ->
                    sheet


cellOp : (Float -> Float -> Float) -> Int -> Int -> Int -> Int -> Spreadsheet -> Maybe Float
cellOp op i1 j1 i2 j2 sheet =
    Maybe.map2 op (getCell i1 j1 sheet |> Maybe.andThen Cell.realValue) (getCell i2 j2 sheet |> Maybe.andThen Cell.realValue)


columnSlice : a -> Int -> Int -> Int -> Array2D a -> List a
columnSlice default c r1 r2 array =
    List.foldl (\r list -> (Array2D.get r c array |> Maybe.withDefault default) :: list) [] (List.range r1 r2)


rowSlice : a -> Int -> Int -> Int -> Array2D a -> List a
rowSlice default r c1 c2 array =
    List.foldl (\c list -> (Array2D.get r c array |> Maybe.withDefault default) :: list) [] (List.range c1 c2)



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
