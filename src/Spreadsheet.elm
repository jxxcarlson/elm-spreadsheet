module Spreadsheet exposing (read, print, readFromList, eval, getCell, evalFormula, isEvaluated)



{-| This module provides functions to parse, evaluate, and render spreadsheets.

@docs read, readFromList, print, getCell, eval, evalFormula, isEvaluated


-}

import Array exposing (Array)
import Array2D exposing (Array2D)
import Cell exposing (Cell, Formula(..), Op(..), Operands(..), Value(..))
import CellParser
import Dict exposing (Dict)
import Either exposing (Either(..))
import Maybe.Extra
import List.Extra

s7 = """

100; 1.1; mul A1,A2
120; 1.4; mul B1,B2
140; 0.9; mul C1,C2
-;   add A2:C2; add A3:C3

""" 

{-| -}
print : Array2D (Cell) -> String
print sheet = 
    let 
      cols = Array2D.columns sheet
    in
    Array2D.map Cell.render sheet
        |> Array2D.toFlatArrayRowMajor
        |> Array.toList
        |> List.Extra.groupsOf cols
        |> List.map (String.join "; ")
        |> String.join "\n"
     
  

{-| -}
read: String ->  Spreadsheet
read str = 
  str 
   |> String.lines
   |> List.map String.trim
   |> List.filter (\line -> line /= "")
   |> List.map (String.split ";" >> (List.map String.trim))
   |> readFromList
   

{-| Evaluate the formulas in a spreadsheet -}
eval : Spreadsheet -> Spreadsheet
eval sheet =
    eval_ { notEvaluated = Array2D.length sheet, sheet = evalOnce sheet } |> .sheet


eval_ : { notEvaluated : Int, sheet : Spreadsheet } -> { notEvaluated : Int, sheet : Spreadsheet }
eval_ { notEvaluated, sheet } =
    let
      sheet2 = evalOnce sheet
      notEvaluated2 = countNotEvaluated sheet2

    in
    if notEvaluated2 == notEvaluated then
        { notEvaluated = notEvaluated, sheet = sheet }

    else 
      eval_  { notEvaluated = notEvaluated2, sheet = sheet2 }


{-| -}
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

countNotEvaluated : Spreadsheet -> Int
countNotEvaluated sheet =
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
type alias Spreadsheet =
    Array2D Cell

emptySpreadsheet : Array2D Cell
emptySpreadsheet = Array2D.repeat 1 1 (Right (String "nothing"))
{-| -}
type alias TextSpreadsheet =
    Array2D String





-- FUNCTIONS

{-| -}
getCell : Int -> Int -> Spreadsheet -> Maybe Cell
getCell i j sheet =
    Array2D.get i j sheet



-- PARSE

{-| -}
readFromList : List (List String) -> Spreadsheet
readFromList lists =
    textSpreadSheetFromListList lists 
       |> Maybe.map (Array2D.map CellParser.parse)
       |> Maybe.withDefault emptySpreadsheet



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

{-| -}
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
