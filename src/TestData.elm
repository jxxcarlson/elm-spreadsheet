module TestData exposing (ast, text)

import Cell exposing (Formula(..), Value(..))
import Either exposing (Either(..))



-- TEST DATA


testSheetCol1 =
    [ Right (Real 100), Right (Real 120), Right (Real 140), Right Undefined ]


textCol1 =
    [ "100.0", "120.0", "140.0", "" ]


testSheetCol2 =
    [ Right (Real 1.1), Right (Real 1.4), Right (Real 0.9), Left (ColOp "col sum" 0 2) ]


textCol2 =
    [ "1.1", "1.4", "0.9", "col sum 0 2" ]


testSheetCol3 =
    [ Left (RowOp "*" 0 1), Left (RowOp "*" 0 1), Left (RowOp "*" 0 1), Left (ColOp "col sum" 0 2) ]


textCol3 =
    [ "row * 0 1", "row * 0 1", "row * 0 1", "col sum 0 2" ]


ast =
    [ testSheetCol1, testSheetCol2, testSheetCol3 ]


text =
    [ textCol1, textCol2, textCol3 ]
