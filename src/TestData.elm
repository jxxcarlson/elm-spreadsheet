module TestData exposing (ast, text)

import Cell exposing (Formula(..), Value(..))
import Either exposing (Either(..))



-- TEST DATA


testSheetCol1 =
    [ Right (Real 100), Right (Real 120), Right (Real 140), Right Undefined ]


textCol1 =
    [ "100.0", "120.0", "140.0", "-" ]


testSheetCol2 =
    [ Right (Real 1.1), Right (Real 1.4), Right (Real 0.9), Left (ColOp "col sum" 1 3) ]


textCol2 =
    [ "1.1", "1.4", "0.9", "col sum 1 3" ]


testSheetCol3 =
    [ Left (RowOp "*" 1 2), Left (RowOp "*" 1 2), Left (RowOp "*" 1 2), Left (ColOp "col sum" 1 3) ]


textCol3 =
    [ "row * 1 2", "row * 1 2", "row * 1 2", "col sum 1 3" ]


ast =
    [ testSheetCol1, testSheetCol2, testSheetCol3 ]


text =
    [ textCol1, textCol2, textCol3 ]
