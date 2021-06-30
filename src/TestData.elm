module TestData exposing (ast, text, c,d)

import Cell exposing (Formula(..), Value(..))
import Either exposing (Either(..))



-- TEST DATA


textCol1 =
    [ "100.0", "120.0", "140.0", "-" ]


textCol2 =
    [ "1.1", "1.4", "0.9", "col sum 1 3" ]


textCol3 =
    [ "row * 1 2", "row * 1 2", "row * 1 2", "col sum 1 3" ]

ast :  List (List (Either Formula Value))
ast =
    [ [ Right (Real 100), Right (Real 120), Right (Real 140), Right Undefined ], [ Right (Real 1.1), Right (Real 1.4), Right (Real 0.9), Left (ColOp "sum" 0 2) ], [ Left (RowOp "*" 0 1), Left (RowOp "*" 0 1), Left (RowOp "*" 0 1), Left (ColOp "sum" 0 2) ] ]


text =
    [ textCol1, textCol2, textCol3 ]

c1 =
    [ "100.0", "120.0", "140.0", "-" ]


c2 =
    [ "1.1", "1.4", "0.9", "col sum 1 3" ]


c3 =
    [ "a", "b2", "c", "d" ]


c = [c1, c2, c3]

d1 = ["100", "1.1", "a"]
d2 = ["120", "1.4", "b"]
d3 = ["140", "0.9", "c"]
d4 = ["-", "col sum 1 3", "d"]

d = [d1,d2, d3, d4]