module Spreadsheet exposing (..)

import Cell exposing (Cell, Formula(..), Value(..))
import Dict exposing (Dict)
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra



{-
   > Spreadsheet.parse textSheet
       [[Right (Real 100),Right (Real 120),Right (Real 140)],[Right (Real 1.1),Right (Real 1.4),Right (Real 0.9)],[Left (ColumnOp "*" 0 1),Left (ColumnOp "*" 0 1),Left (ColumnOp "*" 0 1)]]

   > > Spreadsheet.parse textSheet |>  applyRealOp 2
     [[Right (Real 100),Right (Real 120),Right (Real 140)],[Right (Real 1.1),Right (Real 1.4),Right (Real 0.9)],[Right (Real 110.00000000000001),Right (Real 168),Right (Real 126)]]
-}
-- TYPES


type alias Row =
    Int


type alias Col =
    Int


type alias SpreadsheetColumn =
    List Cell


type alias Spreadsheet =
    List SpreadsheetColumn


type alias TextSpreadsheet =
    List TextColumn


type alias TextColumn =
    List String



-- PARSE


parse : TextSpreadsheet -> Spreadsheet
parse text =
    List.map parseColumn text


parseColumn : List String -> List Cell
parseColumn cells =
    List.map Cell.parse cells |> Maybe.Extra.values



-- RENDER


render : Spreadsheet -> TextSpreadsheet
render sheet =
    List.map renderColumn sheet


renderColumn : List Cell -> List String
renderColumn cells =
    List.map Cell.render cells



-- COMPUTE


applyRealOp : Col -> Spreadsheet -> Spreadsheet
applyRealOp col sheet =
    List.foldl (\row sheet_ -> applyRealOp_ row col sheet_) sheet (List.range 0 (height sheet - 1))


applyRealOp_ : Row -> Col -> Spreadsheet -> Spreadsheet
applyRealOp_ row col sheet =
    case getCell row col sheet of
        Nothing ->
            sheet

        Just (Left (ColumnOp opSymbol i j)) ->
            case ( Dict.get opSymbol opRealDict, getCell row i sheet, getCell row j sheet ) of
                ( Just op, Just (Right (Real x)), Just (Right (Real y)) ) ->
                    putCell row col (Right (Real (op x y))) sheet

                _ ->
                    sheet

        _ ->
            sheet


{-|

    > textSheet
    [["100.0","120.0","140.0"],["1.1","1.4","0.9"],["*","*","*"]]

    > textSheet |> computeReal 2 0 1
    [["100","120","140"],["1.1","1.4","0.9"],["110.00000000000001","168","126"]]

-}
computeReal : Col -> TextSpreadsheet -> TextSpreadsheet
computeReal opCol text =
    text
        |> parse
        |> applyRealOp opCol
        |> render



-- CELL


getCell : Row -> Col -> Spreadsheet -> Maybe Cell
getCell row col sheet =
    Maybe.andThen (List.Extra.getAt row) (getColumn col sheet)


putCell : Row -> Col -> Cell -> Spreadsheet -> Spreadsheet
putCell row col cell sheet =
    case getColumn col sheet of
        Nothing ->
            sheet

        Just column_ ->
            let
                newColumn =
                    List.Extra.setAt row cell column_
            in
            List.Extra.setAt col newColumn sheet



-- COLUMN


getColumn : Col -> Spreadsheet -> Maybe SpreadsheetColumn
getColumn col sheet =
    List.Extra.getAt col sheet


height : Spreadsheet -> Int
height sheet =
    List.head sheet |> Maybe.map List.length |> Maybe.withDefault 0



-- COMPUTATION


opRealDict : Dict String (Float -> Float -> Float)
opRealDict =
    Dict.fromList
        [ ( "+", (+) )
        , ( "-", (-) )
        , ( "*", (*) )
        , ( "/", (/) )
        ]



-- TEST DATA


testSheetCol1 =
    [ Right (Real 100), Right (Real 120), Right (Real 140) ]


textCol1 =
    [ "100.0", "120.0", "140.0" ]


testSheetCol2 =
    [ Right (Real 1.1), Right (Real 1.4), Right (Real 0.9) ]


textCol2 =
    [ "1.1", "1.4", "0.9" ]


testSheetCol3 =
    [ Left (ColumnOp "*" 0 1), Left (ColumnOp "*" 0 1), Left (ColumnOp "*" 0 1) ]


textCol3 =
    [ "* 0 1", "* 0 1", "* 0 1" ]


testSheet =
    [ testSheetCol1, testSheetCol2, testSheetCol3 ]


textSheet =
    [ textCol1, textCol2, textCol3 ]
