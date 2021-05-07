module Spreadsheet exposing
    ( Spreadsheet, TextSpreadsheet
    , parse, eval, evalText, render
    )

{-| This module provides functions to parse, evaluate, and render spreadsheets.

@docs Spreadsheet, TextSpreadsheet

@docs parse, eval, evalText, render

-}

import Cell exposing (Cell, Formula(..), Value(..))
import Dict exposing (Dict)
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra



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
    List (List Cell)


{-| -}
type alias TextSpreadsheet =
    List (List String)


type alias TextColumn =
    List String



-- PARSE


{-| -}
parse : TextSpreadsheet -> Spreadsheet
parse text =
    List.map parseColumn text


parseColumn : List String -> List Cell
parseColumn cells =
    List.map Cell.parse cells



-- RENDER


{-| Convert a List Cell to a List (List String) representation.
-}
render : Spreadsheet -> TextSpreadsheet
render sheet =
    List.map renderColumn sheet


renderColumn : List Cell -> List String
renderColumn cells =
    List.map Cell.render cells



-- EVAL


{-| Evaluate a Spreadsheet using the formula cells.
-}
eval : Spreadsheet -> Spreadsheet
eval sheet =
    sheet |> evalRowOps |> evalColOps


evalColOps : Spreadsheet -> Spreadsheet
evalColOps sheet =
    List.foldl (\row sheet_ -> applyColOp row sheet_) sheet (List.range 0 (width sheet - 1))


applyColOp : Col -> Spreadsheet -> Spreadsheet
applyColOp col sheet =
    List.foldl (\row sheet_ -> applyColOp_ row col sheet_) sheet (List.range 0 (height sheet - 1))


applyColOp_ : Row -> Col -> Spreadsheet -> Spreadsheet
applyColOp_ row col sheet =
    case getCell row col sheet of
        Nothing ->
            sheet

        Just (Left (ColOp opSymbol i j)) ->
            case opSymbol of
                "sum" ->
                    putCell row col (sumColumn col i j sheet) sheet

                _ ->
                    sheet

        _ ->
            sheet


sumColumn : Col -> Row -> Row -> Spreadsheet -> Cell
sumColumn col row1 row2 sheet =
    case getColumn col sheet of
        Nothing ->
            Right Undefined

        Just column_ ->
            sumColumn_ row1 row2 column_



-- sumColumn_ : Row -> Row -> SpreadsheetColumn -> Cell


sumColumn_ : Int -> Int -> List Cell -> Cell
sumColumn_ row1 row2 column =
    List.foldl (\cell acc -> addCell acc cell) 0 (listSlice row1 row2 column) |> (\x -> Right (Real x))


addCell : Float -> Cell -> Float
addCell y cell =
    case cell of
        Right (Real x) ->
            x + y

        _ ->
            0


listSlice : Int -> Int -> List a -> List a
listSlice a b list =
    list |> List.take (b + 1) |> List.drop a


evalRowOps : Spreadsheet -> Spreadsheet
evalRowOps sheet =
    List.foldl (\col sheet_ -> applyRowOp col sheet_) sheet (List.range 0 (width sheet - 1))


applyRowOp : Col -> Spreadsheet -> Spreadsheet
applyRowOp col sheet =
    List.foldl (\row sheet_ -> applyRowOp_ row col sheet_) sheet (List.range 0 (height sheet - 1))


applyRowOp_ : Row -> Col -> Spreadsheet -> Spreadsheet
applyRowOp_ row col sheet =
    case getCell row col sheet of
        Nothing ->
            sheet

        Just (Left (RowOp opSymbol i j)) ->
            case ( Dict.get opSymbol opRealDict, getCell row i sheet, getCell row j sheet ) of
                ( Just op, Just (Right (Real x)), Just (Right (Real y)) ) ->
                    putCell row col (Right (Real (op x y))) sheet

                _ ->
                    sheet

        _ ->
            sheet


{-| Use the formulas in the cells to evaluate a TextSpreadsheet.
-}
evalText : TextSpreadsheet -> TextSpreadsheet
evalText text =
    text
        |> parse
        |> eval
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


width : Spreadsheet -> Int
width sheet =
    List.length sheet



-- COMPUTATION


opRealDict : Dict String (Float -> Float -> Float)
opRealDict =
    Dict.fromList
        [ ( "+", (+) )
        , ( "-", (-) )
        , ( "*", (*) )
        , ( "/", (/) )
        ]
