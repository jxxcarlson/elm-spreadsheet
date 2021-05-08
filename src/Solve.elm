module Solve exposing (..)

import Cell exposing (Cell, Col, Formula(..), Row, Value(..))
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra
import Spreadsheet exposing (Spreadsheet)
import TestData
import Tree exposing (Tree(..), singleton, tree)



-- TREES FROM SPREADSHEET


treesFromSpreadsheet : List (List Cell) -> List (Tree ( Int, Int ))
treesFromSpreadsheet sheet =
    sheet
        |> augmentSpreadsheet
        |> List.map (List.map treeFromCAugmentedCell)
        |> List.map Maybe.Extra.values
        |> List.concat


cellAt : Row -> Col -> List (List a) -> Maybe a
cellAt row col list =
    List.Extra.getAt col list |> Maybe.andThen (List.Extra.getAt row)


augmentCells : List Cell -> List ( Int, Cell )
augmentCells cells =
    List.indexedMap (\k cell -> ( k, cell )) cells


augmentColumnsOfSpreadsheet : List (List Cell) -> List (List ( Int, Cell ))
augmentColumnsOfSpreadsheet sheet =
    List.map augmentCells sheet


augmentRowOfSpreadsheet : Int -> List ( Int, Cell ) -> List ( ( Int, Int ), Cell )
augmentRowOfSpreadsheet i list =
    List.map (\( j, cell ) -> ( ( j, i ), cell )) list


augmentRowsOfSpreadsheet : List (List ( Int, Cell )) -> List (List ( ( Int, Int ), Cell ))
augmentRowsOfSpreadsheet list =
    List.indexedMap (\row cells -> augmentRowOfSpreadsheet row cells) list


augmentSpreadsheet : List (List Cell) -> List (List ( ( Int, Int ), Cell ))
augmentSpreadsheet list =
    list |> augmentColumnsOfSpreadsheet |> augmentRowsOfSpreadsheet


treeFromCAugmentedCell : ( ( Row, Col ), Cell ) -> Maybe (Tree ( Int, Int ))
treeFromCAugmentedCell ( ( row, col ), cell ) =
    case cell of
        Right _ ->
            Nothing

        Left formula ->
            treeFromFormula row col formula


treeFromFormula : Row -> Col -> Formula -> Maybe (Tree ( Int, Int ))
treeFromFormula row col formula =
    case formula of
        RowOp op i j ->
            if List.member op [ "+", "-", "*", "/" ] then
                Just (tree ( row, col ) [ singleton ( row, i ), singleton ( row, j ) ])

            else
                Nothing

        ColOp op i j ->
            if List.member op [ "sum" ] then
                Just (tree ( row, col ) (singletonsWithRowRange col i j))

            else
                Nothing


singletonsWithRowRange : Col -> Row -> Row -> List (Tree ( Int, Int ))
singletonsWithRowRange col row1 row2 =
    List.map (\row -> singleton ( row, col )) (List.range row1 row2)


{-| -}



-- TEST


t1 =
    tree ( 0, 2 ) [ singleton ( 0, 0 ), singleton ( 0, 1 ) ]


t2 =
    tree ( 1, 2 ) [ singleton ( 0, 0 ), singleton ( 0, 1 ) ]


t3 =
    tree ( 2, 2 ) [ singleton ( 0, 0 ), singleton ( 0, 1 ) ]


t4 =
    tree ( 3, 2 ) [ singleton ( 0, 2 ), singleton ( 1, 2 ), singleton ( 2, 2 ) ]


t5 =
    tree ( 3, 1 ) [ singleton ( 0, 1 ), singleton ( 1, 1 ), singleton ( 1, 2 ) ]
