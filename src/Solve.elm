module Solve exposing (..)

import Cell exposing (Cell, Col, Formula(..), Row, Value(..))
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra
import Spreadsheet exposing (Spreadsheet)
import TestData
import Tree exposing (Tree(..), singleton, tree)
import Tree.Extra
import Tree.Zipper as Zipper exposing (Zipper)



-- EDGES FROM SPREADSHEET


e1 =
    tree ( 0, 2 ) [ singleton ( 0, 0 ), singleton ( 0, 1 ) ]


type alias TreeData =
    { trees : List (Tree ( Int, Int )), nodes : List ( Int, Int ) }


labelInTree : a -> Tree a -> Bool
labelInTree label tree =
    Tree.foldl
        (\label_ bool ->
            if label_ == label then
                True

            else
                bool
        )
        False
        tree


treesWithLabel : a -> List (Tree a) -> List (Tree a)
treesWithLabel label trees =
    List.foldl
        (\tree acc ->
            if labelInTree label tree then
                tree :: acc

            else
                acc
        )
        []
        trees


treesFromEdges : List (Edge ( Int, Int )) -> TreeData
treesFromEdges edges =
    List.foldl addTreeFromEdge { trees = [], nodes = [] } edges


addTreeFromEdge : Edge ( Int, Int ) -> TreeData -> TreeData
addTreeFromEdge edge treeData =
    case ( List.member edge.from treeData.nodes, List.member edge.to treeData.nodes ) of
        ( False, False ) ->
            -- None of the nodes of the edge are in node list
            let
                newNodes =
                    edge.from :: edge.to :: treeData.nodes

                newTree =
                    tree edge.from [ singleton edge.to ]

                newTrees =
                    newTree :: treeData.trees
            in
            { treeData | nodes = newNodes, trees = newTrees }

        ( False, True ) ->
            -- Terminal node of edge is in node list
            let
                nodes =
                    edge.from :: treeData.nodes

                _ =
                    Debug.log "EDGE" edge

                _ =
                    Debug.log "TREES WITH LABEL" (treesWithLabel edge.to treeData.trees)

                _ =
                    Debug.log "TREES" treeData.trees

                --newTrees =
                --    List.map Tree.Extra.attach
            in
            { treeData | nodes = nodes }

        ( True, False ) ->
            let
                nodes =
                    edge.to :: treeData.nodes
            in
            { treeData | nodes = nodes }

        ( True, True ) ->
            let
                nodes =
                    treeData.nodes
            in
            { treeData | nodes = nodes }


edgeInNodeList : Edge ( Int, Int ) -> List ( Int, Int ) -> Bool
edgeInNodeList edge nodes =
    List.member edge.from nodes || List.member edge.to nodes


addNode : ( Int, Int ) -> TreeData -> TreeData
addNode node treeData =
    if List.member node treeData.nodes then
        treeData

    else
        { treeData | nodes = node :: treeData.nodes }


{-|

    > edgesFromSpreadsheet ast |> List.sortWith edgeGT
    [{ from = (0,2), to = (0,0) },{ from = (0,2), to = (0,1) },{ from = (1,2), to = (1,0) },{ from = (1,2), to = (1,1) },{ from = (2,2), to = (2,0) },{ from = (2,2), to = (2,1) },{ from = (3,1), to = (0,1) },{ from = (3,1), to = (1,1) },{ from = (3,1), to = (2,1) },{ from = (3,2), to = (0,2) },{ from = (3,2), to = (1,2) },{ from = (3,2), to = (2,2) }]

-}
edgesFromSpreadsheet : List (List Cell) -> List (Edge ( Int, Int ))
edgesFromSpreadsheet sheet =
    sheet
        |> augmentSpreadsheet
        |> List.map (List.map edgesFromAugmentedCell)
        |> List.concat
        |> List.concat


edgesFromAugmentedCell : ( ( Row, Col ), Cell ) -> List (Edge ( Int, Int ))
edgesFromAugmentedCell ( ( row, col ), cell ) =
    case cell of
        Right _ ->
            []

        Left formula ->
            edgesFromFormula row col formula


edgesFromFormula : Row -> Col -> Formula -> List (Edge ( Int, Int ))
edgesFromFormula row col formula =
    case formula of
        RowOp op i j ->
            if List.member op [ "+", "-", "*", "/" ] then
                [ { from = ( row, col ), to = ( row, i ) }, { from = ( row, col ), to = ( row, j ) } ]

            else
                []

        ColOp op row1 row2 ->
            if List.member op [ "sum" ] then
                List.map (\row_ -> { from = ( row, col ), to = ( row_, col ) }) (List.range row1 row2)

            else
                []


type alias Edge a =
    { from : a, to : a }


nodeCompare : ( Int, Int ) -> ( Int, Int ) -> Order
nodeCompare ( a, b ) ( c, d ) =
    if a > c then
        GT

    else if a == c then
        if b > d then
            GT

        else if b == d then
            EQ

        else
            LT

    else
        LT


edgeCompare : Edge ( Int, Int ) -> Edge ( Int, Int ) -> Order
edgeCompare a b =
    let
        fromOrd =
            nodeCompare a.from b.from
    in
    case fromOrd of
        GT ->
            GT

        LT ->
            LT

        EQ ->
            nodeCompare a.to b.to


nodesFromEdges : List (Edge ( Int, Int )) -> List ( Int, Int )
nodesFromEdges edges =
    edges
        |> List.map (\e -> [ e.from, e.to ])
        |> List.concat
        |> List.Extra.unique



--TREES FROM SPREADSHEET


treesFromSpreadsheet : List (List Cell) -> List (Tree ( Int, Int ))
treesFromSpreadsheet sheet =
    sheet
        |> augmentSpreadsheet
        |> List.map (List.map treeFromAugmentedCell)
        |> List.map Maybe.Extra.values
        |> List.concat


treeFromAugmentedCell : ( ( Row, Col ), Cell ) -> Maybe (Tree ( Int, Int ))
treeFromAugmentedCell ( ( row, col ), cell ) =
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



-- AUGMENT


augmentSpreadsheet : List (List Cell) -> List (List ( ( Int, Int ), Cell ))
augmentSpreadsheet list =
    list |> augmentColumnsOfSpreadsheet |> augmentRowsOfSpreadsheet


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



-- UTILITY


cellAt : Row -> Col -> List (List a) -> Maybe a
cellAt row col list =
    List.Extra.getAt col list |> Maybe.andThen (List.Extra.getAt row)


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
