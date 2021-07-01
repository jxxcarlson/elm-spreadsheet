module Solve exposing (..)

import Cell exposing (Cell, Col, Formula(..), Row, Value(..))
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra
import Tree exposing (Tree(..), singleton, tree)
import TreeUtil exposing (..)



-- EDGES FROM SPREADSHEET


e1 =
    tree ( 0, 2 ) [ singleton ( 0, 0 ), singleton ( 0, 1 ) ]


type alias TreeData =
    { count : Int, trees : List (Tree ( Int, Int )), nodes : List ( Int, Int ) }


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
    List.foldl addTreeFromEdge { count = 0, trees = [], nodes = [] } edges


reduce : List (Tree ( Int, Int )) -> List (Tree ( Int, Int ))
reduce trees =
    case List.Extra.uncons trees of
        Nothing ->
            trees

        Just ( head, remaining ) ->
            let
                leafLabels =
                    TreeUtil.getLeaves head |> List.map Tree.label

                targets =
                    List.filter (\tree -> List.member (Tree.label tree) leafLabels) remaining

                folder : Tree a -> Tree a -> Tree a
                folder =
                    \tree acc -> attach tree acc

                remaining2 =
                    List.filter (\tree -> not (List.member tree targets)) remaining
            in
            List.foldl folder head remaining :: remaining2


reduce2 : List (Tree ( Int, Int )) -> List (Tree ( Int, Int ))
reduce2 trees =
    case List.Extra.uncons trees of
        Nothing ->
            trees

        Just ( head, remaining ) ->
            let
                leafLabels =
                    TreeUtil.getLeaves head |> List.map Tree.label 

                targets =
                    List.filter (TreeUtil.hasLabelIn leafLabels) remaining 

                prepare : ( List a, Tree a ) -> Maybe ( a, Tree a )
                prepare ( labels, tree ) =
                    case List.head labels of
                        Nothing ->
                            Nothing

                        Just a ->
                            Just ( a, tree )

                pairs : List ( ( Int, Int ), Tree ( Int, Int ) )
                pairs =
                    List.map (\target -> ( TreeUtil.commonLabels leafLabels target, target )) targets
                        |> List.map prepare
                        |> Maybe.Extra.values


                folder2 : ( ( Int, Int ), Tree ( Int, Int ) ) -> Tree ( Int, Int ) -> Tree ( Int, Int )
                folder2 ( label, target ) tree =
                    TreeUtil.attachAtLabel label target tree

                folder : Tree a -> Tree a -> Tree a
                folder =
                    \tree acc -> attach tree acc

                remaining2 =
                    List.filter (\tree -> not (List.member tree targets)) remaining
            in
            List.foldl folder2 head pairs :: remaining2



-- REDUCE3


type alias ReducerState =
    { tree : Tree ( Int, Int ), list : List (Tree ( Int, Int )) }



-- loop : state -> (state -> Step state a) -> a


reduce3 : List (Tree ( Int, Int )) -> Maybe (Tree ( Int, Int ))
reduce3 list =
    case List.Extra.uncons list of
        Nothing ->
            Nothing

        Just ( tree, remaining ) ->
            Just (loop { tree = tree, list = remaining } updateReducerState)


updateReducerState : ReducerState -> Step ReducerState (Tree ( Int, Int ))
updateReducerState state =
    case List.Extra.uncons state.list of
        Nothing ->
            Done state.tree

        Just ( tree, remaining ) ->
            Loop { tree = attachTree tree state.tree, list = remaining }


attachTree : Tree ( Int, Int ) -> Tree ( Int, Int ) -> Tree ( Int, Int )
attachTree tree1 tree2 =
    case List.head (TreeUtil.commonLabelsOfTrees tree1 tree2) of
        Nothing ->
            tree2

        Just a ->
            TreeUtil.attachAtLabel a tree1 tree2


insert : a -> List a -> List a
insert a list =
    if List.member a list then
        list

    else
        a :: list


multipleInsert : List a -> List a -> List a
multipleInsert insertions list =
    List.foldl (\item list_ -> insert item list_) list insertions


addTreeFromEdge : Edge ( Int, Int ) -> TreeData -> TreeData
addTreeFromEdge edge treeData =
    --let
    --    indicator =
    --        ( List.member edge.from treeData.nodes, List.member edge.to treeData.nodes )
    --
    --    _ =
    --        Debug.log "TREES" ( ( treeData.count, List.length treeData.trees, indicator ), edge, treeData.trees )
    --in
    case ( List.member edge.from treeData.nodes, List.member edge.to treeData.nodes ) of
        ( False, False ) ->
            -- None of the nodes of the edge are in node list
            let
                newNodes =
                    edge.from :: edge.to :: treeData.nodes

                newTree =
                    tree edge.from [ singleton edge.to ]
            in
            { treeData | count = treeData.count + 1, nodes = newNodes, trees = insert newTree treeData.trees }

        ( False, True ) ->
            -- Terminal node of edge is in node list
            let
                newNodes =
                    edge.from :: treeData.nodes

                newTree =
                    Tree.tree edge.from [ Tree.singleton edge.to ]

                --|> Debug.log "TREE of EDGE"
                treesForAttachment =
                    TreeUtil.treesWithLabel edge.to treeData.trees

                --|> Debug.log "TREES FOR ATTACHMENT"
                treesWithoutAttachment =
                    List.foldl (\tree_ acc -> List.Extra.remove tree_ acc) treeData.trees treesForAttachment

                -- |> Debug.log "TREES WITHOUT ATTACHMENT"
                newTrees =
                    List.map (\tree_ -> attach tree_ newTree) treesForAttachment

                -- |> Debug.log "NEW TREES"
            in
            { treeData | count = treeData.count + 1, nodes = newNodes, trees = newTrees ++ treesWithoutAttachment }

        ( True, False ) ->
            let
                nodes =
                    edge.to :: treeData.nodes

                --_ =
                --    Debug.log "EDGE" edge
                newTree =
                    Tree.tree edge.from [ singleton edge.to ]

                treesForAttachment =
                    TreeUtil.treesWithLabel edge.from treeData.trees

                --|> Debug.log "TREES FOR ATTACHMENT"
                treesWithoutAttachment =
                    List.foldl (\tree_ acc -> List.Extra.remove tree_ acc) treeData.trees treesForAttachment

                --|> Debug.log "TREES WITHOUT ATTACHMENT"
                newTrees =
                    List.map (\tree_ -> TreeUtil.attachAtLabel edge.from newTree tree_) treesForAttachment

                --|> Debug.log "NEW TREES"
            in
            { treeData | count = treeData.count + 1, nodes = nodes, trees = newTrees ++ treesWithoutAttachment }

        ( True, True ) ->
            let
                nodes =
                    treeData.nodes

                newTree =
                    Tree.tree edge.from [ singleton edge.to ]

                treesForAttachment =
                    TreeUtil.treesWithLabel edge.from treeData.trees

                --|> Debug.log "TREES FOR ATTACHMENT"
                treesWithoutAttachment =
                    List.foldl (\tree_ acc -> List.Extra.remove tree_ acc) treeData.trees treesForAttachment

                --|> Debug.log "TREES WITHOUT ATTACHMENT"
                newTrees =
                    List.map (\tree_ -> TreeUtil.attachAtLabel edge.from newTree tree_) treesForAttachment

                --|> Debug.log "NEW TREES"
            in
            { treeData | count = treeData.count + 1, nodes = nodes, trees = newTrees ++ treesWithoutAttachment }


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


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b


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
