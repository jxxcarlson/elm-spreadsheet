module TreeUtil exposing
    ( attach
    , attachAtLabel
    , commonLabels
    , commonLabelsOfTrees
    , count
    , depth
    , getLeaves
    , hasLabel
    , hasLabelIn
    , height
    , isSingleton
    , treesWithLabel
    )

import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Tree exposing (Tree(..), singleton, tree)
import Tree.Zipper as Zipper exposing (Zipper)


count : Tree a -> Int
count t =
    let
        c =
            Tree.children t
    in
    if c == [] then
        1

    else
        1 + List.sum (List.map count c)



-- LEAVES


hasLabel : a -> Tree a -> Bool
hasLabel a tree =
    List.member a (Tree.flatten tree)


hasLabelIn : List a -> Tree a -> Bool
hasLabelIn labels tree =
    let
        labelsOfTree =
            Tree.flatten tree
    in
    List.any (\a -> List.member a labelsOfTree) labels


isSingleton : Tree a -> Bool
isSingleton tree =
    Tree.children tree == []


commonLabelsOfTrees : Tree comparable -> Tree comparable -> List comparable
commonLabelsOfTrees tree1 tree2 =
    let
        a =
            Set.fromList (Tree.flatten tree1)

        b =
            Set.fromList (Tree.flatten tree2)
    in
    Set.intersect a b |> Set.toList


commonLabels : List comparable -> Tree comparable -> List comparable
commonLabels labels tree =
    let
        a =
            Set.fromList labels

        b =
            Set.fromList (Tree.flatten tree)
    in
    Set.intersect a b |> Set.toList



-- LEAVES


getLeaves : Tree a -> List (Tree a)
getLeaves tree =
    loop { leaves = [], trees = [ tree ] } updateLeaves


type alias LeafState a =
    { leaves : List (Tree a), trees : List (Tree a) }


updateLeaves : LeafState a -> Step (LeafState a) (List (Tree a))
updateLeaves state =
    if state.trees == [] then
        Done state.leaves

    else
        case List.Extra.uncons state.trees of
            Nothing ->
                Done state.leaves

            Just ( tree, remaining ) ->
                let
                    { leaves, trees } =
                        someLeaves tree
                in
                Loop { leaves = leaves ++ state.leaves, trees = trees ++ remaining }


someLeaves : Tree a -> { leaves : List (Tree a), trees : List (Tree a) }
someLeaves tree =
    let
        c =
            Tree.children tree

        a =
            List.filter isSingleton c

        b =
            List.filter (\t -> not (isSingleton t)) c
    in
    { leaves = a, trees = b }



-- leaves : Tree a -> List (Tree a)


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



-- TREES WITH LABEL


treesWithLabel : a -> List (Tree a) -> List (Tree a)
treesWithLabel a trees =
    List.map (treeWithLabel a) trees |> Maybe.Extra.values


treeWithLabel : a -> Tree a -> Maybe (Tree a)
treeWithLabel a list =
    Zipper.findFromRoot (\label -> label == a) (Zipper.fromTree list) |> Maybe.map Zipper.toTree


attachAtLabel : a -> Tree a -> Tree a -> Tree a
attachAtLabel a tree targetTree =
    let
        childrenOfTree : List (Tree a)
        childrenOfTree =
            Tree.children tree
    in
    case Zipper.fromTree targetTree |> setFocus a of
        Nothing ->
            targetTree

        Just zipper ->
            let
                childrenAtFocus : List (Tree a)
                childrenAtFocus =
                    Tree.children (Zipper.tree zipper)

                allChildren =
                    childrenOfTree ++ childrenAtFocus

                subtree : Tree a
                subtree =
                    Tree.tree a allChildren
            in
            Zipper.replaceTree subtree zipper |> Zipper.toTree


attach : Tree a -> Tree a -> Tree a
attach tree targetTree =
    let
        a =
            Tree.label tree

        childrenOfTree : List (Tree a)
        childrenOfTree =
            Tree.children tree
    in
    case Zipper.fromTree targetTree |> setFocus a of
        Nothing ->
            targetTree

        Just zipper ->
            let
                childrenAtFocus : List (Tree a)
                childrenAtFocus =
                    Tree.children (Zipper.tree zipper)

                allChildren =
                    childrenOfTree ++ childrenAtFocus

                subtree : Tree a
                subtree =
                    Tree.tree a allChildren
            in
            Zipper.replaceTree subtree zipper |> Zipper.toTree


attach_ : Tree a -> Tree a -> Maybe (Tree a)
attach_ subTree tree =
    let
        a =
            Tree.label subTree

        zipper : Maybe (Zipper a)
        zipper =
            Zipper.fromTree tree |> setFocus a

        children : List (Tree a)
        children =
            Tree.children subTree

        zipper2 : Maybe (Zipper a)
        zipper2 =
            List.foldl (\subtree_ zipper_ -> Maybe.map2 Zipper.append (Just subtree_) zipper_) zipper children
    in
    Maybe.map Zipper.toTree zipper2


depth : a -> Tree a -> Maybe Int
depth a tree =
    case Zipper.fromTree tree |> setFocus a of
        Nothing ->
            Nothing

        Just zipper ->
            Just (depth_ zipper)


depth_ : Zipper a -> Int
depth_ zipper =
    case Zipper.parent zipper of
        Nothing ->
            0

        Just zipper2 ->
            1 + depth_ zipper2


height : Tree a -> Int
height t =
    let
        c =
            Tree.children t
    in
    if c == [] then
        0

    else
        1 + (List.maximum (List.map height c) |> Maybe.withDefault 0)


setFocus : a -> Zipper a -> Maybe (Zipper a)
setFocus node zipper =
    Zipper.findFromRoot (\label -> label == node) zipper
