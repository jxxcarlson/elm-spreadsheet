module TreeUtil exposing (..)

import Tree exposing (Tree(..), singleton, tree)
import Tree.Zipper as Zipper exposing (Zipper)


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
