module TestFunctions exposing (..)

import Solve
import TestData


edges =
    TestData.ast |> Solve.edgesFromSpreadsheet


trees =
    edges |> Solve.treesFromEdges |> .trees
