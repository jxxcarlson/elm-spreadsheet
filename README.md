# Elm-Spreadsheet

Elm-spreadsheet provides the means to represent and compute with
spreadsheets in Elm. A spreadsheet is a two-dimensional array
of cells, each of which may contain either data (values) or formulas.
To evaluate a spreadsheet is to apply the formulas to create
a new spreadsheet.  If the new spreadsheet contains no formulas,
we say that it is _fully evaluated._

## Representation

Spreadsheets can be represented in a variety of ways.

1. As a `String`
    
    ```
      100.0             1.1       row * 1 2
      120.0             1.4       row * 1 2
      140.0             0.9       row * 1 2
          -     col sum 1 3     col sum 1 3
    ```
   
    The entry `row * 1 2` in row `r`, column `3` means "add the cell contents at `(r, 1)`
    to those at `(r, 2)` and put the result in the cell at `(r,3)`."  At the
    text level, we use "human numbering," with indices starting at 1, not 0.
    
   
2. As a `List (List String)`
    
    ```elm
    > TestData.text
    [
         ["100.0","120.0","140.0","-"]
        ,["1.1","1.4","0.9","col sum 1 3"]
        ,["row * 1 2","row * 1 2,"row * 1 2,"col sum 1 3]
    ]
    ```

3. As a `List Cell`

    ```elm
    > parse TestData.text
    [ [Right (Real 100),Right (Real 120),Right (Real 140),Right Undefined]
     ,[Right (Real 1.1),Right (Real 1.4),Right (Real 0.9),Left (ColOp "sum" 1 3)]
     ,[Left (RowOp "*" 1 2),Left (RowOp "*" 1 2),Left (RowOp "*" 1 2),Left (ColOp "sum" 1 3)]
    ]
    ```
   
    where

    ```elm
    type alias Cell =
        Either Formula Value
        
    type Formula
        = RowOp String Col Col
        | ColOp String Row Row
        
    type Value
        = Integer Int
        | Real Float
        | Boolean Bool
        | String String
        | Undefined
    ```
    One can convert this format back into a text represention: 
   
    ```elm
    render : List Cell -> List (List String)
    render sheet =
        List.map renderColumn sheet
    ```
   
## Evaluation

The function `eval` carries out the computations specified in the
formula cells.  Thus the sequence


```elm
  TestData.text |> parse |> eval |> render |> Pretty.print
```

yields the data

```
  100    1.1    110.0
  120    1.4    168.0
  140    0.9    126.0
    -    3.4    404.0
```

The `eval`
function always produces a transformed spreadsheet. When fully
evaluated, all cells are of type`Right Value`.  In this early version
of the package, not all valid spreadsheets are fully evaluated.


### Remarks on `eval`

The current strategy is to evaluate
row formulas from left to right and then evaluate column formulas
from top to bottom.  This strategy always succeeds in that it
yields a spreadsheet.  However, the result is fully evaluated
only if certain restrictions hold:

1. A row operation in a cell at (row, col) must refer only to
   columns i, j where i < col, j < col. Moreover, the cells
   at (row, i) and (row, j) must contain values.

2. A column operation in a cell at (row, col) must refer only
   to rows i, j where i < row and j < row.  Moreover after
   all row operations have been carried out as in (1), the
   cells referred to must contain values.

To lift these restrictions, we must "solve" the spreadsheet
by finding an order in which to evaluate the operations which
results in a fully evaluated spreadsheet.  To do this,
consider the dependencies of the operations.  If an operation
in cell A, which we label as "op A", depends on an operation in
cell B, then we say that `op B` is a child of `op A`:

```bash
op A -> op B
```

Thus the operations constitute the nodes of a graph in which the
edges represent dependencies.  A depth-first traversal of the
graph, assuming no cyclic dependencies, will result in
full evaluation.

Thus it remains to (a) derive dependency graphs from
spreadsheets, (b) traverse the graph depth-first, performing the
indicated computations as each node is visited.

## Plans

1. Implement a depth-first evaluation strategy

2. Detect cyclic dependencies

3. Add formulas


