# elm-spreadsheet

This is an experimental spreadsheet package. The idea is to
be able to 

#### Example

Consider the spreadsheet data given below in pretty-printed form
and represented internally by `Data.text: List (List String)`:

```
  100.0             1.1       row * 0 1
  120.0             1.4       row * 0 1
  140.0             0.9       row * 0 1
      -     col sum 0 2     col sum 0 2
```

The cells of the spreadsheet contain either data or formulas,
represented by the type 

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

The computation

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
At the moment, the number of implemented formulas is quite small,
and the evaluation strategy is weak.  To make this remark precise,
call a spreadsheet _fully evaluated_ if all of its cells are 
of type `Right Value`.  The current strategy is to evaluate 
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

## Log

### 4/6/2021

Below is the endpoint for today's work. In module
`Spreadsheet` there are two data structures,

```elm
type alias Spreadsheet =
    List (List Cell)
```

and
```elm
type alias TextSpreadsheet =
    List (List String)
```

where the `Cell` type is defined as in the introduction.

The `Spreadsheet` type is like an AST for `TextSpreadsheet`,
and indeed we have a function 

```elm
parse : TextSpreadSheet -> SpreadSheet
```

with counterpart

```elm
render : Spreadsheet -> TextSpreadsheet
```

A typical text spreadsheet will have both columns
of data and columns of operations on cells, as in 
this example:

```elm
    > textSheet
    [["100.0","120.0","140.0"],["1.1","1.4","0.9"],["* 0 1","* 0 1","* 0 1"]]
```
Column 2 says: compute a new column 2 by taking cell-wise projects
of the contents of columns 0 and 1.

Here is what the `parse` function yields:

```elm
    > Spreadsheet.parse textSheet
    [[Right (Real 100),Right (Real 120),Right (Real 140)]
    ,[Right (Real 1.1),Right (Real 1.4),Right (Real 0.9)]
    ,[Left (ColumnOp "*" 0 1),Left (ColumnOp "*" 0 1),Left (ColumnOp "*" 0 1)]]
```

Computation of new `TextSpreadsheet` is carried out by the function

```elm
compute : TextSpreadsheet -> TextSpreadsheet
compute text =
    text
        |> parse
        |> eval
        |> render
```

The middle function has type

```elm
eval : Spreadsheet -> Spreadsheet
```

Its effect is to carry out the computations in column k, first with 
k = 0, then k = 1, and so on.  For this computational rule to 
result in complete evaluaton, where all cells contain 
values instead of formulas, computations in column k should 
involve columns  i, j for i <  k and j < k.  


### 4/7/2021

Introduc the type of formulas mentioned in the introduction:

```elm
type Formula
    = RowOp String Col Col
    | ColOp String Row Row
```

Then re-work `eval` as follows:

```elm
eval : Spreadsheet -> Spreadsheet
eval sheet =
    sheet |> evalRowOps |> evalColOps
```




## References

- [Fundamentals of Spreadsheet Languages](https://www.10studio.tech/docs/fundamentals)

- [Graph Traversals (Cornell)](https://www.cs.cornell.edu/courses/cs2112/2012sp/lectures/lec24/lec24-12sp.html)


   