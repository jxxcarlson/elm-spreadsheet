# Elm-spreadsheet

We present an experimental spreadsheet package.  There is nothing original here,
but when finished, or at least more fully developed, it should be useful.

Spreadsheets
can be represented in various ways — as plain text, in CSV
format, as a `List (List String)`, and as `Spreadsheet = List (List Cell)`.
The last representation is a kind of AST for spreadsheets 
for we implement a function

```elm
eval : Spreadsheet -> Spreadsheet
```

which "solves" or "computes" the spreadsheet. The cells of the spreadsheet 
contain either data or formulas, a fact which is declared by the type

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

### Example

Consider the spreadsheet data given below in pretty-printed form
and represented internally by `Data.text: List (List String)`:

```
  100.0             1.1       row * 0 1
  120.0             1.4       row * 0 1
  140.0             0.9       row * 0 1
      -     col sum 0 2     col sum 0 2
```


The AST of this spreadsheet is 

```elm
> parse Data.text
[ [Right (Real 100),Right (Real 120),Right (Real 140),Right Undefined]
, [Right (Real 1.1),Right (Real 1.4),Right (Real 0.9), Left (ColOp "sum" 0 2)]
, [Left (RowOp "*" 0 1),Left (RowOp "*" 0 1),Left (RowOp "*" 0 1),Left (ColOp "sum" 0 2)]
]
```
and the computation

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



## References

- [Fundamentals of Spreadsheet Languages](https://www.10studio.tech/docs/fundamentals)

- [Graph Traversals (Cornell)](https://www.cs.cornell.edu/courses/cs2112/2012sp/lectures/lec24/lec24-12sp.html)


   