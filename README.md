# elm-spreadsheet

This is an experimental spreadsheet package.


## Log

### 4/6/2021

Below is the endpoint for today's work. In module
`Spreadsheet` there are two types of spreadsheet,

```elm
type alias Spreadsheet =
    List (List Cell)
```

and
```elm
type alias TextSpreadsheet =
    List (List String)
```

where the `Cell` type is defined as 

```elm
type alias Cell =
    Either Formula Value

{-|
    Example: ColumnOp "*" 0 1 means multiply the cell in column 0
    by the cell in column 1.
-}
type Formula
    = ColumnOp String Int Int


type Value
    = Integer Int
    | Real Float
    | Boolean Bool
    | String String
    | Undefined
```

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
computeReal : TextSpreadsheet -> TextSpreadsheet
computeRealtext =
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
k = 0, then k = 1, and so on.  For this computational rule to have
the desired result, computations in column k should involve columns
i, j for i <  k and j < k.