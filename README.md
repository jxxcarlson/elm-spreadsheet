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


type Formula
    = OpSymbol String


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
    [["100.0","120.0","140.0"],["1.1","1.4","0.9"],["*","*","*"]]
```

Here is what the `parse` function yields:

```elm
    > Spreadsheet.parse textSheet
    [[Right (Real 100),Right (Real 120),Right (Real 140)]
    ,[Right (Real 1.1),Right (Real 1.4),Right (Real 0.9)]
    ,[Left (OpSymbol "*"),Left (OpSymbol "*"),Left (OpSymbol "*")]]
  
```

The function `computeReal 2 0 1` parses the input spreadsheet
, performs the computations specified in column 2 on 
columns 0 and 1, producing a new `Spreadsheet`, which 
is then rendered back into a `TextSpreadsheet.`

```elm
    > textSheet |> computeReal 2 0 1
    [["100","120","140"],["1.1","1.4","0.9"],["110.00000000000001","168","126"]]
```


