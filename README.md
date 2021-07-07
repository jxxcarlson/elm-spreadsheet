# Elm-Spreadsheet

Elm-spreadsheet provides the means to represent and compute with
spreadsheets. A spreadsheet is a two-dimensional array
of cells, each of which may contain either data (values) or formulas.
To evaluate a spreadsheet is to apply the formulas to create
a new spreadsheet.  If the new spreadsheet contains no formulas,
we say that it is _fully evaluated._  The `eval` function calls itself
repeatedly so as to attempt full evaluation.

While this library operates on the level of text, it is quite possible
to use it as the engine for a GUI spreadsheet app. 

 - Use a
`parse: String -> Cell` to map your representation of the spreadsheet
to the internal `Spreadsheet` representation of this library.  That is,
create your own function `read: YourSpreadsheetRepresentation -> Spreadsheet`.
   
- Use the library function `eval: Spreadsheet -> Spreadsheet` to 
evaluate the sheet.
  
- Write a function `render : Spreadsheet -> YourSpreadsheetRepresentation`
to get back to your spreadsheet representation.

## Representation

A Spreadsheet is defined internally as an `Array2D Cell`. 
Externally, it can be represented as a string, with cells separated by semicolons.  Cells
are referred to in the same way as in Excel.  The external represenation can take various
forms, as illustrated in the two figures below:
    
    -- 'Plain' representation
    ss1 = """
    100;     1.1;       mul A1,A2
    120;     1.4;       mul B1,B2
    140;     0.8;       mul C1,C2
    -;    add A2:C2;    add A3:C3
    """

    -- Excel representation
    ss2 = """
    100;     1.1;      =A1*A2
    120;     1.4;      =B1*B2
    140;     0.8;      =C1*C2
    -;   =sum(A2:C2);  =sum(A3:C3)
    """

The entry `mul A1,A2` means multiply the contents of cell A1 by the
contents of cell A2. The entry `add A2:C2` means add the entries in column 2 between rows A and C.


## Read, eval, and print

Use 

```elm
   read: (String -> Cell) - String -> Spreadsheet
``` 

to compute the internal representation as `Array2D Cell`, as in these examples:

```elm
    > s1 = read CellParser.parse ss1
    > s2 = read CellParserExcel.parse ss2
```


use 

```elm
   eval : Spreadsheet -> Spreadsheet
``` 

to evaluate the formulas in a spreadsheet, and use 

```elm
   print : Spreadsheet -> String
``` 

to render the result back into text:


```elm
   > ss2 |> read CellParserExcel.parse |> eval |> print
  "100; 1.1; 110\n120; 1.4; 168\n140; 0.9; 126\n-; 3.4; 404"
```

Thus the result is the fully evaluated spreadsheet


    100; 1.1; 110
    120; 1.4; 168
    140; 0.9; 126
    -;   3.4; 404


## Operations

Currently the operations implemented are limited:

    add A1,H4   -- cellwise addition
    mul A1,H4   -- cellwise multiplication
    sub A1,H4   -- cellwise subtraction
    div A1,H4   -- cellwise division

    add B2:B9   -- add the elements of row B from columns 2 through 9
    add B2:Q2   -- add the elements of column 2 from rows B through Q

and their Excel counterparts:

    =A1+H4   -- cellwise addition
    =A1*H4   -- cellwise multiplication
    =A1-H4   -- cellwise subtraction
    =A1/H4   -- cellwise division

    =sum(B2:B9)  -- add the elements of row B from columns 2 through 9
    =sum(B2:Q2)  -- add the elements of column 2 from rows B through Q 

More operations later.

