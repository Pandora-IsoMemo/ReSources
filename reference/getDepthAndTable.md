# Get Depth And Table

Get the depth of the nested list and the content of the most inner
table.

## Usage

``` r
getDepthAndTable(entryContent, isEntryFun = isDeepestEntry, n = NULL)
```

## Arguments

- entryContent:

  (list) list to look for names

- isEntryFun:

  (function) function that checks for the correct level in the list
  hierarchy

- n:

  (numeric) depth of list to look for names of values
