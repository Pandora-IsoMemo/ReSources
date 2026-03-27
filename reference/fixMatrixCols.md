# Fix Matrix Cols

Fix columns of a matrix. If fixed colums are specified, they are used as
column names otherwise oldNames are kept.

## Usage

``` r
fixMatrixCols(m, oldNames, fixedCols = FALSE, row, col)
```

## Arguments

- m:

  matrix

- oldNames:

  (character) old column names, e.g. of "meanData()" or
  "covarianceData()"

- fixedCols:

  either FALSE or a character vector of column names to be kept, e.g.
  "Offset", "longitude", "latitude", ...

- row:

  (character) row variable, e.g. "obsvnNames"

- col:

  (character) column variable, e.g. "targetNames"
