# Create events from changed matrix names

Create events from changed matrix names

## Usage

``` r
createNameEvents(old, new, row, col, update = TRUE)
```

## Arguments

- old:

  old matrix

- new:

  new matrix

- row:

  name of variable for row names to be used in events list

- col:

  name of variable for column names to be used in events list

- update:

  create update events

## Value

list of events of the form list(event, variable, old, new)
