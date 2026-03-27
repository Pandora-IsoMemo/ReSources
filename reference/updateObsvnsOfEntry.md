# Remove Obsvn From Entry

Removes all list entries named "name" from the element "entry".

## Usage

``` r
updateObsvnsOfEntry(values, entry, name, updateFun)
```

## Arguments

- values:

  (list) list containing all input data (all input tables)

- entry:

  (character) one of c("source", "sourceUncert", "sourceOffset",
  "sourceOffsetUncert", "sourceCovariance", "concentration",
  "concentrationUncert", "concentrationCovariance")

- name:

  (character) name of the target to be removed

- updateFun:

  name of function that updates the lists, either deleteTableFromList or
  updateListNames
