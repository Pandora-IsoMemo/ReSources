# Remove Obsvn From Lists

Removes all list entries named "name" from all lists that contain
entries named by observations.

## Usage

``` r
updateObsvnsInLists(values, name, updateFun)
```

## Arguments

- values:

  (list) list containing all input data (all input tables)

- name:

  (character) name of the target to be removed

- updateFun:

  name of function that updates the lists, either deleteTableFromList or
  updateListNames
