# Update Targets In Lists

Update all list entries named "name" from all lists that contain entries
named by targets.

## Usage

``` r
updateTargetsInLists(values, name, updateFun)
```

## Arguments

- values:

  (list) list containing all input data (all input tables)

- name:

  (character) name of the target to be removed (only )

- updateFun:

  name of function that updates the lists, either deleteTableFromList or
  updateListNames
