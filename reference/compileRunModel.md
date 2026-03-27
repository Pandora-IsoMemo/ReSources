# Modeling ReSources

Run model on ReSources object

## Usage

``` r
compileRunModel(
  fruitsObj,
  progress = FALSE,
  onlySim = FALSE,
  userDefinedAlphas = NULL,
  seqSim = 0.2,
  simSourceNames = NULL
)
```

## Arguments

- fruitsObj:

  object of class fruits: input data

- progress:

  boolean: show progress in shiny

- onlySim:

  boolean: only simulate from prior

- userDefinedAlphas:

  list of matrices: for simulation only: food source intakes values

- seqSim:

  numeric grid of mixture steps

- simSourceNames:

  names of sources to simulate
