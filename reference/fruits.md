# Constructor for S3 class `fruits`

All arguments are checked for correct class, length, names etc.
Constants and model code are added automatically.

## Usage

``` r
fruits(
  data,
  modelOptions,
  valueNames,
  priors = list(),
  userEstimates = list(list(), list())
)
```

## Arguments

- data:

  list: input data

- modelOptions:

  list: contains elements `burnin` and `iterations`

- valueNames:

  list: contains names for the `targets`, `fractions`, and `sources`

- priors:

  list of characters with prior expressions

- userEstimates:

  list of characters

## Examples

``` r
if (FALSE) { # \dontrun{
# load fruits data:
load(system.file("app/exampleModels/", "fruitsExample.Rdata", package = "ReSources"))
# get Fruits-Object
fruitsObj <- fruits(data, modelOptions, valueNames, priors, userEstimates)
# run model
model <- compileRunModel(fruitsObj)
modelResults <- getResultStatistics(model$parameters, model$userEstimateSamples, fruitsObj,
  DT = FALSE, agg = FALSE
)

# better: use shiny app:
shiny::runApp(paste0(system.file(package = "ReSources"), "/app"))
} # }
```
