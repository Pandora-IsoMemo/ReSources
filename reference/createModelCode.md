# Create model code

Create model code

## Usage

``` r
createModelCode(
  priors,
  userEstimates,
  valueNames,
  constants,
  individualNames,
  modelOptions,
  covariates,
  termsSources,
  termsTargets,
  covariatesNum = NULL
)
```

## Arguments

- priors, :

  character vector of priors from shiny interface

- userEstimates, :

  character vector of priors from shiny interface

- valueNames, :

  list of value names

- constants, :

  list of constants

- individualNames, :

  names of the individuals

- modelOptions, :

  list of model options

- covariates, :

  character matrix of categorical covariates

- termsSources, :

  list of sourceTerms

- termsTargets, :

  list of termsTargets

- covariatesNum, :

  numeric atrix of numeric covariates

## Value

Nimble code to be used in
[`nimbleModel`](https://rdrr.io/pkg/nimble/man/nimbleModel.html)
