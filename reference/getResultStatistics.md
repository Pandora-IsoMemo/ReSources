# Analyse MCMC results

Get results statistics from ReSources model object

## Usage

``` r
getResultStatistics(
  parameters,
  userEstimates,
  fruitsObj,
  statistics = NULL,
  DT = TRUE,
  agg = TRUE,
  bins = FALSE
)
```

## Arguments

- parameters:

  parameters object of ReSources model object

- userEstimates:

  userEstimateSamples object of ReSources model object

- fruitsObj:

  object of class fruits: input data

- statistics:

  character vector only if agg = TRUE, compute aggregate statistics, use
  R functions such as mean or sd

- DT:

  boolean export as data table

- agg:

  boolean aggregate data by statistcs

- bins:

  boolean quantile bins 0-100%
