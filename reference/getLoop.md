# Get Loop

Loop over all targets

## Usage

``` r
getLoop(optionCurve1, optionCurve2, model, bins, OxCalA, OxCalB, coordinates)
```

## Arguments

- optionCurve1:

  (character) option text for the aquatic curve 1 read from external
  source

- optionCurve2:

  (character) option text for the aquatic curve 2 read from external
  source

- model:

  output of the model

- bins:

  (character) either "meansd" for the usage of mean and sd, or "bins"
  for the usage of pdf for the selected parameter estimate(s)

- OxCalA:

  (character) parameter estimate for aquatic curve 1

- OxCalB:

  (character) parameter estimate for aquatic curve 2

- coordinates:

  (data.frame) containing the radiocarbon values (mean+SD) for each
  target
