# Create Oxcal Text

Create Oxcal Text

## Usage

``` r
createOxCalText(
  model,
  basicCode,
  terrestrialCurve,
  aquaticCurve1,
  aquaticCurve2,
  OxCalA,
  OxCalB,
  bins,
  coordinates
)
```

## Arguments

- model:

  output of the model

- basicCode:

  (character) basic text read from external source

- terrestrialCurve:

  (character) basic text for the terrestrial curve read from external
  source

- aquaticCurve1:

  (list) header and option text for the aquatic curve 1 read from
  external source

- aquaticCurve2:

  (list) header and option text for the aquatic curve 2 read from
  external source

- OxCalA:

  (character) parameter estimate for aquatic curve 1

- OxCalB:

  (character) parameter estimate for aquatic curve 2

- bins:

  (character) either "meansd" for the usage of mean and sd, or "bins"
  for the usage of pdf for the selected parameter estimate(s)

- coordinates:

  (data.frame) containing the radiocarbon values (mean+SD) for each
  target
