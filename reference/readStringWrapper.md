# Read String Wrapper

Read and check string from clipboard

## Usage

``` r
readStringWrapper(
  content,
  mode,
  class,
  withRownames = TRUE,
  withColnames = TRUE
)
```

## Arguments

- content:

  (character) string from clipboard

- mode:

  (character) paste mode, one of "auto", "comma-separated",
  "tab-separated", "semicolon"

- class:

  (character) class of content, e.g. "numeric", "character"

- withRownames:

  (logical) should the first column be used as rownames?

- withColnames:

  (logical) should the first row be used as colnames?
