# Check if class is in a set of accepted classes

Check if class is in a set of accepted classes

## Usage

``` r
checkClass(x, classesExpected = "list", argName = NULL)
```

## Arguments

- x:

  any object

- classesExpected:

  character: vector with accepted classes

- argName:

  character: name of argument displayed in error message

## Examples

``` r
ReSources:::checkClass(c(1, 2, 3), c("numeric", "integer"))
if (FALSE) { # \dontrun{
ReSources:::checkClass("This is not a list", argName = "x")
} # }
```
