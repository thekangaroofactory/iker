# Load Data From Connector

Load Data From Connector

## Usage

``` r
load_data(..., verbose = FALSE, data.frame = FALSE, force_empty = FALSE)
```

## Arguments

- ...:

  arguments to pass to the connector function (see details).

- verbose:

  a logical (default = FALSE) to indicate if additional traces should be
  sent to the console.

- data.frame:

  a logical (default = FALSE) if a data.frame should be returned
  (instead of a tibble).

- force_empty:

  a logical (default = FALSE) if NULL should be returned when data has
  no row.

## Value

the output of the connector function.

## Details

The function is a wrapper around connector functions. It offers a single
point of access to the package data persistence features.

Arguments passed to `...` depend on the desired connector. Vignettes are
available to describe the connectors.

## See also

[read_data()](https://thekangaroofactory.github.io/iker/reference/supabase_select.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# -- file connector
load_data(path = getwd(), file = "my_data.csv")
} # }
```
