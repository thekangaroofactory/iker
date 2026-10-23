# Save Data To Connector

Save Data To Connector

## Usage

``` r
save_data(..., verbose = FALSE)
```

## Arguments

- ...:

  arguments to pass to the connector function (see details).

- verbose:

  a logical (default = FALSE) to indicate if additional traces should be
  sent to the console.

## Value

the output of the connector function.

## Details

The function is a wrapper around connector functions. It offers a single
point of access to the package data persistence features.

Arguments passed to `...` depend on the desired connector: Vignettes are
available to describe the connectors.

## See also

\[write_data()\]\[supabase_upsert()\]\[supabase_delete()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# -- file connector
save_data(file = "my_data.csv", path = getwd())
} # }
```
