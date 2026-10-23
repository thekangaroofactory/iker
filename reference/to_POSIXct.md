# Convert ISO-8601 Character Columns to POSIXct

Convert ISO-8601 Character Columns to POSIXct

## Usage

``` r
to_POSIXct(data, names_ct)
```

## Arguments

- data:

  a data.frame

- names_ct:

  the names of the columns to convert

## Value

a data.frame with converted columns

## Examples

``` r
to_POSIXct(data.frame(a = "2025-05-13T12:15:30+0200"), "a")
#>                     a
#> 1 2025-05-13 10:15:30
```
