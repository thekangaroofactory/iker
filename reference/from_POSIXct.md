# Convert POSIXct Columns to ISO-8601 Character

Convert POSIXct Columns to ISO-8601 Character

## Usage

``` r
from_POSIXct(data)
```

## Arguments

- data:

  a data.frame (expected to contain POSIXct columns)

## Value

a data.frame, whose POSIXct columns have been converted to ISO-8601
character columns

## Examples

``` r
from_POSIXct(data.frame(a = Sys.time()))
#>                          a
#> 1 2026-10-24T16:08:08+0000
```
