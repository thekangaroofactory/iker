# Read Data

Read Data

## Usage

``` r
read_data(
  path = Sys.getenv("R_IKER_PATH"),
  resource = NULL,
  file,
  delim = ",",
  col_types = NULL,
  verbose = FALSE
)
```

## Arguments

- path:

  A path where to read the data (see details).

- resource:

  An optional resource name (used as sub folder inside path).

- file:

  The name of the file to read (with extension).

- delim:

  Single character used to separate fields within a record.

- col_types:

  See read_delim function from readr package.

- verbose:

  A logical whether additional trace should be sent to the console.

## Value

A tibble, output value of the read_delim() call or NULL if data can't be
read.

## Details

By default, path takes its value from the `R_IKER_PATH` environment
variable. If it's not set in the environment and no value has been
provided, function will be stopped (and return NULL). When NULL, it will
be ignored (assuming file contains the full path to the file)

col_types will be used to detect if POSIXct columns are expected. In
this case they will be read as character first (expecting an ISO-8601
compatible string) then converted to POSIXct.

## Examples

``` r
if (FALSE) { # \dontrun{
foo <- read_data(file = "my_data.csv")
} # }
```
