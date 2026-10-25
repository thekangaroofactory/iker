# Write Data

Write Data

## Usage

``` r
write_data(
  x,
  path = Sys.getenv("R_IKER_PATH"),
  resource = NULL,
  file,
  delim = ",",
  verbose = FALSE
)
```

## Arguments

- x:

  A data frame to write to disk.

- path:

  A path where to write the data (see details).

- resource:

  An optional resource name (used as sub folder inside path).

- file:

  The name of the file to write (with extension)

- delim:

  Single character used to separate fields within a record.

- verbose:

  If TRUE, additional messages will be sent to the console

## Value

returns the input x invisibly, or FALSE when it fails

## Details

By default, path takes its value from the `R_IKER_PATH` environment
variable. If it's not set in the environment and no value has been
provided, function will be stopped (and return FALSE). If the
corresponding path does not exist, it won't be created - to avoid
creating data inside a container if the mount has not been done for
example. When NULL, no check will be done.

Resource is an optional parameter to allow the use of sub folder inside
path. If not NULL, it will be concatenated to path (and created inside
path if it does not exist) When path is NULL, it will be ignored.

Columns with POSIXct type will be converted to ISO-8601 character
columns before writing the file

## Examples

``` r
if (FALSE) { # \dontrun{
write_data(foo, file = "my_data.csv")
} # }
```
