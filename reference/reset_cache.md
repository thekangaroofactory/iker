# Reset Cache

Clear all files from cache folder.

## Usage

``` r
reset_cache(path = Sys.getenv("R_IKER_CACHE_PATH"), force = FALSE)
```

## Arguments

- path:

  the path to the cache folder.

- force:

  a logical, if confirmation should be skipped.

## Value

NULL or a character string when folder does not exist.

## Details

When a single location is used to store the cache content, the
"R_IKER_CACHE_PATH" is recommended to define `path`.

## Examples

``` r
if (FALSE) { # \dontrun{
reset_cache(path = "./cache")
} # }
```
