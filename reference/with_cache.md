# With Cache

Runs an expression with cache.

## Usage

``` r
with_cache(
  expr,
  path = Sys.getenv("R_IKER_CACHE_PATH"),
  timeout = 8,
  force = FALSE,
  limit = 1,
  cleanup = c("auto", "off"),
  encrypt = TRUE,
  key = Sys.getenv("R_IKER_CACHE_KEY"),
  verbose = FALSE
)
```

## Arguments

- expr:

  an expression to evaluate.

- path:

  the path to the cache folder.

- timeout:

  a numeric (in hours) to indicate when cache content is expired.

- force:

  a logical, if cache content should be updated.

- limit:

  a numeric, the cache size limit (in Mb).

- cleanup:

  a character string or a numeric value (see details).

- encrypt:

  a logical, if cache content should be encrypted.

- key:

  a character string, to be used as a secret.

- verbose:

  a logical, if logs should be printed.

## Value

whatever the evaluation of expr produces.

## Details

Even though the function is designed to work with call_api(), it could
in principle work with any expression.

When a single location is used to store the cache content, the
"R_IKER_CACHE_PATH" is recommended to define `path`.

When `force = TRUE`, the expression will be run even if the cache
content is not expired.

By default `cleanup` is set to "auto". The function will check whether
the files in cache are older than the timeout. Obsolete files will be
deleted. This behavior can be turned off with `cleanup = "off"` or tuned
with a numeric value `cleanup = 24` in hours.

By default, `encrypt` is set to `TRUE` so that cache content is
encrypted. This is because many APIs require a key/token as an
authentication method to access their resources.

Setting `encrypt = FALSE` exposes the content of the cache files. It
should only be used for APIs that do not require an authentication (an
open source geocoding service for example).

When `encrypt = TRUE`, a `key` is mandatory to encode/decode the content
of the cache. The expected value is a character string that does NOT
need to be hashed. This value will be used to generate the actual
encryption key. That being said, this key should not be hard coded or an
easy one to find. The
[`rstring()`](https://thekangaroofactory.github.io/iker/reference/rstring.md)
function can be used to generate one, but the generated value needs to
be saved and passed to the "R_IKER_CACHE_KEY" environment variable. If
the key is lost, the cache content is lost.

## See also

[`rstring()`](https://thekangaroofactory.github.io/iker/reference/rstring.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# set R_IKER_CACHE_PATH environment variable
# set R_IKER_CACHE_KEY environment variable

# basic example:
# first call will evaluate & return the current time
with_cache(Sys.time(), verbose = T)
# next calls will return cache content (for 8 hours)
with_cache(Sys.time(), verbose = T)
} # }
```
