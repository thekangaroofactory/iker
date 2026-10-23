# Call API

High level implementation of 'curl' fetch to memory.

## Usage

``` r
call_api(url, header, options = NULL, raw = FALSE, verbose = FALSE)
```

## Arguments

- url:

  character, the API url.

- header:

  a list of character headers.

- options:

  optional list (see details).

- raw:

  a logical (default FALSE) if the raw content should be returned.

- verbose:

  a logical (default FALSE) if logs should be printed to the console.

## Value

a list (the full response).

## Details

`url` must start with "https://".

The values in `header` must be character strings (see examples).

The list of options can be accessed with
[`curl::curl_options()`](https://jeroen.r-universe.dev/curl/reference/curl_options.html).

When `raw` is FALSE, the content (& headers) of the response will be
converted into character strings.

## Examples

``` r
if (FALSE) { # \dontrun{
call_api(url = "https://some/url", header = list(Accept = "application/json"))
} # }
```
