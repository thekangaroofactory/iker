# Supabase API

Helpers to communicate with the Supabase API.

## Usage

``` r
supabase_select(
  table = Sys.getenv("SUPABASE_TABLE"),
  query = NULL,
  url = Sys.getenv("SUPABASE_API_URL"),
  api_key = Sys.getenv("SUPABASE_API_KEY"),
  verbose = FALSE
)

supabase_upsert(
  table = Sys.getenv("SUPABASE_TABLE"),
  data,
  url = Sys.getenv("SUPABASE_API_URL"),
  api_key = Sys.getenv("SUPABASE_API_KEY"),
  verbose = FALSE
)

supabase_delete(
  table = Sys.getenv("SUPABASE_TABLE"),
  key = "id",
  values,
  url = Sys.getenv("SUPABASE_API_URL"),
  api_key = Sys.getenv("SUPABASE_API_KEY"),
  verbose = FALSE
)
```

## Arguments

- table:

  the name of the database table.

- query:

  an optional character string (for select).

- url:

  the Supabase API url.

- api_key:

  the Supabase API key.

- verbose:

  if logs should be printed.

- data:

  a data.frame (for upsert).

- key:

  the label of the key (for delete).

- values:

  the key value(s) to delete.

## Value

a data.frame or NULL (if call fails) for select, TRUE or FALSE for
successful or failing update and delete.

## Details

The following parameters accept an environment variable as default:

- url: "SUPABASE_API_URL"

- api_key: "SUPABASE_API_KEY"

- table: "SUPABASE_TABLE" The two first ones are highly recommended for
  security reasons. The last one (table) is useful only when a single
  table is used.

## Examples

``` r
if (FALSE) { # \dontrun{
# select all rows
supabase_select(table = "foo")

# upsert (assuming foo table has attributes id, value, comment)
supabase_upsert(table = "foo", data = data.frame(id = 1, value = 12, comment = "update"))

# delete row with id=1
supabase_delete(table = "foo", key = "id", values = 1)
} # }
```
