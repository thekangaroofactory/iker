# GitHub API

Helper to get resources from GitHub API.

## Usage

``` r
github(
  resource = c("repos", "milestones", "issues"),
  repo = NULL,
  param = NULL,
  url = Sys.getenv("GITHUB_API_URL"),
  owner = Sys.getenv("GITHUB_OWNER"),
  user_agent = Sys.getenv("GITHUB_USER_AGENT"),
  token = Sys.getenv("GITHUB_API_TOKEN"),
  api_version = c("2026-03-10", "2022-11-28"),
  verbose = FALSE
)
```

## Arguments

- resource:

  the expected resource type ("repos", "milestones" or "issues").

- repo:

  optional, the name of the targeted repository.

- param:

  optional, a named list of query parameters.

- url:

  optional, the url of the GitHub API.

- owner:

  optional, the GitHub owner.

- user_agent:

  optional, the GitHub User-Agent.

- token:

  optional, the GitHub API token.

- api_version:

  optional, the GitHub API version.

- verbose:

  optional, if logs should be printed.

## Value

a data.frame.

## Details

The following optional parameters have corresponding environment
variables to define their values:

- url: "GITHUB_API_URL"

- owner: "GITHUB_OWNER"

- user_agent: "GITHUB_USER_AGENT"

- token: "GITHUB_API_TOKEN"

`repo` is not required when listing authenticated user's repositories.

`param` is used to pass query parameters to the API (see example).

## Examples

``` r
if (FALSE) { # \dontrun{
# list authenticated user's repositories:
github()
github(resource = "repos")

# list repository milestones:
github(resource = "milestones", repo = "foo")

# list repository issues:
github(resource = "issues", repo = "foo")

# list repository closed issues:
github(resource = "issues", repo = "foo", param = list(state = "closed"))

} # }
```
