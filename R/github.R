
#' GitHub API
#'
#' @description
#' Helper to get resources from GitHub API.
#'
#' @param resource the expected resource type ("repos", "milestones" or "issues").
#' @param repo optional, the name of the targeted repository.
#' @param param optional, a named list of query parameters.
#' @param url optional, the url of the GitHub API.
#' @param owner optional, the GitHub owner.
#' @param user_agent optional, the GitHub User-Agent.
#' @param token optional, the GitHub API token.
#' @param api_version optional, the GitHub API version.
#' @param verbose optional, if logs should be printed.
#'
#' @details
#' The following optional parameters have corresponding environment variables
#' to define their values:
#' - url: "GITHUB_API_URL"
#' - owner: "GITHUB_OWNER"
#' - user_agent: "GITHUB_USER_AGENT"
#' - token: "GITHUB_API_TOKEN"
#'
#' `repo` is not required when listing authenticated user's repositories.
#'
#' `param` is used to pass query parameters to the API (see example).
#'
#' @returns a data.frame.
#' @export
#'
#' @examples
#' \dontrun{
#' # list authenticated user's repositories:
#' github()
#' github(resource = "repos")
#'
#' # list repository milestones:
#' github(resource = "milestones", repo = "foo")
#'
#' # list repository issues:
#' github(resource = "issues", repo = "foo")
#'
#' # list repository closed issues:
#' github(resource = "issues", repo = "foo", param = list(state = "closed"))
#'
#' }

github <- function(resource = c("repos", "milestones", "issues"), repo = NULL, param = NULL,
                   url = Sys.getenv("GITHUB_API_URL"), owner = Sys.getenv("GITHUB_OWNER"),
                   user_agent = Sys.getenv("GITHUB_USER_AGENT"), token = Sys.getenv("GITHUB_API_TOKEN"),
                   api_version = c("2026-03-10", "2022-11-28"), verbose = FALSE){

  # check params
  stopifnot("url is empty" = nchar(url) > 0)
  stopifnot("token is empty" = nchar(token) > 0)
  stopifnot("user_agent is empty" = nchar(user_agent) > 0)
  api_version <- match.arg(api_version)
  resource <-  match.arg(resource)


  # ////////////////////////////////////////////////////////////////////////////
  # URL

  # compute url based on resource
  # milestones & issues have similar path
  if(resource == "repos"){

    # specific repo vs authenticated user's repos
    if(is.null(repo)){

      # List authenticated user's repositories
      # user/repos
      url <- paste(url, "user/repos", sep = "/")

    } else {

      # Get specific repo:
      # repos/OWNER/REPO
      url <- paste(url, "repos", owner, repo, sep = "/")

    }

  } else {

    # List repository milestones/issues
    # repos/OWNER/REPO/milestones
    # repos/OWNER/REPO/issues
    url <- paste(url, "repos", owner, repo, resource, sep = "/")

  }


  # ////////////////////////////////////////////////////////////////////////////
  # Query parameters
  # issues:
  # - milestone
  # - state
  # - since
  # - sort (maybe try updated)

  # prepare (flatten) params
  if(!is.null(param)){
    if(!is.list(param))
      stop("param should be a named list")
    else {
      param <- paste(names(param), param, sep = "=", collapse = "&")
      url <- paste(url, param, sep = "?")}}


  # ////////////////////////////////////////////////////////////////////////////
  # Headers

  # prepare headers
  headers <- list(Accept = "application/vnd.github+json",
                 Authorization = paste("Bearer", token),
                 "User-Agent" = user_agent,
                 "X-GitHub-Api-Version" = api_version)


  # ////////////////////////////////////////////////////////////////////////////
  # Call API

  if(verbose)
    cat("API url =", url, "\n")
  res <- call_api(url, headers, verbose = verbose)


  # ////////////////////////////////////////////////////////////////////////////
  # Response

  # check & return
  if(res$status_code == 200){
    x <- jsonlite::fromJSON(res$content)
    if(verbose)
      cat("Output dim =", dim(x), "\n")
    x
  } else if(res$status_code >= 400) {
    warning("GitHub API call failed, returning NULL!", call. = FALSE)
    if(verbose)
      cat("Details:", jsonlite::fromJSON(res$content)$message, "\n")
    NULL
  }

}
