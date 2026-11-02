
#' Changelog
#'
#' @description
#' Generate changelog from GitHub milestone.
#'
#' @param repo the name of the GitHub repo.
#' @param milestone the milestone number.
#' @param verbose a logical if more logs should be written to the console.
#'
#' @details
#' The following environment variables should be defined:
#' - GITHUB_API_URL
#' - GITHUB_OWNER
#' - GITHUB_USER_AGENT
#' - GITHUB_API_TOKEN
#'
#' @seealso [github()]
#'
#' @returns NULL (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' gh_changelog(repo = "foo", milestone = 1)
#' }

gh_changelog <- function(repo, milestone, verbose = FALSE){

  # call GitHub API
  cat("Retrieving data from GitHub... \n")
  res <- github(resource = "issues",
                repo = repo,
                param = list("milestone" = milestone,
                             "state" = "all"),
                verbose = verbose)
  if(is.null(res)){
    warning("Failed to retrieve data from GitHub!")
    return(invisible(NULL))}

  # filter PR
  if("pull_request" %in% names(res))
    res <- res[is.na(res$pull_request$url), ]

  # extract useful info
  change_log <- res[c("number", "title")]
  change_log$label <- unlist(lapply(res$labels, `[[`, "name"))

  # helper
  print_section <- function(section){

    cat(paste0("\n## ", ktools::toupperfirst(unique(section$label))), "\n\n")
    apply(section, MARGIN = 1, \(x) cat("-", x['title'], paste0("#", x['number']), "\n"))

  }

  # apply helper
  message("Changelog for repo = ", repo, " miletsone = ", milestone, " :")
  lapply(unique(change_log$label), \(x) print_section(change_log[change_log$label == x, ]))

  # return
  return(invisible(NULL))

}
