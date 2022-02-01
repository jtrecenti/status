# Library ----
#library(gh)
#library(memoise)
#library(purrr)
#library(dplyr)
#library(tidyr)
#library(glue)
#library(readr)


# gh functions ----------------------------------------------------------------------------------------------------------------------------------

#' gh_workflows
#'
#' Function to create a memorized copy of the repository cache.
#'
#' @param owner repository's owner
#' @param repo repository's name
#' @param ... others paramaters
#'
#' @export
gh_workflows <- memoise::memoise(function(owner, repo, ...) {
  gh::gh("/repos/{owner}/{repo}/actions/workflows", owner = owner, repo = repo, private = TRUE) %>%
    .$workflows
}, cache =  memoise::cache_memory())

#' gh_runs
#'
#' Function to create a memorized copy of the repository wokflow id.
#'
#' @param owner repository's owner
#' @param repo repository's name
#' @param workflow_id workflow id
#' @param ... others paramaters
#'
#' @export
gh_runs <- memoise::memoise(function(owner, repo, workflow_id, ...) {
  gh::gh(
    "/repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs",
    owner = owner,
    repo = repo,
    workflow_id = workflow_id,
    per_page = 1,
    private = TRUE
  )$workflow_runs[[1]]
}, cache = memoise::cache_memory())

#' gh_url
#'
#' Function to create a memorized copy of the repository url.
#'
#' @param url repository's url
#'
#' @export
gh_url <- memoise::memoise(function(url) {
  gh::gh(url)
}, cache = memoise::cache_memory())


# get repos -------------------------------------------------------------------------------------------------------------------------------------

#' gh_url
#'
#' Function that creates csv with status repositories.
#'
#' @param repos list of repositories
#'
#' @export
gh_get_repo_status <- function(repos) {
  repos <- purrr::map_dfr(repos, ~ tibble::tibble(repo = .), .id = "owner")

  repos$workflows <- repos %>% purrr::pmap(gh_workflows)
  repos <- repos %>%
    tidyr::unnest(workflows) %>%
    dplyr::mutate(
      workflow_id = purrr::map_chr(workflows, "id"),
      badge_url = purrr::map_chr(workflows, "badge_url")
    )

  repos$runs <- purrr::pmap(repos, gh_runs)
  repos <- repos %>%
    dplyr::mutate(
      html_url_run = purrr::map_chr(runs, "html_url"),
      run_conclusion = purrr::map_chr(runs, "conclusion"),
      commit_message = purrr::map_chr(runs, ~ .x$head_commit$message),
      commit_id = purrr::map_chr(runs, `[[`, c("head_commit", "id")),
      repo_name = purrr::map_chr(runs, `[[`, c("head_repository", "full_name")),
      html_url_repo = purrr::map_chr(runs, `[[`, c("head_repository", "html_url")),
      .repo = purrr::map(purrr::map_chr(runs, `[[`, c("head_repository", "url")), gh_url),
      stargazers_count = purrr::map_dbl(.repo, "stargazers_count"),
      watchers_count = purrr::map_dbl(.repo, "watchers_count"),
      open_issues_count = purrr::map_dbl(.repo, "open_issues_count"),
      forks_count = purrr::map_dbl(.repo, "forks_count"),
      open_issues_count = purrr::map_dbl(.repo, "open_issues_count")
    )

  repos %>%
    dplyr::select(-where(is.list)) %>%
    dplyr::arrange(repo_name) %>%
    readr::write_csv("repos.csv")

  repos
}
