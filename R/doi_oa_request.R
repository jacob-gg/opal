#' doi_oa_request
#'
#' Retrieve data from the OpenAlex API based on a DOI
#'
#' (Details forthcoming.)
#'
#' @param doi A DOI
#'
#' @return A list containing the API call results
#'
#' @export
doi_oa_request <- function(doi) {
  if (stringi::stri_detect(doi, regex = '^https?://doi\\.org/') == F) {
    doi <- paste0('https://doi.org/', doi)
  }

  # Build query (OpenAlex DOI search query looks like: https://api.openalex.org/works/doi:https://doi.org/10.1016/j.joi.2017.08.007)
  doi_query <- paste0('https://api.openalex.org/works/doi:', doi)

  cat('DOI query link: ', doi_query, '\n')
  oa_request(doi_query)
}
