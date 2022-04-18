#' doi_oa_request
#'
#' Retrieve data for a work from the OpenAlex API based on a DOI.
#'
#' (Details forthcoming.)
#'
#' @param doi A DOI
#' @param ... Additional arguments to be passed to `oa_request()` (e.g., verbose = `T`/`F`)
#'
#' @return A list containing the API call results
#'
#' @family OpenAlex
#'
#' @examples
#' # https://doi.org/10.4324/9780203873366 and doi:10.4324/9780203873366 work as well
#' doi <- '10.4324/9780203873366'
#' results <- doi_oa_request(doi)
#'
#' @export
doi_oa_request <- function(doi, ...) {
  stopifnot(length(doi) == 1)
  # All DOIs start with "10.": https://www.doi.org/doi_handbook/2_Numbering.html#2.2.2
  if (stringi::stri_detect(doi, regex = '^10\\.') == F) {doi <- stringi::stri_extract(doi, regex = '10\\..*')}
  # Build query (OpenAlex DOI search query looks like: https://api.openalex.org/works/doi:https://doi.org/10.1016/j.joi.2017.08.007)
  doi_query <- paste0('https://api.openalex.org/works/doi:', doi)
  oa_request(doi_query, ...)
}
