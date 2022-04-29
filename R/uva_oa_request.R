#' uva_oa_request
#'
#' Retrieve data for works affiliated with UVA institutions from the OpenAlex API.
#'
#' (Details forthcoming.)
#'
#' @param ... Additional arguments to be passed to `oa_request()` (e.g., verbose = `T`/`F`)
#'
#' @return A list containing the data from the OpenAlex API on all works affiliated with UVA institutions
#'
#' @family OpenAlex
#'
#' @examples
#' \dontrun{uva_oa_request()}
#'
#' @export
uva_oa_request <- function(...) {
  uva_institutions <- get_uva_institutions()
  inst_ids <- paste0(uva_institutions$id, collapse = '|')
  query <- paste0('https://api.openalex.org/works?filter=institutions.id:', inst_ids)
  uva_results <- oa_request(query, ...)
  cat('Results retrieved for the following UVA-affiliated institutions: ', paste(uva_institutions$institution, collapse = ','))
  uva_results
}

# Note: May eventually switch this so that institutions to pull works for are based on
# on OpenAlex's new (April 2022) autocomplete search feature:
# https://api.openalex.org/autocomplete/institutions?q=university%20of%20virginia
