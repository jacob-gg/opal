#' uva_oa_request
#'
#' Retrieve data for works affiliated with UVA institutions from the OpenAlex API.
#'
#' (Details forthcoming.)
#'
#' @param id_file A string indicating the file name in the data/ folder containing OpenAlex IDs and names of UVA institutions
#' @param all_data_one_list `T/F` indicating whether results for all UVA institutions should be collapsed into a single list or left separated by institution
#'
#' @return A list containing the data from the OpenAlex API on all works affiliated with UVA institutions
#'
#' @family OpenAlex
#'
#' @examples
#' \dontrun{uva_oa_request()}
#'
#' @export
uva_oa_request <- function(id_file = 'uva_institutions.rda', all_data_one_list = T) {
  if (id_file %in% dir('data') == F) {stop(paste0('Ensure that ', id_file, ' is in data/ folder'), call. = F)}
  load('data/uva_institutions.rda')
  if (all(c('id', 'institution') %in% colnames(uva_institutions)) == F) {stop('Variables `id` (OpenAlex ID) and `institution` must be in id_file', call. = F)}
  uva_institutions$temp_query <- paste0('https://api.openalex.org/works?filter=institutions.id:', uva_institutions$id)

  # \----------------------------- Vectorize this -----------------------------/
  if (all_data_one_list == T) {
    uva_results <- list()
    for (i in 1:nrow(uva_institutions)) {
      temp <- oa_request(uva_institutions$temp_query[i])
      uva_results <- append(uva_results, temp)
    }
  } else {
    uva_results <- list()
    for (i in 1:nrow(uva_institutions)) {
      temp <- oa_request(uva_institutions$temp_query[i])
      uva_results[[i]] <- temp
    }
    names(uva_results) <- uva_institutions$institution
  }

  cat('Results retrieved for the following UVA-affiliated institutions: ', paste(uva_institutions$institution, collapse = ','))
  uva_results
}
