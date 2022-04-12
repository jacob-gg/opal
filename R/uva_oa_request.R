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
#' @export
uva_oa_request <- function(id_file = 'uva_institutions.txt', all_data_one_list = T) {
  if (id_file %in% dir('data') == F) {
    stop(paste0('Ensure that ', id_file, ' is in data/ folder'), call. = F)
  }
  institutions <- utils::read.csv('data/uva_institutions.txt', header = T)
  if (all(c('id', 'institution') %in% colnames(institutions)) == F) {
    stop('Variables `id` (OpenAlex ID) and `institution` must be in id_file', call. = F)
  }
  institutions$temp_query <- paste0('https://api.openalex.org/works?filter=institutions.id:', institutions$id)

  if (all_data_one_list == T) {
    uva_results <- list()
    for (i in 1:nrow(institutions)) {
      temp <- oa_request(institutions$temp_query[i])
      uva_results <- append(uva_results, temp)
    }
  } else {
    uva_results <- list()
    for (i in 1:nrow(institutions)) {
      temp <- oa_request(institutions$temp_query[i])
      uva_results[[i]] <- temp
    }
    names(uva_results) <- institutions$institution
  }

  cat('Results retrieved for the following institutions: ', paste(institutions$institution, collapse = ','))

  uva_results
}
