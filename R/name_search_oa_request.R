#' name_search_oa_request
#'
#' Retrieve data from the OpenAlex API for works with authors matching a search string.
#'
#' (Details forthcoming.)
#'
#' @param name A search string (e.g., "sagan"; "hadley wickham")
#'
#' @return A list containing the API call results; each element of the outermost list contains data for the works associated with one matched author (some searchers return >1 author)
#'
#' @family OpenAlex
#'
#' @examples
#' results <- name_search_oa_request('carl sagan')
#'
#' @export
name_search_oa_request <- function(name) {
  # Spaces in search --> %20 for query URL
  name <- stringi::stri_replace_all(name, regex = '\\s{1,}', replacement = '%20')
  # Build query to find authors matching search_name (OpenAlex name search query looks like: https://api.openalex.org/authors?filter=display_name.search:carl sagan)
  name_query <- paste0('https://api.openalex.org/authors?filter=display_name.search:', name)
  # Find authors
  matching_authors <- oa_request(name_query)
  # Generate output list to store data for those authors' works
  author_works <- vector(mode = 'list', length = length(matching_authors))
  # Pull works for each matching author and add to list
  for (i in 1:length(matching_authors)) {
    author_works[[i]] <- oa_request(paste0('https://api.openalex.org/works?filter=author.id:', matching_authors[[i]]$id))
    names(author_works)[[i]] <- paste0(matching_authors[[i]]$display_name, ' (',
                                       stringi::stri_extract(matching_authors[[i]]$id, regex = '(?<=\\W)(?i)[A-Z|0-9]+$'), ')')
  }
  cat(paste0('Found ', length(matching_authors), " authors in OpenAlex matching the search string '", name, "'"))
  author_works
}
