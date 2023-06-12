#' oa_request
#'
#' Retrieve data from the OpenAlex API using a custom query.
#'
#' (Details forthcoming.)
#'
#' @param query A URL (string) beginning with "http(s)://api.openalex.org"
#' @param use_fast_api_pool Logical value indicating whether to call to the OpenAlex API polite pool (faster) or common pool (slower)
#' @param remove_duplicates Logical value indicating whether to check for and remove duplicate records at the end of the call
#' @param verbose Logical value indicating whether to print API call details to the console
#' @param return_one_pg Logical value indicating whether to only return one page of results from the API (for testing and debugging purposes)
#'
#' @return A list containing the API call results
#'
#' @family OpenAlex
#'
#' @examples
#' query <- 'https://api.openalex.org/authors/A3184395717'
#' sagan_dat <- oa_request(query)
#'
#' @export
oa_request <- function(query, use_fast_api_pool = T, remove_duplicates = T, verbose = T, return_one_pg = F) {
  # Check structure of query and modify as needed
  query <- check_oa_query(query)
  if (all(c(use_fast_api_pool, remove_duplicates, verbose) %in% c(T, F)) == F) {stop('use_fast_api_pool, remove_duplicates, and verbose must all be T(RUE) or F(ALSE)')}

  # Make contact and check returned data format
  user <- httr::user_agent(ifelse(use_fast_api_pool == T, email_gen(), 'httr'))
  init_query <- httr::GET(query, user)
  if (httr::http_type(init_query) != 'application/json') {stop('API did not return json', call. = F)}
  if (httr::status_code(init_query) != 200) {stop(paste0('OpenAlex API request failed; status code: ', httr::status_code(init_query)), call. = F)}
  scanned_query <- jsonlite::fromJSON(httr::content(init_query, as = 'text', encoding = 'utf-8'), simplifyVector = F)

  # If meta field is null, go ahead and return results as is (e.g., for single-result request)
  if (is.null(scanned_query$meta)) {
    scanned_query <- list(scanned_query)
    return(scanned_query)
  }

  # Download data as appropriate given query_size
  large_request <- 10001
  query_size <- scanned_query$meta$count
  pages_needed <- ifelse(return_one_pg == TRUE, 1, ceiling(query_size / scanned_query$meta$per_page))
  returned_pages <- vector(mode = 'list', length = pages_needed)

  if (verbose == T) {cat('Attempting to pull', query_size, ifelse(query_size != 1, 'records', 'record'), 'from the OpenAlex API via',
                         ifelse(query_size < large_request, 'basic paging', 'cursor paging'), '@', scanned_query$meta$per_page, 'records per page',
                         '\nAPI pool:', init_query$headers$`x-api-pool`, paste0('(', ifelse(init_query$headers$`x-api-pool` == 'polite', 'faster', 'slower'),')\n'))}

  # Pull records via basic paging if `query_size` is 10k or less; pull via cursor paging if `query_size` is larger than 10k
  if (query_size < large_request) {
    for (i in 1:pages_needed) {
      if (verbose == T) {cat(paste0('\rDownloaded ', i, '/', pages_needed, ' pages'))}
      query_paged <- paste0(query, '&page=', i)
      a_page <- httr::GET(query_paged, user)
      if (httr::status_code(a_page) != 200) {stop(paste0('OpenAlex API request failed on page ', i, ' of ', pages_needed, '; ', 'status code: ', httr::status_code(a_page)), call. = F)}
      a_page_parsed <- jsonlite::fromJSON(httr::content(a_page, as = 'text', encoding = 'utf-8'), simplifyVector = F)
      returned_pages[[i]] <- a_page_parsed$results
    }
  } else {
    for (i in 1:pages_needed) {
      if (verbose == T) {cat(paste0('\rDownloaded ', i, '/', pages_needed, ' pages'))}
      if (i == 1) {query_paged <- paste0(query, '&cursor=*')}
      a_page <- httr::GET(query_paged, user)
      if (httr::status_code(a_page) != 200) {stop(paste0('OpenAlex API request failed on page ', i, ' of ', pages_needed, '; ', 'status code: ', httr::status_code(a_page)), call. = F)}
      a_page_parsed <- jsonlite::fromJSON(httr::content(a_page, as = 'text', encoding = 'utf-8'), simplifyVector = F)
      returned_pages[[i]] <- a_page_parsed$results
      query_paged <- paste0(query, '&cursor=', a_page_parsed$meta$next_cursor)
    }
  }

  # Output of API call is list of pages; un-peel list by one layer and put individual records into single list
  # Vectorize this soon for speed
  to_return <- list()
  for (i in 1:length(returned_pages)) {
    to_return <- append(to_return, returned_pages[[i]])
  }

  # Remove duplicates if requested
  if (remove_duplicates == T) {
    to_return <- to_return[duplicated(to_return) == F]
    if (verbose == T) {cat('\nDuplicate records removed:', query_size - length(to_return), '\n')}
  }

  to_return
}
