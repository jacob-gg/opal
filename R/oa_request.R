#' oa_request
#'
#' Retrieve data from the OpenAlex API using a custom query.
#'
#' (Details forthcoming.)
#'
#' @param query A URL beginning with "http(s)://api.openalex.org"
#' @param use_fast_api_pool Logical value indicating whether to make OpenAlex API call to polite pool (faster) or common pool (slower)
#' @param remove_duplicates Check for and remove duplicate records at the end of the call
#' @param verbose Print API call details to the console
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
oa_request <- function(query, use_fast_api_pool = T, remove_duplicates = T, verbose = T) {
  # Check structure of query and modify as needed
  query <- check_oa_query(query)
  if (!is.logical(use_fast_api_pool)) {stop('use_fast_api_pool must be logical')}

  # Make contact and check returned data format
  user <- httr::user_agent(ifelse(use_fast_api_pool == T, email_gen(), 'httr'))
  init_query <- httr::GET(query, user)
  if (httr::http_type(init_query) != 'application/json') {stop('API did not return json', call. = F)}
  if (httr::status_code(init_query) != 200) {stop(paste0('OpenAlex API request failed; status code: ', httr::status_code(init_query)), call. = F)}
  scanned_query <- jsonlite::fromJSON(httr::content(init_query, as = 'text', encoding = 'utf-8'), simplifyVector = F)

  # If meta field is null, return results as is (e.g., for single-result request); otherwise, download as appropriate given size of request
  if(is.null(scanned_query$meta)) {
    scanned_query <- list(scanned_query)
    return(scanned_query)
  } else {
    # Set size of OpenAlex request that requires cursor paging
    large <- 10001
    # Identify size of request
    size <- scanned_query$meta$count
    if (verbose == T) {
      cat('Attempting to pull', size, ifelse(size != 1, 'records', 'record'), 'from the OpenAlex API via', ifelse(size < large, 'basic paging', 'cursor paging'), '(200 records per page)',
          '\nDetails on the basic paging/cursor paging distinction are here: https://docs.openalex.org/api/get-lists-of-entities#basic-paging-up-to-10-000-results',
          '\nAPI pool:', init_query$headers$`x-api-pool`, paste0('(', ifelse(init_query$headers$`x-api-pool` == 'polite', 'faster', 'slower'),')\n'))
    }
    pages_needed <- ceiling(size / 200)

    returned_pages <- vector(mode = 'list', length = pages_needed)

    # Pull records via basic paging if `size` is 10k or less; pull via cursor paging if `size` is larger than 10k
    if (size < large) {
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

    # Output of API call is list of pages (200 records/page); un-peel list by one layer and combine individual records into single list
    # \----------------- vectorize this once the null cursor issue is resolved -----------------/
    to_return <- list()
    for (i in 1:length(returned_pages)) {
      to_return <- append(to_return, returned_pages[[i]][1:length(returned_pages[[i]])])
    }

    # Remove duplicates
    if (remove_duplicates == T) {
      to_return <- to_return[duplicated(to_return) == F]
      if (verbose == T) {cat('\nDuplicate records removed:', size - length(to_return), '\n')}
    }

    to_return
  }
}

# Note: Partway through some large (>10k records) calls, the `next_cursor` value suddenly becomes null, despite there being more entries to come
#   (e.g., to see example, drop cat('\n... ', length(a_page_parsed$results), ' ... ', a_page_parsed$meta$next_cursor, '... \n') in the cursor paging loop)
# during a call to https://api.openalex.org/authors?filter=display_name.search:marks&per-page=200&cursor=*
# After that point, we just return the first page (&cursor=*) over and over until page = pages_needed
# Will contact OpenAlex folks to see if they have input
