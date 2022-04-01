#' oa_request
#'
#' Retrieve data from the OpenAlex API.
#'
#' (Details forthcoming.)
#'
#' @param query A single URL string beginning with "http(s)://api.openalex.org"
#'
#' @return A list containing the API call results
#'
#' @examples
#' # query <- 'https://api.openalex.org/authors/A3184395717'
#' # sagan_dat <- oa_request(query)
#'
#' @export
oa_request <- function(query) {
  # Check and format query
  if (stringi::stri_detect(query, regex = '^https?://api\\.openalex\\.org') == F) {stop('`query` argument must begin with `http(s)://api.openalex.org`', call. = F)}
  if (stringi::stri_detect(query, regex = '&per-page=\\d+') == F) {query <- paste0(query, '&per-page=200')}
  if (stringi::stri_extract(query, regex = '(?<=&per-page=)\\d+') != '200') {query <- stringi::stri_replace(query, regex = '(?<=&per-page=)\\d+', replacement = '200')}
  if (stringi::stri_detect(query, regex = '&cursor=\\*')) {query <- stringi::stri_replace(query, regex = '&cursor=\\*', replacement = '')}

  # Make contact and check returned data format
  user <- httr::user_agent(email_gen())
  init_query <- httr::GET(query, user)
  if (httr::http_type(init_query) != 'application/json') {stop('API did not return json', call. = F)}
  if (httr::status_code(init_query) != 200) {stop(paste0('OpenAlex API request failed; status code: ', httr::status_code(init_query)), call. = F)}
  scanned_query <- jsonlite::fromJSON(httr::content(init_query, as = 'text', encoding = 'utf-8'), simplifyVector = F)

  # If meta field is null, return results as is (e.g., for single-result request); otherwise, download as appropriate given size of request
  if(is.null(scanned_query$meta)) {
    return(scanned_query)
  } else {
    # Set size of OpenAlex request that requires cursor paging
    large <- 10001
    # Identify size of request
    size <- scanned_query$meta$count
    cat('Attempting to pull', size, ifelse(size > 1, 'records', 'record'), 'from the OpenAlex API via', ifelse(size < large, 'basic paging', 'cursor paging'), '(200 records per page)',
        '\nDetails on the basic paging/cursor paging distinction are here: https://docs.openalex.org/api/get-lists-of-entities#basic-paging-up-to-10-000-results',
        '\nAPI pool:', init_query$headers$`x-api-pool`, paste0('(', ifelse(init_query$headers$`x-api-pool` == 'polite', 'faster', 'slower'),')\n'))
    out <- list()
    pages_needed <- ceiling(size / 200)

    # Pull records via basic paging if `size` is 10k or less; pull via cursor paging if `size` is larger than 10k
    if (size < large) {
      for (i in 1:pages_needed) {
        cat(paste0('\rDownloaded ', i, '/', pages_needed, ' pages'))
        query_paged <- paste0(query, '&page=', i)
        a_page <- httr::GET(query_paged, user)
        if (httr::status_code(a_page) != 200) {stop(paste0('OpenAlex API request failed on page ', i, ' of ', pages_needed, '; ', 'status code: ', httr::status_code(a_page)), call. = F)}
        a_page_parsed <- jsonlite::fromJSON(httr::content(a_page, as = 'text', encoding = 'utf-8'), simplifyVector = F)
        out <- append(out, list(a_page_parsed$results))
      }
    } else {
      for (i in 1:pages_needed) {
        cat(paste0('\rDownloaded ', i, '/', pages_needed, ' pages'))
        if (i == 1) {query_paged <- paste0(query, '&cursor=*')}
        a_page <- httr::GET(query_paged, user)
        if (httr::status_code(a_page) != 200) {stop(paste0('OpenAlex API request failed on page ', i, ' of ', pages_needed, '; ', 'status code: ', httr::status_code(a_page)), call. = F)}
        a_page_parsed <- jsonlite::fromJSON(httr::content(a_page, as = 'text', encoding = 'utf-8'), simplifyVector = F)
        out <- append(out, list(a_page_parsed$results))
        query_paged <- paste0(query, '&cursor=', a_page_parsed$meta$next_cursor)
      }
    }

    # Output of API call is list of pages (200 records/page); un-peel list by one layer and combine individual records into single list
    to_return <- list()
    to_return <- lapply(out, function(x) append(to_return, x))

    # Return
    cat('\n')
    to_return
  }
}
