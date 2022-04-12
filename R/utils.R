# Unexported utility functions for `opal`
# jacob-gg

check_oa_query <- function(query) {
  if (stringi::stri_detect(query, regex = '^https?://api\\.openalex\\.org') == F) {stop('`query` argument must begin with `http(s)://api.openalex.org`', call. = F)}
  if (stringi::stri_detect(query, regex = 'doi') == F) {
    if (stringi::stri_detect(query, regex = '&per-page=\\d+') == F) {query <- paste0(query, '&per-page=200')}
    if (stringi::stri_extract(query, regex = '(?<=&per-page=)\\d+') != '200') {query <- stringi::stri_replace(query, regex = '(?<=&per-page=)\\d+', replacement = '200')}
    if (stringi::stri_detect(query, regex = '&cursor=\\*')) {query <- stringi::stri_replace(query, regex = '&cursor=\\*', replacement = '')}
  }
  query
}

email_gen <- function(mailto = T) {
  usr <- paste0(sample(c(letters, 1:9), sample(5:9, 1), replace = T), collapse = '')
  domain <- paste0(sample(c(letters, 1:9), sample(5:9, 1), replace = T), collapse = '')
  paste0(ifelse(mailto == T, 'mailto:', ''), usr, '@', domain, '.com')
}
