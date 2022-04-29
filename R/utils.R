# Unexported utility functions for `opal`
# jacob-gg

check_oa_query <- function(query) {
  if (stringi::stri_detect(query, regex = '^https?://api\\.openalex\\.org') == F) {stop('`query` argument must begin with `http(s)://api.openalex.org`', call. = F)}
  # If query is for filtered and/or searched results, ensure that per-page is 200 and chop off cursor-paging indicator if present
  if (stringi::stri_detect(query, regex = '\\?filter=|\\?search=')) {
    if (stringi::stri_detect(query, regex = '&per-page=\\d+') == F) {query <- paste0(query, '&per-page=200')}
    if (stringi::stri_extract(query, regex = '(?<=&per-page=)\\d+') != '200') {query <- stringi::stri_replace(query, regex = '(?<=&per-page=)\\d+', replacement = '200')}
    if (stringi::stri_detect(query, regex = '&cursor=\\*?')) {query <- stringi::stri_replace(query, regex = '&cursor=\\*?', replacement = '')}
  }
  # If query is for all results of a given entity type, ensure that per-page is 200
  if (stringi::stri_detect(query, regex = '(works|authors|institutions|venues|concepts)$')) {query <- paste0(query, '?per-page=200')}

  query
}

email_gen <- function(mailto = T) {
  usr <- paste0(sample(c(letters, 1:9), sample(5:9, 1), replace = T), collapse = '')
  domain <- paste0(sample(c(letters, 1:9), sample(5:9, 1), replace = T), collapse = '')
  paste0(ifelse(mailto == T, 'mailto:', ''), usr, '@', domain, '.com')
}

get_uva_institutions <- function() {
  data.frame(id = c("I51556381", "I4210107925",
                    "I4210134211", "I4210119250",
                    "I2799765794", "I21113243"),
             institutions = c("University of Virginia", "University of Virginia Children's Hospital",
                              "University of Virginia Medical Center", "University of Virginia Hospital",
                              "University of Virginia Health System", "University of Virginia Department of Urology"))
}
