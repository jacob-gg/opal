# Unexported utility functions for `opal`
# jacob-gg

email_gen <- function() {
  usr <- paste0(sample(c(letters, 1:9), sample(5:9, 1), replace = T), collapse = '')
  domain <- paste0(sample(c(letters, 1:9), sample(5:9, 1), replace = T), collapse = '')
  out <- paste0('mailto:', usr, '@', domain, '.com')
  out
}
