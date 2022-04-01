# Unexported utility functions for `opal`
# jacob-gg

email_gen <- function(mailto = T) {
  usr <- paste0(sample(c(letters, 1:9), sample(5:9, 1), replace = T), collapse = '')
  domain <- paste0(sample(c(letters, 1:9), sample(5:9, 1), replace = T), collapse = '')
  paste0(ifelse(mailto == T, 'mailto:', ''), usr, '@', domain, '.com')
}
