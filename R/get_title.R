#' Gets title of stream
#' 
#' @param path
#' @return str
get_title <- function(path) {
  local <- stringr::str_split(stringi::stri_reverse(path), "/", 3)[[1]][3] %>%
    stringi::stri_reverse() %>%
    glue::glue("/README.md")
  readLines(local, n = 1)
}
