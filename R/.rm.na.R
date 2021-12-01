#' Converts NA to ""
#' 
#' @param str
#' @return ""
.rm.na <- function(str) {
  ifelse(is.na(str), "", str)
}