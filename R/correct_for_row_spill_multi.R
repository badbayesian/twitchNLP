#' Corrects for when .html to .txt spills over single comment to multiple lines.
#' This seems to happen when the comment has a specific emoji (not super clear)
#' 
#' @import data.table
#' @param df
#' @return df
correct_for_row_spill_multi <- function(df) {
  .rm.na <- function(str) {
    ifelse(is.na(str), "", str)
  }
  
  error_ <- FALSE
  j <- 1
  for (i in 1:nrow(df)) {
    if (df[i, error]) {
      error_ <- TRUE
      df[j, X1 := glue::glue("{df[j, X1]} {.rm.na(df[i, X1])}") %>%
           as.character() %>%
           trimws()]
    } else {
      error_ <- FALSE
      j <- i
    }
  }
  
  df %>%
    dplyr::filter(!error) %>%
    dplyr::select(-error)
}