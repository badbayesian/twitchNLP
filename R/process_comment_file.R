#' Processes .txt files of comments
#' 
#' @param address Address of translated twitch comments .txt
#' @return df
process_comment_file <- function(address) {
  # suppress warning message where some columns are extended to 3 columns from 1
  suppressWarnings({
    address %>%
      readr::read_csv(col_names = FALSE, show_col_types = FALSE) %>%
      dplyr::select(X1) %>%
      dplyr::mutate(error = !stringr::str_detect(X1, "\\*"),
                    error = ifelse(is.na(error), FALSE, error)) %>%
      data.table::as.data.table() %>%
      correct_for_row_spill_multi() %>%
      tidyr::separate(col = X1, into = c("skip", "time", "user", "comment"),
                      sep = " ", extra = "merge", remove = TRUE) %>%
      dplyr::select(-skip) %>%
      dplyr::mutate(user = gsub(".{1}$", "", user),
                    time = ifelse(nchar(time) == 5,
                                  glue::glue("00:{time}"), time),
                    elapsed_time = as.difftime(time, units = "secs")) %>%
      tibble::as_tibble() %>%
      return()
  })
}