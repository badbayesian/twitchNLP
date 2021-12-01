#' Sets moving window average
#' TODO expand to w/e function not just moving ave (or have a separate func?)
#' 
#' @param df
#' @param window_size 
#' @param .f
#' @return
set_window <- function(df, window_size, .f = sum) {
  window_size  <- window_size  / 2
  df_ <- df %>%
    dplyr::count(elapsed_time) %>%
    dplyr::mutate(count =
                    slider::slide_index(.x = n,
                                        .i = elapsed_time,
                                        .f = .f,
                                        .before = 60 * window_size ,
                                        .after = 60 * window_size ) %>%
                    unlist(),
                  count = 2 * count / window_size )
  dplyr::left_join(df, df_, by = "elapsed_time")
}