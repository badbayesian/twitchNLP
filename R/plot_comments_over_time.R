#' Plots moving windowed comment of some moment over some stream
#'
#' @import ggplot2
#' @param df
#' @param stream_title
#' @param window_size
#' @param .f function of moment
#' @param .fname name of function of moment for ggplot title
#' @param interactive
#' @return ggplot (time series of some moment of twitch comments)
#' @export
plot_comments_over_time <- function(
  df, stream_title, window_size = 5, .f = mean, .fname = "Mean",
  interactive = FALSE) {
  
  gg <- df %>%
    set_window(window_size, .f) %>%
    dplyr::mutate(elapsed_time = as.numeric(elapsed_time)) %>%
    ggplot() +
    aes(x = elapsed_time, y = count) +
    geom_line() +
    theme_classic() +
    labs(title = glue::glue("Moving Window Comment {.fname} ",
                            "({window_size} minutes window)"),
         subtitle = glue::glue("Stream Title: {stream_title}") %>%
           stringr::str_wrap(89),
         x = "Elapsed Time",
         y = "")
  
  if (interactive) {
    # ggplotly subtitle fix
    # https://datascott.com/blog/subtitles-with-ggplotly/
    plotly::ggplotly(gg) %>%
      plotly::layout(
        title = list(
          text = glue::glue("Moving Window Comment {.fname} ",
                            "({window_size} minutes window)",
                            "<br>",
                            "<sup>",
                            "Stream Title: {stream_title}",
                            "</sup>")))
  } else {
    gg
  }
}