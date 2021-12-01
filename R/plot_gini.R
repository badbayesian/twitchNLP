#' Plot gini of conversation
#' 
#' @import ggplot2
#' @param df
#' @param stream_title
#' @param interactive
#' @return ggplot/ggplotly
#' @export
plot_gini <- function(df, stream_title, interactive=FALSE) {
  gg <- df %>%
    dplyr::count(user) %>%
    dplyr::arrange(-n) %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(comment_frac = n / sum(n),
                  cumsum_ranking = rowid / max(rowid),
                  cumsum_comments = cumsum(comment_frac))
  
  gini_coefficient <- sum(outer(gg$n, gg$n, FUN = function(x, y) {abs(x - y)})) /
    (2 * length(gg$n) * sum(gg$n))
  
  gg <- gg %>%
    ggplot() +
    aes(x = cumsum_comments, y = cumsum_ranking) +
    geom_line() +
    geom_abline(linetype = "dashed") +
    coord_fixed() +
    theme_bw() +
    labs(title = glue::glue(
      "Gini Plot (coefficient: {round(gini_coefficient, 3)})"),
      subtitle = glue::glue("Stream Title: {stream_title}"),
      x = "Cumsum of comments",
      y = "User ranking by most comments") +
    scale_x_continuous(breaks = seq(0, 1, .1), labels = scales::percent_format(1)) +
    scale_y_continuous(breaks = seq(0, 1, .1), labels = scales::percent_format(1))
  
  if (interactive) {
    # ggplotly subtitle fix
    # https://datascott.com/blog/subtitles-with-ggplotly/
    plotly::ggplotly(gg) %>%
      plotly::layout(
        title = list(
          text = glue::glue(
            "Gini Plot (coefficient: {round(gini_coefficient, 3)})",
            "<br>",
            "<sup>",
            "Stream Title: {stream_title}",
            "</sup>")))
  } else {
    gg
  }
}