#' Generates all plots
#' 
#' @param df
#' @param path
#' @param window_size
#' @param interactive
#' @return list
#' @export
gen_all_plots <- function(df, path,  window_size, interactive) {
  stream_title <- get_title(path)
  
  list(
    comments_counts = plot_comments_over_time(
      df, stream_title = stream_title, window_size = window_size, .f = mean,
      .fname = "Count", interactive = interactive),
    comments_var = plot_comments_over_time(
      df, stream_title = stream_title, window_size = window_size, .f = var,
      .fname = "Variance", interactive = interactive),
    gini_plot = plot_gini(df, stream_title = stream_title,
                          interactive = interactive)
  )
}