#' Generates full analysis of one stream of streamer
#' 
#' @param path
#' @param window_size
#' @param interactive
#' @return list of plots
#' @export
analysis_of_stream <- function(path, window_size = 5, interactive = TRUE) {
  df <- load_stream_chat(path)
  gen_all_plots(df, path = path, window_size = window_size,
                interactive = interactive)
}