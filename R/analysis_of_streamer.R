#' Generates full analysis of all streams of streamer
#' 
#' @param path
#' @param window_size
#' @param interactive
#' @return plots
#' @export
analysis_of_streamer <- function(path, window_size = 5, interactive=TRUE) {
  streams <- glue::glue("{list.files(path, full.names = TRUE)}/chat/")
  
  progressr::with_progress({
    p <- progressr::progressor(along = streams)
    plots <- furrr::future_map(streams, function(stream) {
      p()
      analysis_of_stream(stream, window_size  = window_size,
                         interactive = interactive)
    }, .options = furrr::furrr_options(seed = TRUE))
  })
  return(plots)
}