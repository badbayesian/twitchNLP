#' Loads and Tidies chat data from twitch chat downloader
#' https://vods.online/chat.html
#' 
#' @param path
#' @param MIN_TIME
#' @return df
#' @export
load_stream_chat <- function(path, MIN_TIME = 1e-6) {
  
  reticulate::use_condaenv(condaenv = "twitchNLP")
  
  # Run Python script clean_html.py to clean html before it enters R
  reticulate::py_run_string("from python.clean_html import main")
  reticulate::py_run_string(glue::glue("main('{path}')"))
  
  all_files <- list.files(path)
  valid_files <- all_files[which(stringr::str_detect(all_files, "translated"))]
  addresses <- glue::glue("{path}{valid_files}")
  
  print("Cleaning .txt files")
  progressr::with_progress({
    p <- progressr::progressor(along = addresses)
    df <- furrr::future_map(addresses, function(address) {
      p()
      process_comment_file(address)
    }, .options = furrr::furrr_options(seed = TRUE))
  })
  data.table::rbindlist(df) %>%
    dplyr::filter(!is.na(elapsed_time))
}