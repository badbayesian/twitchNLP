library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(readr)
library(lubridate)
library(glue)
library(data.table)
library(slider)
library(forecast)
library(textreadr)
library(plotly)
library(tibble)
library(tidytext)
library(tidylo)
library(scales)

library(furrr)
plan(multisession)

library(progressr)
handlers("pbmcapply", "beepr")

library(reticulate)
use_condaenv(condaenv = "tmp")


#' Converts NA to ""
#' 
#' @param str
#' @return ""
.rm.na <- function(str) {
  ifelse(is.na(str), "", str)
}

#' Processes .txt files of comments
#' 
#' @param address Address of translated twitch comments .txt
#' @return df
process_comment_file <- function(address) {
  # suppress warning message where some columns are extended to 3 columns from 1
  suppressWarnings({
    address %>%
      read_csv(col_names = FALSE, show_col_types = FALSE) %>%
      select(X1) %>%
      mutate(error = !str_detect(X1, "\\*"),
             error = ifelse(is.na(error), FALSE, error)) %>%
      as.data.table() %>%
      error_correct_for_row_spill_multi() %>%
      separate(col = X1, into = c("skip", "time", "user", "comment"), sep = " ",
               extra = "merge", remove = TRUE) %>%
      select(-skip) %>%
      mutate(user = gsub(".{1}$", "", user),
             time = ifelse(nchar(time) == 5, glue("00:{time}"), time),
             elapsed_time = as.difftime(time, units = "secs")) %>%
      as_tibble() %>%
      return()
  })
}

#' Corrects for when .html to .txt spills over single comment to multiple lines.
#' This seems to happen when the comment has a specific emoji (not super clear)
#' 
#' @param df
#' @return df
error_correct_for_row_spill_multi <- function(df) {
  .rm.na <- function(str) {
    ifelse(is.na(str), "", str)
  }
  
  error_ <- FALSE
  j <- 1
  for (i in 1:nrow(df)) {
    if (df[i, error]) {
      error_ <- TRUE
      df[j, X1 := glue("{df[j, X1]} {.rm.na(df[i, X1])}") %>%
           as.character() %>%
           trimws()]
    } else {
      error_ <- FALSE
      j <- i
    }
  }
  df %>%
    filter(!error) %>%
    select(-error)
}

#' Loads and Tidies chat data from twitch chat downloader
#' https://vods.online/chat.html
#' 
#' @param path
#' @param MIN_TIME
#' @return df
load_stream_chat <- function(path, MIN_TIME=1e-6) {
  # Run Python script clean_html.py to clean html before it enters R
  py_run_string("from clean_html import main")
  py_run_string(glue("main('{path}')"))
  
  all_files <- list.files(path)
  valid_files <- all_files[which(str_detect(all_files, "translated"))]
  addresses <- glue("{path}{valid_files}")
  
  print("Cleaning .txt files")
  with_progress({
    p <- progressor(along = addresses)
    df <- future_map(addresses, function(address) {
      p()
      process_comment_file(address)
    }, .options = furrr_options(seed = TRUE))
  })
  rbindlist(df) %>%
    filter(!is.na(elapsed_time))
}

#' Sets moving window average
#' TODO expand to w/e function not just moving ave (or have a separate func?)
#' 
#' @param df
#' @param window_size 
#' @param .f
#' @return
set_window <- function(df, window_size , .f = sum) {
  window_size  <- window_size  / 2
  df_ <- df %>%
    count(elapsed_time) %>%
    mutate(count =
             slide_index(.x = n,
                         .i = elapsed_time,
                         .f = .f,
                         .before = 60 * window_size ,
                         .after = 60 * window_size ) %>%
             unlist(),
           count = 2 * count / window_size )
  left_join(df, df_, by = "elapsed_time")
}

#' Plots moving windowed comment of some moment over some stream
#'
#' @param df
#' @param stream_title
#' @param window_size
#' @param .f function of moment
#' @param .fname name of function of moment for ggplot title
#' @param interactive 
#' @return ggplot (time series of some moment of twitch comments)
plot_comments_over_time <- function(
  df, stream_title, window_size = 5, .f = mean, .fname = "Mean",
  interactive = FALSE) {
  
  gg <- df %>%
    set_window(window_size , .f) %>%
    mutate(elapsed_time = as.numeric(elapsed_time)) %>%
    ggplot() +
    aes(x = elapsed_time, y = count) +
    geom_line() +
    theme_classic() +
    labs(title = glue("Moving Window Comment {.fname} ",
                      "({window_size} minutes window)"),
         subtitle = glue("Stream Title: {stream_title}") %>%
           str_wrap(89),
         x = "Elapsed Time",
         y = "")
  
  if (interactive) {
    # ggplotly subtitle fix
    # https://datascott.com/blog/subtitles-with-ggplotly/
    ggplotly(gg) %>%
      layout(
        title = list(
          text = glue("Moving Window Comment {.fname} ",
                      "({window_size} minutes window)",
                      "<br>",
                      "<sup>",
                      "Stream Title: {stream_title}",
                      "</sup>")))
  } else {
    gg
  }
}

#' Plot gini of conversation
plot_gini <- function(df, stream_title, interactive=FALSE) {
  gg <- df %>%
    count(user) %>%
    arrange(-n) %>%
    rowid_to_column() %>%
    mutate(comment_frac = n / sum(n),
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
    labs(title = glue("Gini Plot (coefficient: {round(gini_coefficient, 3)})"),
         subtitle = glue("Stream Title: {stream_title}"),
         x = "Cumsum of comments",
         y = "User ranking by most comments") +
    scale_x_continuous(breaks = seq(0, 1, .1), labels = percent_format(1)) +
    scale_y_continuous(breaks = seq(0, 1, .1), labels = percent_format(1))
  
  if (interactive) {
    # ggplotly subtitle fix
    # https://datascott.com/blog/subtitles-with-ggplotly/
    ggplotly(gg) %>%
      layout(
        title = list(
          text = glue("Gini Plot (coefficient: {round(gini_coefficient, 3)})",
                      "<br>",
                      "<sup>",
                      "Stream Title: {stream_title}",
                      "</sup>")))
  } else {
    gg
  }
}

#' Gets title of stream 
get_title <- function(path) {
  local <- str_split(stri_reverse(path), "/", 3)[[1]][3] %>%
    stri_reverse() %>%
    glue("/README.md")
  readLines(local, n = 1)
}

#' Generates all plots
gen_all_plots <- function(df, path,  window_size , interactive) {
  stream_title <- get_title(path)
  
  list(
    comments_counts = plot_comments_over_time(
      df, stream_title = stream_title, window_size  = window_size , .f = mean,
      .fname = "Count", interactive = interactive),
    comments_var = plot_comments_over_time(
      df, stream_title = stream_title, window_size  = window_size , .f = var,
      .fname = "Variance", interactive = interactive),
    gini_plot = plot_gini(df, stream_title = stream_title,
                          interactive = interactive)
  )
}

#' Generates full analysis of one stream of streamer
analysis_of_stream <- function(path, window_size  = 5, interactive= TRUE) {
  df <- load_stream_chat(path)
  gen_all_plots(df, path = path, window_size  = window_size , interactive = interactive)
}

#' Generates full analysis of all streams of streamer
analysis_of_streamer <- function(path, window_size = 5, interactive=TRUE) {
  streams <- glue("{list.files(path, full.names = TRUE)}/chat/")
  
  with_progress({
    p <- progressor(along = streams)
    plots <- future_map(streams, function(stream) {
      p()
      analysis_of_stream(stream, window_size  = window_size , interactive = interactive)
    }, .options = furrr_options(seed = TRUE))
  })
  return(plots)
}

# Examples
dylan_plots <- analysis_of_streamer("data/dylanburnstv")

dylan_nov_12_path <- "data/dylanburnstv/Friday, November 26, 2021/chat/"
df <- load_stream_chat(dylan_nov_12_path)
plots <- gen_all_plots(df, path = dylan_nov_12_path,
                       window_size  = 5, interactive = TRUE)
