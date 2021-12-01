
# twitchNLP

<!-- badges: start -->
<!-- badges: end -->

Toolkit to do NLP analysis of twitch comments streams.

## Installation

You can install the development version of twitchNLP from [GitHub](https://github.com/badbayesian/twitchNLP) with:

``` r
# install.packages("devtools")
devtools::install_github("badbayesian/twitchNLP")
```

``` bash
conda env create -f enviroment.yml
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(furrr)
plan(multisession)

library(progressr)
handlers("pbmcapply", "beepr")

library(twitchNLP)
analysis_of_streamer("data/dylanburnstv")

dylan_nov_26_path <- "data/dylanburnstv/Friday, November 26, 2021/chat/"
df <- load_stream_chat(dylan_nov_26_path)
plots <- gen_all_plots(df, path = dylan_nov_26_path, window_size  = 5, interactive = TRUE)
```

