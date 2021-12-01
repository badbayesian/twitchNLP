
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

You'll need to also generate the conda enviroment `twitchNLP` with:
``` bash
conda env create -f enviroment.yml
```

## Downloading twitch comments and structure.

You can download [twitch stream comments here](https://vods.online/twitch_chat.html), be nice!

For this program to understand the chat logs, the structure needs to be `/<streamer>/<date of stream>/chat/*.html` e.g. `data/dylanburnstv/Friday, November 26, 2021/chat/page_1.html`. There may be multiple pages of html files corresponding to the number of comments. Similarly, the first line in `/<streamer>/<date of stream>/chat/README.md` will be considered as the stream title for any plots.

## Example

This is a simple example for processing a single downloaded stream and all the streams downloaded from a streamer.

``` r
library(furrr)
plan(multisession)

library(progressr)
handlers("pbmcapply", "beepr")

library(twitchNLP)

# From a given twitch stream chat logs, we can generate a number of different plots
dylan_nov_26_path <- "data/dylanburnstv/Friday, November 26, 2021/chat/"
df <- load_stream_chat(dylan_nov_26_path)
single_stream_plots <- gen_all_plots(df, path = dylan_nov_26_path, window_size = 5, interactive = TRUE)

# Similarly, from a given streamer, we can generate the previous plots for each stream
streamer_plots <- analysis_of_streamer("data/dylanburnstv")
```

