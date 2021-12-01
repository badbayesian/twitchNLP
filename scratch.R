

a <- function(df) {
  df_ <- df %>%
    rowid_to_column() %>%
    unnest_tokens(word, comment)
  
  df_ <- df_ %>%
    anti_join(get_stopwords(), by="word")
  
  a <- df_ %>%
    add_count(rowid,  name = "total_words_wo_stopwords") %>%
    inner_join(get_sentiments("bing"), by = "word") %>%
    add_count(rowid, name = "sentiment_match_count") %>%
    mutate(pct_matched = sentiment_match_count/total_words_wo_stopwords)
  add_count(rowid, sentiment, name = "counts") %>%
    mutate(sentiment = ifelse(sentiment == "positive", 1, -1),
           count = sentiment * counts)
  
  b <- a %>%
    group_by(rowid) %>%
    summarize(n = sum(count))
  
  df_ <- df %>%
    rowid_to_column() %>%
    left_join(b, by = "rowid")
  
  
  
  group_by(rowid) %>%
    summarize(n = )
  mutate(sentiment = positive - negative)
  
  
  
  janeaustensentiment <- tidy_books %>%
    inner_join(get_sentiments("bing"), by = "word") %>% 
    count(book, index = line %/% 80, sentiment) %>% 
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive - negative)
  
}

df <- df %>%
  rowid_to_column()

df %>%
  unnest_tokens(word, comment)
library(janeaustenr)
library(dplyr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number()) %>%
  ungroup()

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books <- tidy_books %>%
  anti_join(get_stopwords())

tidy_books %>%
  count(word, sort = TRUE) 

get_sentiments("bing")

top_50_commenters <- df %>%
  filter(!str_detect(comment, "https|www|com")) %>%
  group_by(user) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  rowid_to_column() %>%
  head(50) %>%
  pull(user)

df %>%
  filter(user %in% top_50_commenters)


a %>%
  ggplot() +
  aes(x = rowid, y = count) +
  geom_line()

group_by(user) %>%
  unnest_tokens(word, comment) %>%
  count(word) %>%
  ungroup() %>%
  bind_log_odds(user, word, n)

avatar_lo <- avatar %>%
  unnest_tokens(word, text) %>%
  count(aang, word) %>%
  bind_log_odds(aang, word, n) %>%
  arrange(-log_odds_weighted)

avatar_lo %>%
  group_by(aang) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, log_odds_weighted)) %>%
  ggplot(aes(log_odds_weighted, word, fill = aang)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~aang, scales = "free") +
  scale_fill_avatar(palette = "AirNomads") +
  labs(y = NULL)


data("mtcars")
library(dplyr)
library(pbapply)
library(gtools)

p_hacked_lm <- function(df, formula){
  y <- strsplit(formula, " [~]")[[1]][1]
  N <- ncol(df) - 1
  X_cols_ids <- which(colnames(df) != y)
  Y_id <- which(colnames(df) == y)
  
  X_combos <- lapply(1:N, function(i) combinations(N, i, X_cols_ids))
  regs <- pblapply(X_combos,
                   function(Xs) {
                     regs <- lapply(1:nrow(Xs), function(row) lm(formula, data = df[ , c(Y_id, Xs[row, ])]))
                     max_R2 <- lapply(regs, function(reg) summary(reg)$adj.r.square) %>%
                       which.max()
                     regs[[max_R2]]
                   })
  max_R2 <- lapply(regs, function(reg) summary(reg)$adj.r.square) %>%
    which.max()
  regs[[max_R2]]
}

reg <- p_hacked_lm(mtcars, formula = paste("mpg ~ ."))
