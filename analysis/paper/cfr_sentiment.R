library(ggplot2)
library(sentimentr)
library(tidyr)
library(dplyr)


all_cfr <- readRDS("analysis/data/raw_data/20200609-cfrtext.rds")

tt <- all_cfr %>%
  head(1) %>%
  select(-year, -part) %>%
  tidyr::unnest(cols = c(regulations)) %>%
  tidyr::unnest(cols = c(data)) %>%
  # group_by(SECTION_NUMBER) %>%
  mutate(sentences = purrr::map(TEXT, get_sentences),
         sentiment = purrr::map(sentences, sentiment_by)) %>%
  unnest(cols = sentiment) %>%
  mutate(max = ave_sentiment + 2 * sd,
         min = ave_sentiment - 2 * sd) %>%
  I()

tt %>%
  ggplot() +
  #aes(y = ave_sentiment, x = factor(year)) +
  aes(y = ave_sentiment, ymin = min, ymax = max,  x = SECTION_NUMBER) +
  geom_point() +
  geom_linerange()
  # geom_jitter(alpha = 0.5) +
  # geom_smooth() +
  NULL


