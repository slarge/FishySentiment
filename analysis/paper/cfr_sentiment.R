library(ggplot2)
library(sentimentr)
library(tidyr)
library(dplyr)
library(patchwork)
library(magrittr)


all_cfr <- readRDS("analysis/data/raw_data/20200609-cfrtext.rds")


section_sentiment <- all_cfr %>%
  select(-year, -part) %>%
  tidyr::unnest(cols = c(regulations)) %>%
  tidyr::unnest(cols = c(data)) %>%
  mutate(sentences = get_sentences(TEXT)) %$%
  sentiment_by(sentences, list(year, part)) %>%
  mutate(max = ave_sentiment + 2 * sd,
         min = ave_sentiment - 2 * sd) %>%
  I()

sect <- section_sentiment %>%
   ggplot() +
   aes(y = ave_sentiment, ymin = min, ymax = max,  x = as.factor(year), color = as.factor(part)) +
   geom_point() +
   geom_linerange() +
   labs(title = "Title 50 Chapter 6",
          subtitle = "Sentiment Analysis by part over time",
        x = "",
        y = "Mean sentiment (±2×sd)",
        color = "") +
   facet_wrap(.~ part) +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90, size = 8)) +
   NULL

ggsave("analysis/figures/sentiment_on_50.pdf", sect)




