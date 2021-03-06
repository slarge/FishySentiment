
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(sentimentr)
source(here::here("R/utilities.R"))


#load data

output <- readRDS(here::here("analysis/data/derived_data/FR_section_output.RDS"))
#section_summary <- readRDS(here::here("analysis/data/derived_data/FR_section_summary.RDS"))
#section_summary

## Look at the sentiment of the summary and supplements

#Using `sentimentr` package. to find average sentiment of sentences in the abstracts.


safe_summary <- purrr::safely(get_fr_summary, NA_real_)

## null distribution
#get parts for a random sample of all rules
set.seed(8675309)
build_null <- output %>%
  select(cfr_title_number, cfr_part) %>%
  distinct() %>%
  slice_sample(n = 500) %>%
  I()
#build_null

future::plan(future::multisession)
system.time(fr_null_summary <- output %>%
  filter(!is.na(fr_full_text_xml_url),
         cfr_title_number %in% build_null$cfr_title_number,
         cfr_part %in% build_null$cfr_part) %>%
  slice(1:100) %>%
  mutate(summary =  furrr::future_pmap(list(fr_full_text_xml_url,
                                            #cfr_part), get_fr_summary)) %>%
                                            cfr_part), safe_summary)) %>%
  I()
)
saveRDS(fr_null_summary, "analysis/data/derived_data/FR_null_summary.RDS")

# #trial
# build_null <- tibble(cfr_title_number = 1,
#                      cfr_part = "10")
#
# null_fr_sentiment <- fr_sentiment %>%
#   right_join(build_null) %>%
#   I()
# null_fr_sentiment
#```

## fisheries sentiment

#get the sentiment for rules for NOAA Fisheries

nmfs_part <- as.character(c(600, 622, 635, 648, 660, 665, 679, 680, 697))

fr_nmfs_summary <- output %>%
  filter(cfr_title_number == 50,
         cfr_part %in% nmfs_part) %>%
  slice(1:100) %>%
  mutate(summary =  furrr::future_pmap(list(fr_full_text_xml_url,
                                            cfr_part), safe_summary)) %>%
  I()

saveRDS(fr_nmfs_summary, "analysis/data/derived_data/FR_nmfs_summary.RDS")


# nmfs_fr_sentiment <- fr_sentiment %>%
#   filter(cfr_title_number == 50,
#          cfr_part %in% nmfs_part) %>%
#  I()
#nmfs_fr_sentiment



#
# # sentiments of sentences of the summary
# fr_sentiment <- section_summary %>%
#   filter(fr_action == "Final rule.") %>%
#   select(fr_publication_date, cfr_title_number, cfr_part, fr_citation, summary) %>%
#   rowid_to_column() %>%
#   mutate(fr_supplement = map_chr(summary, ~.$fr_supplement[[1]]),
#          fr_summary = map_chr(summary, ~.$fr_summary[[1]])) %>%
#   pivot_longer(cols = c(fr_supplement, fr_summary), names_to = "fr_text_name") %>%
#   mutate(sentences = map(value, get_sentences),
#          fr_sentiment = map(sentences, sentiment_by),
#          fr_text_name = str_remove(fr_text_name, "fr_")) %>%
#   unnest(cols = fr_sentiment) %>%
#   #make publication date more easy to work with
#   #select(publication_date, ave_sentiment) %>%
#   mutate(date = lubridate::date(fr_publication_date),
#          year = lubridate::year(fr_publication_date)) %>%
#   I()
#

## viz

#summarize by year

# nmfs_fr_sentiment %>%
#   filter(fr_text_name == "summary") %>%
#   group_by(fr_citation) %>%
#   slice(1) %>%
#   ungroup() %>%
#   ggplot() +
#   aes(y = ave_sentiment, x = factor(year)) +
#   #aes(y = ave_sentiment, x = date) +
#   geom_boxplot() +
#   #geom_jitter(alpha = 0.5) +
#   geom_smooth() +
#   facet_wrap(~cfr_part) +
#   theme_minimal() +
#   labs(x = "year",
#        y = "average sentiment",
#        title = "NMFS rules in federal register",
#        subtitle = "average sentiment of summary paragraphs of NOAA Rules",
#        caption = "\n@gavin_fay\ndata from https://federalregister.gov") +
#   NULL
#
# ggsave("nmfs-sentiment-yr.png",width = 8, height = 4.5)

# <!-- A qnd linear model suggests positive effect of 2020 is statistically clear. -->
#
# <!-- ```{r} -->
# <!-- regs_lm <- regs_summary %>%    -->
# <!--   mutate(month = lubridate::month(date)) %>%  -->
# <!--   filter(month <= 5) %>%  -->
# <!--   mutate(twenty = ifelse(year==2020, 1, 0)) %>%  -->
# <!--   #lm(ave_sentiment~twenty,data=.) %>%  -->
# <!--   lm(ave_sentiment~factor(year)-1,data=.) %>%    -->
# <!--   tidy(conf.int = TRUE) %>% #augment() -->
# <!--   I() -->
# <!-- regs_lm -->
# <!-- regs_lm %>%  -->
# <!--   mutate(year = 2001:2020) %>%  -->
# <!--   ggplot() + -->
# <!--   aes(x=year, y = estimate) + -->
# <!--   #geom_jitter(data = regs_summary, aes(x=year, y=ave_sentiment), alpha = 0.2) + -->
# <!--   geom_point() + -->
# <!--   geom_errorbar(aes(ymin=conf.low, ymax = conf.high)) + -->
# <!--   geom_smooth(data = regs_summary, mapping = aes(x=year, y = ave_sentiment), method = "lm") + -->
# <!--   geom_smooth(data = regs_summary, mapping = aes(x=year, y = ave_sentiment), method = "gam", col = "orange") + -->
# <!--   #ylim(-0.1,0.4) + -->
# <!--   labs(x = "year", -->
# <!--        y = "average sentiment", -->
# <!--        title = "NOAA rules in federal register more positive in 2020", -->
# <!--        subtitle = "average sentiment of abstracts of NOAA Rules during Jan-May", -->
# <!--        caption = "\n@gavin_fay\ndata from https://federalregister.gov") + -->
# <!--   NULL -->
# <!-- ``` -->
#
# <!-- ```{r} -->
# <!-- tt <- " NMFS approves and -->
# <!-- implements the New England Fishery -->
# <!-- Management Council’s Habitat Clam -->
# <!-- Dredge Exemption Framework -->
# <!-- Adjustment to its Fishery Management -->
# <!-- Plans. This action establishes three -->
# <!-- areas within the Great South Channel -->
# <!-- Habitat Management Area where vessels -->
# <!-- may not fish for Atlantic surfclams or blue -->
# <!-- mussels with dredge gear. This action is -->
# <!-- intended to restrict the fishing industry -->
# <!-- access to part of the surfclam and blue -->
# <!-- mussel resource within the Habitat -->
# <!-- Management Area while balancing the -->
# <!-- Council’s habitat conservation -->
# <!-- objectives. " -->
# <!-- sentiment_by(tt) -->
# <!-- ``` -->
#
