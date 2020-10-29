library(dplyr)
library(purrr)

source("R/utilities.R")

## Extract the section for each title of the CFR and creates the fr url to query
fr_section_data <- data.frame(cfr_title_number = 1:50,
                              stringsAsFactors = FALSE) %>%
  mutate(cfr_url = sprintf("https://www.govinfo.gov/bulkdata/ECFR/title-%s/ECFR-title%s.xml", cfr_title_number, cfr_title_number),
         cfr_toc = purrr::map(cfr_url, purrr::possibly(get_section, otherwise = NA_character_))) %>%
  tidyr::unnest(c(cfr_toc)) %>%
  filter(!grepl("\\[Reserved\\]", cfr_toc)) %>%
  mutate(cfr_section = stringr::str_match(cfr_toc, "([[:digit:]]{1,4}\\.[[:digit:]]{1,4})")[,2],
         cfr_part =  as.character(gsub("\\..*", "", cfr_section)),
         fr_url = paste0("www.federalregister.gov/api/v1/documents.json?fields%5B%5D=action&fields%5B%5D=citation&fields%5B%5D=publication_date&fields%5B%5D=full_text_xml_url&fields%5B%5D=title&fields%5B%5D=type&per_page=2000&order=oldest&conditions%5Btype%5D%5B%5D=RULE&conditions%5Bcfr%5D%5Btitle%5D=",
                         cfr_title_number,
                         "&conditions%5Bcfr%5D%5Bpart%5D=",
                         cfr_part))


# saveRDS(fr_section_data, "analysis/data/derived_data/FR_section_data.RDS")
fr_section_data <- readRDS("analysis/data/derived_data/FR_section_data.RDS")

## Query the Federal Register for each unique CFR title and CFR part (e.g., Title 50 part 648)
fr_section_output <- fr_section_data %>%
  select(cfr_title_number, cfr_part, fr_url) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(out = purrr::map(fr_url, get_fr_text)) %>%
  tidyr::unnest(c(out))

# saveRDS(fr_section_output, "analysis/data/derived_data/FR_section_output.RDS")
fr_section_output <- readRDS("analysis/data/derived_data/FR_section_output.RDS")

get_fr_summary_possibly <- purrr::possibly(get_fr_summary, otherwise = NA_real_)

## Parse the Federal Register entries for CFR Part (ex. 648), summary, and supplementary info (since 2014)
fr_section_summary <- fr_section_output %>%
  mutate(fr_publication_date = as.Date(fr_publication_date)) %>%
  filter(!is.na(fr_full_text_xml_url),
         fr_publication_date >= "2014-01-01",
         grepl("^final rule|^direct final rule|^interim final rule", tolower(fr_action))) %>%
  # head(100) %>%
  mutate(summary =  purrr::pmap(list(fr_full_text_xml_url,
                                            cfr_part), get_fr_summary_possibly, return_summary_text = FALSE))


saveRDS(fr_section_summary, "analysis/data/derived_data/FR_section_summary.RDS")

fr_sections <- bind_rows(fr_section_summary %>%
                           filter(is.na(summary)) %>%
                           mutate(summary =  purrr::pmap(list(fr_full_text_xml_url,
                                                              cfr_part), get_fr_summary_possibly, return_summary_text = FALSE)),
                         fr_section_summary %>%
                           filter(!is.na(summary))) %>%
  filter(map_dbl(summary, nrow) >= 1) %>%
  tidyr::unnest(cols = c(summary)) %>%
  select(-cfr, -next_page_url, -fr_summary, -fr_supplement)

saveRDS(fr_sections, "analysis/data/derived_data/FR_sections.RDS")

clean_sections <- fr_sections %>%
  filter(!grepl('-|appendix|privacy|amended', tolower(cfr_section_number)),
         !grepl("\\d*\\.\\d*\\.", cfr_section_number),
         !grepl("_\\.\\d+", cfr_section_number)) %>%
  mutate(cfr_section_number =  sub("([.-])|[[:punct:]]", "\\1", cfr_section_number),
         cfr_section_number = sub("\u201C", "", cfr_section_number)) %>%
  filter(grepl("(^[[:digit:]]{1,5}\\.[[:digit:]]{1,5})", cfr_section_number))

saveRDS(clean_sections, "analysis/data/derived_data/clean_fr_sections.RDS")
## Next, need to join fr_section_data and fr_section_summary by unique cfr_part to create time-series
