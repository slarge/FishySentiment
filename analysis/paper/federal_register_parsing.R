# devtools::install_github("slarge/fedregs")

library(dplyr)
# library(fedregs)
# library(httr)
# library(lubridate)
# library(ggplot2)
# library(xml2)
library(purrr)

source("R/utilities.R")

# future::plan(multisession, workers = future::availableCores() - 2)
## Extract the section for each title of the CFR and creates the fr url to query
fr_section_data <- data.frame(cfr_title_number = 1:50,
                              stringsAsFactors = FALSE) %>%
  mutate(cfr_url = sprintf("https://www.govinfo.gov/bulkdata/ECFR/title-%s/ECFR-title%s.xml", cfr_title_number, cfr_title_number),
         cfr_toc = purrr::map(cfr_url, purrr::possibly(get_section, otherwise = NA_character_))) %>%
  tidyr::unnest(c(cfr_toc)) %>%
  filter(!grepl("\\[Reserved\\]", cfr_toc)) %>%
  mutate(cfr_section = stringr::str_match(cfr_toc, "([[:digit:]]{1,4}\\.[[:digit:]]{1,4})")[,2],
         cfr_part =  as.character(gsub("\\..*", "", cfr_section)),
         fr_url = paste0("www.federalregister.gov/api/v1/documents.json?fields%5B%5D=action&fields%5B%5D=citation&fields%5B%5D=publication_date&fields%5B%5D=full_text_xml_url&fields%5B%5D=title&fields%5B%5D=type&per_page=2000&order=newest&conditions%5Bcfr%5D%5Btitle%5D=",
                         cfr_title_number,
                         "&conditions%5Bcfr%5D%5Bpart%5D=",
                         cfr_part))

saveRDS(fr_section_data, "analysis/data/derived_data/FR_section_data.RDS")

## Query the Federal Register for each unique CFR title and CFR part (e.g., Title 50 part 648)
future::plan(multisession, workers = future::availableCores() - 2)
fr_section_output <- fr_section_data %>%
  select(cfr_title_number, cfr_part, fr_url) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(out = purrr::map(fr_url, get_fr_text)) %>%
  tidyr::unnest(c(out))

saveRDS(fr_section_output, "analysis/data/derived_data/FR_section_output.RDS")

## Parse the Federal Register entries for CFR Part (ex. 648), summary, and supplementary info
fr_section_summary <- fr_section_output %>%
  filter(!is.na(fr_full_text_xml_url)) %>%
  head(10) %>%
  mutate(summary =  purrr::pmap(list(fr_full_text_xml_url,
                                            cfr_part), get_fr_summary)) #%>%
  # tidyr::unnest(c(summary)) %>%
  # select(cfr_part, -count, -description, -total_pages, -cfr, -next_page_url)

saveRDS(fr_section_summary, "analysis/data/derived_data/FR_section_summary.RDS")
