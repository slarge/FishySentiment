# remotes::install_github("NOAA-EDAB/fedregs@eCFR")
library(fedregs)
library(purrr)
library(dplyr)

possibly_text <- purrr::possibly(cfr_text, otherwise = NA_character_)

## Specify the parts to download
today <- format(Sys.Date(), "%Y%m%d")
part_df <- tidyr::tibble(part = c(600, 622, 635, 648, 660, 665, 679, 680, 697))%>%
  tidyr::crossing(year = seq(1996, 2020, by = 1)) %>%
  dplyr::mutate(regulations = purrr::pmap(list(year, part), ~ possibly_text(year = .x,
                                                                            title_number = 50,
                                                                            chapter = 6,
                                                                            part = .y,
                                                                            return_tidytext = FALSE,
                                                                            verbose = TRUE)))

saveRDS(part_df, sprintf("analysis/data/raw_data/%s-cfrtext.rds", today))
