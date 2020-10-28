
get_section <- function(x) {
  xml2::read_xml(x, as = "parsed", encoding = "UTF-8")%>%
    xml2::xml_find_all(".//DIV8/HEAD") %>%
    xml2::xml_text()
}


get_fr_summary <- function(fr_xml_url, cfr_part, return_summary_text = FALSE){

  res <- xml2::read_xml(fr_xml_url, as = "parsed", encoding = "UTF-8")

  classification <- NA
  sectno <- NA
  fr_summary <- NA
  fr_supplement <- NA

  ## Capture all referenced section numbers from the Classification section of the supplimentary information
  if(length(xml2::xml_find_all(res, ".//SUPLINF/HD[contains(text(), 'Classification')]")) > 0) {
    regx <- sprintf("(%s\\.[[:digit:]]{1,4})", cfr_part)
    classification <- res %>%
      xml2::xml_find_all(".//SUPLINF/HD[contains(text(), 'Classification')]/following-sibling::P") %>%
      xml2::xml_text() %>%
      stringr::str_match_all(., regx) %>% unlist() %>% unique()

    if(length(classification) == 0){
      classification <- NA
    }
  }
  ## Capture all referenced section numbers from the "List of Subjects" part of the XML
  if(length(xml2::xml_find_all(res, ".//SECTNO")) > 0) {
    sectno <- res %>%
      xml2::xml_find_all(".//SECTNO") %>%
      xml2::xml_text() %>%
      gsub("\u00A7|\u2009|\\s|and", "", .) %>%
      strsplit(., split = ",") %>% unlist()

    if(length(sectno) == 0){
      sectno <- NA
    }
  }
  ## Capture the warning output to go back and check on if there isn't CFR reference or if the reference is hiding in an unknown spot
  if(all(is.na(classification) & is.na(sectno))){
    warning(paste0("No CFR sections found in ", fr_xml_url, ". Part ", cfr_part))
  }

  if(return_summary_text) {
    ## Extract the summary section
    if(length(xml2::xml_find_all(res, ".//SUM")) > 0) {
      fr_summary <- res %>%
        xml2::xml_find_all(".//SUM") %>%
        xml2::xml_text() %>%
        gsub("^SUMMARY:", "", .)

      if(length(fr_summary) == 0)
        fr_summary <- NA
    }

    ## Extract the supplementary information section
    if(length(xml2::xml_find_all(res, ".//SUPLINF/HD[contains(text(), 'SUPPLEMENTARY INFORMATION')]/following-sibling::P")) > 0) {
      fr_supplement <- res %>%
        xml2::xml_find_all(".//SUPLINF/HD[contains(text(), 'SUPPLEMENTARY INFORMATION')]/following-sibling::P") %>%
        xml2::xml_text() %>%
        gsub("^SUPPLEMENTARY INFORMATION:", "", .) %>%
        stringr::str_c(collapse = " ")

      if(fr_supplement == "") {
        fr_supplement <- NA
      }
    }

    ## Capture the warning output to go back and check on if there isn't CFR reference or if the reference is hiding in an unknown spot
    if(all(is.na(fr_summary) & is.na(fr_supplement))){
      warning(paste0("No 'Summary' or 'Supplementary Information' found in ", fr_xml_url, ". Part ", cfr_part))
    }

    summary_df <- tibble::tibble(cfr_section_number = c(classification, sectno), fr_summary, fr_supplement ) %>%
      distinct(.keep_all = TRUE) %>%
      filter(!is.na(cfr_section_number))
  }
  if(!return_summary_text){

    summary_df <- tibble::tibble(cfr_section_number = c(classification, sectno), fr_summary, fr_supplement ) %>%
      distinct(.keep_all = TRUE) %>%
      filter(!is.na(cfr_section_number))
  }

  return(summary_df)
}


get_fr_text <- function(fr_url){
  httr::GET(fr_url) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    rename_with(~gsub("results.", "fr_", .x)) #%>%
    # select(fr_action, fr_type, fr_citation, fr_publication_date, fr_full_text_xml_url, fr_title)
}
