# This script performs search on PubMed with the RISmed R library. 

library(ProjectTemplate)
load.project()

# Make sure you set the correct range for the date option!
year = 2020
from_date = paste(year, "01", "01", sep = "/")
# till_date = paste(year, "12", "31", sep = "/")
till_date = paste(year, "07", "31", sep = "/")
# Search 1: variants of repetitive Transcranial Magnetic Stimulation -------
topics_rtms <- c("\"repetitive transcranial magnetic stimulation\" [All Fields]", "\"rTMS\" [All Fields]", "repetitive AND TMS [All Fields]", "rhythmic AND TMS [All Fields]")
search_rtms <- purrr::map_df(topics_rtms , function(topic) { 
  query <- RISmed::EUtilsSummary(query = topic, type = "esearch", db = "pubmed", datetype = 'pdat', mindate = from_date, maxdate = till_date)
  if (query@count == 0) {
    d <- NULL
  } else if (query@count >= 1) {
    Sys.sleep(1) #https://stackoverflow.com/questions/42019878/rismed-package-error-cannot-open-the-connection?rq=1 
    query_new <- RISmed::EUtilsSummary(query = topic, type = "esearch", db = "pubmed", datetype = 'pdat', mindate = from_date, maxdate = till_date, retmax = 10000)
    records <- RISmed::EUtilsGet(query_new, type = "efetch", db = "pubmed")
    d <- data.frame("pmid" = as.numeric(records@PMID),
                    "fauthor" = purrr::map_chr(records@Author, ~ dplyr::first(.x[["LastName"]])),
                    "lang" = records@Language,
                    "srch_term" = "rTMS",
                    "art_typ_cr" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Case Reports"))) %>% as.logical(),
                    "art_typ_rev" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Review"))) %>% as.logical(),
                    "art_typ_srev" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Systematic Review"))) %>% as.logical(),
                    "art_typ_met" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Meta-Analysis"))) %>% as.logical(),
                    "art_typ_com" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Editorial|Comment|Letter"))) %>% as.logical(),
                    "art_typ_vam" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Video-Audio Media"))) %>% as.logical(),
                    "art_typ_err" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Published Erratum"))) %>% as.logical(),
                    "art_typ_ret" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Retraction of Publication"))) %>% as.logical(),
                    "subj_anim" = purrr::map_chr(records@Mesh, function(.x) {
                      if("Heading" %in% names(.x)) {
                        r = any(stringr::str_detect(.x[["Heading"]], "Animals"))
                      } else {
                        r = FALSE
                      }
                      return(r);
                    }) %>% as.logical(),
                    "subj_hum" = purrr::map_chr(records@Mesh, function(.x) {
                      if("Heading" %in% names(.x)) {
                        r = any(stringr::str_detect(.x[["Heading"]], "Humans"))
                      } else {
                        r = FALSE
                      }
                      return(r);
                    }) %>% as.logical())
    d <- d %>% dplyr::mutate(fauthor = as.character(fauthor), srch_term = as.character(srch_term)) %>% dplyr::filter(lang == "eng") %>% dplyr::select(-lang) 
  }
  return(d)
})
search_rtms <- search_rtms %>% dplyr::filter(!is.na(pmid)) %>% unique()

# Search 2: variants of Theta Burst Stimulation -------
topics_tbs <- c("\"theta burst stimulation\" [All Fields]", "theta burst AND stimulation [All Fields]", "\"cTBS\" [All Fields]", "\"iTBS\" [All Fields]")
search_tbs <- purrr::map_df(topics_tbs , function(topic) { 
  query <- RISmed::EUtilsSummary(query = topic, type = "esearch", db = "pubmed", datetype = 'pdat', mindate = from_date, maxdate = till_date)
  if (query@count == 0){
    d <- NULL
  } else if (query@count >= 1){
    Sys.sleep(1) #https://stackoverflow.com/questions/42019878/rismed-package-error-cannot-open-the-connection?rq=1 
    query_new <- RISmed::EUtilsSummary(query = topic, type = "esearch", db = "pubmed", datetype = 'pdat', mindate = from_date, maxdate = till_date, retmax = 10000)
    records <- RISmed::EUtilsGet(query_new, type = "efetch", db = "pubmed")
    d <- data.frame("pmid" = as.numeric(records@PMID),
                    "fauthor" = purrr::map_chr(records@Author, ~ dplyr::first(.x[["LastName"]])),
                    "lang" = records@Language,
                    "srch_term" = "TBS",
                    "art_typ_cr" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Case Reports"))) %>% as.logical(),
                    "art_typ_rev" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Review"))) %>% as.logical(),
                    "art_typ_srev" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Systematic Review"))) %>% as.logical(),
                    "art_typ_met" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Meta-Analysis"))) %>% as.logical(),
                    "art_typ_com" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Editorial|Comment|Letter"))) %>% as.logical(),
                    "art_typ_vam" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Video-Audio Media"))) %>% as.logical(),
                    "art_typ_err" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Published Erratum"))) %>% as.logical(),
                    "art_typ_ret" = purrr::map_chr(records@PublicationType, ~ any(stringr::str_detect(.x, "Retraction of Publication"))) %>% as.logical(),
                    "subj_anim" = purrr::map_chr(records@Mesh, function(.x) {
                      if("Heading" %in% names(.x)) {
                        r = any(stringr::str_detect(.x[["Heading"]], "Animals"))
                      } else {
                        r = FALSE
                      }
                      return(r);
                    }) %>% as.logical(),
                    "subj_hum" = purrr::map_chr(records@Mesh, function(.x) {
                      if("Heading" %in% names(.x)) {
                        r = any(stringr::str_detect(.x[["Heading"]], "Humans"))
                      } else {
                        r = FALSE
                      }
                      return(r);
                    }) %>% as.logical())
    d <- d %>% dplyr::mutate(fauthor = as.character(fauthor), srch_term = as.character(srch_term)) %>% dplyr::filter(lang == "eng") %>% dplyr::select(-lang) 
  }
  return(d)
})
search_tbs <- search_tbs %>% dplyr::filter(!is.na(pmid)) %>% unique()

# Combine the two search results (rTMS and TBS) -------
search_ris <- dplyr::bind_rows(search_rtms, search_tbs) %>% distinct(pmid, .keep_all = TRUE)

# Before running the next script, you have to manually process the PubMed hits. 