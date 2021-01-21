# Code used for extracting information from the articles. The results need to be manually approved.

library(ProjectTemplate)
load.project()

# defined by the user
i_path <- file.path("src", "") # add subfolder for step 1 or 2
o_path <- file.path("src", "")
search_year <- "addYear"
#---------------------------------------
# Motor threshold (MT) method parameters
perc <- c("%", "percent") 
MT <- "resting motor threshold|rest motor threshold|active motor threshold|motor threshold|RMT|AMT|MT"
# Fixed intensity parameters
mso <- c("maximal stimulator output", "maximum stimulator output", "maximal power", "maximum power", "maximal output", "maximum output", "maximal capacity", "maximum capacity", "device capacity", "device's capacity", "machine capacity", "stimulator output", "machine output", "device output", " MSO", "fixed intensity")
# Stimulator
stim <- c("Cadwell", "Brainsway", "Dantec", "Deymed", "eNeura", "EB Neuro", "Langer Medical",
          "Mag and More", "Mag & More", "Magstim", "Mag-Stim", "Magventure", "Medtronic", "Neuronetics", 
          "Nanjing Weisi","Neurosoft", "Neurostar", "Nexstim", "Remed", "Yiruide", "Yunsheng")
f8 <- c("figure-8", "figure-of-8", "figure eight", "figure-eight", "figure-of-eight", "figure of eight", "eight-shaped", "eight shaped", 
        "\uFB01gure-8", "\uFB01gure-of-8", "\uFB01gure eight", "\uFB01gure-eight", "\uFB01gure-of-eight", "\uFB01gure of eight", "butterfly", 
        "H-coil", "H coil", "H1-coil", "H1 coil", "double-cone", "double cone") # unicode character for "fi" is "\uFB01!" <- ouch :D https://www.fileformat.info/info/unicode/char/fb01/index.htm          
# Text to remove/replace
rep_words <- c(" a | an | the | this | is | was | be | or | and | of | so | for | in | on | by | with ") 
rep_sym <- c("- " = "-", "\\(" = " ", "\\)" = " ")
# Number of word between the dose (e.g., 90%) and the threshold (e.g., RMT)
word_dist <- 4 

# Step 1: search for the MT methods -------
mt <- purrr::map_df(search_year, function(year) {
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  mt.year <- purrr::map_df(fnames, function(fname) {
    mt.hit <- purrr::map_df(perc, function(percent) {
      d <- pdfsearch::keyword_search(x = file.path(dpath, fname),
                                     keyword = percent,
                                     path = TRUE,
                                     split_pdf = TRUE,
                                     surround_lines = FALSE,
                                     ignore_case = TRUE,
                                     remove_hyphen = TRUE,
                                     token_results = TRUE) %>%
        dplyr::mutate(pdf_name = fname, year = year, 
                      line_text = stringr::str_replace_all(line_text, regex(rep_words, ignore_case = TRUE), " "),
                      line_text = stringr::str_replace_all(line_text, rep_sym) %>% stringr::str_squish(.),
                      thold = stringr::str_extract_all(line_text, regex(MT, ignore_case = TRUE)),
                      dose = stringr::str_extract_all(line_text, paste("([[:digit:]]+\\.*[[:digit:]]*)[", percent, "]", sep = "")))
      # Check how many times the article mentions phosphene threshold 
      PT <- tibble::tibble(txt = pdftools::pdf_text(file.path(dpath, fname))) %>%
        tidytext::unnest_tokens(word, txt) %>%
        tidytext::unnest_tokens(bigram, word, token = "ngrams", n = 2) %>%
        dplyr::mutate(pho = stringr::str_detect(bigram, regex("phosphene threshold", ignore_case = TRUE))) %>%
        dplyr::filter(pho == TRUE) %>% dplyr::count(pho == TRUE) %>% dplyr::pull(n)  %>% rep(., each = dim(d)[1])
      d <- d %>% dplyr::mutate(pho_thold = dplyr::case_when(!rlang::is_empty(PT) ~ dplyr::first(PT),
                                                            rlang::is_empty(PT) ~ rep(as.integer(0), each = dim(d)[1])))
      return(d)
    })
    return(mt.hit)
  })
  return(mt.year)
  })


# Step 2: Clean results -------
# 2.1. Remove hit (for percent), if the sentence contains no number.  
# E.g., "Stimulus intensities were expressed as a percentage of the subject's resting motor threshold (rmt)."
mt <- mt %>% dplyr::mutate(line = 1:nrow(.))
wanted <- mt %>% dplyr::select(line, pdf_name, dose) %>% tidyr::unnest() %>% dplyr::distinct(line) %>% dplyr::pull(line)
mt <- mt %>% dplyr::filter(line %in% wanted) %>% dplyr::mutate(line = 1:nrow(.))
rm(wanted)

# 2.2. Remove hit (for percent), if the sentence contains no threshold term (e.g., RMT, resting motor threshold, etc.).  
mt <- mt %>% dplyr::group_by(line) %>% dplyr::slice() %>% dplyr::filter(!rlang::is_empty(unlist(thold))) %>% dplyr::ungroup() 

# 2.3. Remove hit (for percent), if the number with percent and threshold information are far away from each other in the sentence
# E.g., "To determine rMT, stimulus intensity gradually increased in steps 5% MSO until TMS constently evoked MEPs..."
lines <- 1:dim(mt)[1]
mt <- purrr::map_df(lines, function(line) {
  d <- mt %>% .[line,] 
  d.dose <- tibble::tibble(words = unlist(stringr::str_split(d$line_text, " "))) %>%
    dplyr::mutate(loc = 1:nrow(.)) %>% dplyr::filter(words %in% unlist(d$dose))
  d.mt <- tibble::tibble(words = unlist(stringr::str_split(d$line_text, " "))) %>%
    dplyr::mutate(loc = 1:nrow(.)) 
  # Note that there might be multiple threshold terms in one sentence (e.g., RMT or resting motor threshold)
  thresholds <- unlist(d$thold) %>% unlist(stringr::str_split(., "[ ]")) %>% stringr::str_split(., "[ ]") %>% 
    purrr::map(dplyr::first) %>% unlist() # extract the first word from each threshold term
  d.mt <- d.mt %>% dplyr::mutate(filt = stringr::str_detect(words, paste0(thresholds, collapse = "|"))) %>% 
    dplyr::filter(filt == TRUE) %>% dplyr::select(-filt)
  
  if (dim(d.mt)[1] == 0 | dim(d.dose)[1] == 0) {
    df <- d %>% dplyr::mutate(dose_nr = NA)
  } else {
    res <- outer(d.mt$loc, d.dose$loc, FUN = "-")
    location <- which(res > 0 & res < word_dist, arr.ind = TRUE) %>% unname() %>% .[, 2]
    df <- d %>% dplyr::mutate(dose_nr = ifelse(length(location) >= 1, list(location), NA))
  }
  # If dose_sent is longer than 1, there are multiple possible dose values in one sentence
  df <- df %>% dplyr::mutate(dose_sent = length(unlist(dose_nr)))
  return(df)
})
rm(lines)
mt <- mt %>% dplyr::filter(dose_nr != "NULL") %>% dplyr::mutate(line = 1:nrow(.))

# 2.4. If there are multiple hits for a particular article, compare the dose values and mark if those dose values are different
fnames_temp <- mt %>% distinct(pdf_name) %>% pull(pdf_name)
mt <- purrr::map_df(fnames_temp, function(fname) {
  d <- mt %>% dplyr::filter(pdf_name == fname)
  dose_match <- unlist(stringr::str_match_all(unlist(d$dose)[1], unlist(d$dose)))
  d <- d %>% dplyr::mutate(dose_doc = dplyr::case_when(length(unlist(d$dose)) == 1 ~ 1,
                                                       length(unlist(d$dose)) == length(dose_match) ~ 1,
                                                       length(unlist(d$dose)) != length(dose_match) ~ 2))
  return(d)
})
rm(fnames_temp)

out_mt <- mt %>% dplyr::select(pdf_name, thold, dose) %>% dplyr::mutate(line = 1:nrow(.)) %>% 
  dplyr::group_by(line) %>% dplyr::slice() %>% dplyr::mutate(thold = paste(unlist(thold), collapse = ", "), 
                                                             dose = paste(unlist(dose), collapse = ", ")) %>% 
  dplyr::ungroup() %>% dplyr::select(-line) %>% dplyr::distinct()
writexl::write_xlsx(out_mt, path = file.path(o_path, paste("1_motor_thold_", search_year, ".xlsx", sep = "")),
                    col_names = TRUE, format_headers = TRUE)

# Search for the fixed intensity method
fxd <- purrr::map_df(search_year, function(year) {
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  fxd.year <- purrr::map_df(fnames, function(fname) {
    fxd.hit <- purrr::map_df(mso, function(mso) {
      d <- pdfsearch::keyword_search(x = file.path(dpath, fname),
                                     keyword = mso,
                                     path = TRUE,
                                     split_pdf = TRUE,
                                     surround_lines = FALSE,
                                     ignore_case = TRUE,
                                     remove_hyphen = TRUE,
                                     token_results = TRUE) %>%
        dplyr::mutate(pdf_name = fname, year = year)
      return(d)
      })
    return(fxd.hit)
    })
    return(fxd.year)
  })

out_fxd <- fxd %>% dplyr::select(pdf_name, keyword) %>% dplyr::distinct()
writexl::write_xlsx(out_fxd, path = file.path(o_path, paste("2_mso_", search_year, ".xlsx", sep = "")),
                    col_names = TRUE, format_headers = TRUE)

# Search for stimulator
stimulator <- purrr::map_df(search_year, function(year) {
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  s.year <- purrr::map_df(fnames, function(fname) {
    s.hit <- purrr::map_df(mso, function(mso) {
      d <- pdfsearch::keyword_search(x = file.path(dpath, fname),
                                     keyword = stim,
                                     path = TRUE,
                                     split_pdf = TRUE,
                                     surround_lines = FALSE,
                                     ignore_case = TRUE,
                                     remove_hyphen = TRUE,
                                     token_results = TRUE) %>%
        dplyr::mutate(pdf_name = fname, year = year)
      return(d)
    })
    return(s.hit)
  })
  return(s.year)
})

out_stim <- stimulator %>% dplyr::select(pdf_name, keyword) %>% dplyr::distinct()
writexl::write_xlsx(out_stim, path = file.path(o_path, paste("3_stim_", search_year, ".xlsx", sep = "")),
                    col_names = TRUE, format_headers = TRUE)

# Search for coils
coil <- purrr::map_df(search_year, function(year) {
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  c.year <- purrr::map_df(fnames, function(fname) {
    c.hit <- purrr::map_df(mso, function(mso) {
      d <- pdfsearch::keyword_search(x = file.path(dpath, fname),
                                     keyword = f8,
                                     path = TRUE,
                                     split_pdf = TRUE,
                                     surround_lines = FALSE,
                                     ignore_case = TRUE,
                                     remove_hyphen = TRUE,
                                     token_results = TRUE) %>%
        dplyr::mutate(pdf_name = fname, year = year)
      return(d)
    })
    return(c.hit)
  })
  return(c.year)
})

out_coil<- coil %>% dplyr::select(pdf_name, keyword) %>% dplyr::distinct()
writexl::write_xlsx(out_coil, path = file.path(o_path, paste("4_coil_", search_year, ".xlsx", sep = "")),
                    col_names = TRUE, format_headers = TRUE)

# Prepare dataframe for data input
dpath <- file.path(i_path, search_year)
fnames <- list.files(dpath, pattern = "*.pdf")
df <- data.frame("pdf_name" = fnames,
                 "typ" = NA,
                 "frq" = NA,
                 "si_app" = NA,
                 "th_strat" = NA,
                 "th_meas" = NA,
                 "mt_uV" = NA,
                 "th_ri" = NA,
                 "mt_contr" = NA,
                 "si_pct" = NA,
                 "si_pct_min" = NA,
                 "si_pct_max" = NA,
                 "mso" = NA,
                 "stim_co" = NA,
                 "stim_mo" = NA,
                 "coil_shp" = NA,
                 "coil_siz" = NA,
                 "coil_mo" = NA, 
                 "note" = NA,
                 "contrib_id" = NA)
writexl::write_xlsx(df, path = file.path(o_path, paste("5_input_", search_year, ".xlsx", sep = "")),
                    col_names = TRUE, format_headers = TRUE)
