# Code used for selecting articles.

library(ProjectTemplate)
load.project()

i_path <- file.path("") # add path

# 1999 -------
years <- seq(from = 1999, by = 1, to = 1999) 
articles <- purrr::map_df(years, function(year) { 
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  d <- tibble("pdf_name" = fnames)
  d <- dplyr::sample_n(d, size = 14, replace = FALSE)
    # create destination folder and copy selected files
  d_folder <- file.path("src", "articles_step1", year)
  dir.create(d_folder)
  s_folder <- paste(dpath, d$pdf_name, sep = "/")
  file.copy(from = s_folder, to = d_folder)
  return(d)
})

# 2000-2010 -------
years <- seq(from = 2000, by = 1, to = 2010) 
articles <- purrr::map_df(years, function(year) { 
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  d <- tibble("pdf_name" = fnames)
  d <- dplyr::sample_n(d, size = 15, replace = FALSE)
    # create destination folder and copy selected files
  d_folder <- file.path("src", "articles_step1", year)
  dir.create(d_folder)
  s_folder <- paste(dpath, d$pdf_name, sep = "/")
  file.copy(from = s_folder, to = d_folder)
  return(d)
})


# 2011-2020 -------
years <- seq(from = 2011, by = 1, to = 2020) 
articles <- purrr::map_df(years, function(year) { 
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  d <- tibble("pdf_name" = fnames)
  d <- dplyr::sample_n(d, size = 16, replace = FALSE)
  # create destination folder and copy selected files
  d_folder <- file.path("src", "articles_step1", year)
  dir.create(d_folder)
  s_folder <- paste(dpath, d$pdf_name, sep = "/")
  file.copy(from = s_folder, to = d_folder)
  return(d)
})
