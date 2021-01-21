# Code used for selecting articles.

library(ProjectTemplate)
load.project()

i_path <- file.path("") # add path
i_path_s1 <- file.path("") # path for the first 380 articles

# 1999 -------
years <- 1999 
articles <- purrr::map_df(years, function(year) { 
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  d <- tibble("pdf_name" = fnames)
  # remove those articles we selected in step 1
  dpath_s1 <- file.path(i_path_s1, year)
  fnames_s1 <- list.files(dpath_s1, pattern = "*.pdf")
  diff <- data.frame("pdf_name" = setdiff(d$pdf_name, fnames_s1))
  d <- dplyr::sample_n(diff, size = 13, replace = FALSE)
  # create destination folder and copy selected files
  d_folder <- file.path("src", "articles_step2", year)
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
  # remove those articles we selected in step 1
  dpath_s1 <- file.path(i_path_s1, year)
  fnames_s1 <- list.files(dpath_s1, pattern = "*.pdf")
  diff <- data.frame("pdf_name" = setdiff(d$pdf_name, fnames_s1))
  d <- dplyr::sample_n(diff, size = 15, replace = FALSE)
  # create destination folder and copy selected files
  d_folder <- file.path("src", "articles_step2", year)
  dir.create(d_folder)
  s_folder <- paste(dpath, d$pdf_name, sep = "/")
  file.copy(from = s_folder, to = d_folder)
  return(d)
})

# 2011-2019 -------
years <- seq(from = 2011, by = 1, to = 2019) 
articles <- purrr::map_df(years, function(year) { 
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  d <- tibble("pdf_name" = fnames)
  # remove those articles we selected in step 1
  dpath_s1 <- file.path(i_path_s1, year)
  fnames_s1 <- list.files(dpath_s1, pattern = "*.pdf")
  diff <- data.frame("pdf_name" = setdiff(d$pdf_name, fnames_s1))
  d <- dplyr::sample_n(diff, size = 20, replace = FALSE)
  # create destination folder and copy selected files
  d_folder <- file.path("src", "articles_step2", year)
  dir.create(d_folder)
  s_folder <- paste(dpath, d$pdf_name, sep = "/")
  file.copy(from = s_folder, to = d_folder)
  return(d)
})

# 2020 -------
years <- 2020 
articles <- purrr::map_df(years, function(year) { 
  dpath <- file.path(i_path, year)
  fnames <- list.files(dpath, pattern = "*.pdf")
  d <- tibble("pdf_name" = fnames)
  # remove those articles we selected in step 1
  dpath_s1 <- file.path(i_path_s1, year)
  fnames_s1 <- list.files(dpath_s1, pattern = "*.pdf")
  diff <- data.frame("pdf_name" = setdiff(d$pdf_name, fnames_s1))
  d <- dplyr::sample_n(diff, size = 22, replace = FALSE)
  # create destination folder and copy selected files
  d_folder <- file.path("src", "articles_step2", year)
  dir.create(d_folder)
  s_folder <- paste(dpath, d$pdf_name, sep = "/")
  file.copy(from = s_folder, to = d_folder)
  return(d)
})
