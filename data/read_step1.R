
dpath <- file.path("data", "step1")
files <- list.files(dpath, pattern = "*.xlsx")

step1 <- purrr::map_df(files, function(file) {
  d <- readxl::read_excel(path = file.path(dpath, file), col_names = TRUE)
  d <- d %>% dplyr::mutate(pdf_name = as.factor(pdf_name),
                           typ = as.factor(typ),
                           frq = as.numeric(frq),
                           si_app = as.character(si_app),
                           th_strat = as.factor(th_strat),
                           th_meas = as.factor(th_meas),
                           mt_uV = as.numeric(mt_uV),
                           th_ri = as.factor(th_ri),
                           mt_contr = as.factor(mt_contr),
                           si_pct = as.numeric(si_pct),
                           si_pct_min = as.numeric(si_pct_min),
                           si_pct_max = as.numeric(si_pct_max),
                           mso = as.numeric(mso),
                           stim_co = as.factor(stim_co),
                           stim_mo = as.factor(stim_mo),
                           coil_shp = as.factor(coil_shp),
                           coil_siz = as.factor(coil_siz),
                           coil_mo = as.factor(coil_mo),
                           note = as.character(note),
                           contrib_id = as.factor(contrib_id))
  return(d)
  })

rm(dpath, files)
