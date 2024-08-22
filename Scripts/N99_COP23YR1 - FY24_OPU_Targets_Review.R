# PROJECT: si-naija
# PURPOSE: COP23YR1 - OPU Target Reviews
# AUTHOR: Baboyma Kagniniwa | USAID/GH - Office of HIV-AIDS
# LICENSE: MIT
# REF. ID: d135ded1
# CREATED: 2024-08-20
# UPDATED: 2024-08-20
# NOTES:

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(grabr)
  library(glamr)
  library(gophr)
  library(glue)
  library(janitor)

# Set paths  ====

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_datim <- glamr::si_path("path_datim")

  dir_cntry <- "../../PEPFAR/COUNTRIES/Nigeria"

# Params ====

  ref_id <- "d135ded1"
  ou <-  "Nigeria"
  cntry <- ou
  agency <- "USAID"

# Files ====

  file_psnu <- dir_mer %>% return_latest(glue("PSNU_IM_FY22.*_{cntry}"))

  file_opu <- dir_cntry %>%
    file.path("COPs/OPUs/COP23-FY24-Year 1") %>%
    return_latest(glue("PSNUxIM"))

  meta <- get_metadata(file_psnu)

# Functions  ====

  name <- function(variables) {

  }

# LOAD DATA ====

  df_psnu <- file_psnu %>%
    read_psd() %>%
    filter(fiscal_year == meta$curr_fy)

# MUNGE ====

  df_psnu %>% glimpse()

  ## Ref file

  df_psnu %>%
    distinct(fiscal_year, psnu, funding_agency) %>%
    relocate(fiscal_year, .after = last_col()) %>%
    write_csv(na = "", file = file.path(dir_dataout, "FY24_Nigeria_PSNU_distributions.csv"))

  ## Agencies summary
  df_agency <- df_psnu %>%
    filter(str_detect(standardizeddisaggregate, "Total")) %>%
    clean_indicator() %>%
    summarise(targets = sum(targets, na.rm = T),
              .by = c(fiscal_year, funding_agency, indicator))

  df_agency <- df_agency %>%
    summarise(targets = sum(targets, na.rm = T),
              .by = c(fiscal_year, indicator)) %>%
    mutate(funding_agency = "OU") %>%
    bind_rows(df_agency, .) %>%
    filter(targets > 0) %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)

  df_agency %>%
    write_csv(na = "", file = file.path(dir_dataout, "FY24_Nigeria_Agency_targets_initial.csv"))

  ## PSNU Summary
  df_psnu_agency <- df_psnu %>%
    filter(str_detect(standardizeddisaggregate, "Total")) %>%
    clean_indicator() %>%
    summarise(targets = sum(targets, na.rm = T),
              .by = c(fiscal_year, funding_agency, psnu, indicator)) %>%
    filter(targets > 0) %>%
    pivot_wider(names_from = indicator, values_from = targets)

  df_psnu_agency %>%
    write_csv(na = "", file = file.path(dir_dataout, "FY24_Nigeria_psnu_targets_initial.csv"))

# VIZ ====



# EXPORT ====

