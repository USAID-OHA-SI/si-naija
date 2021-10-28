##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: USAID Quarterly Report Cards
##  LICENCE: MIT
##  DATE:    2021-10-22
##  UPDATED: 2021-10-22

# PACKAGES ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glitr)
  library(glamr)
  library(sf)
  library(gisr)
  library(janitor)
  library(gt)

# GLOBAL ----

  # DIR - Global ----
  dir_merdata <- glamr::si_path("path_msd")
  dir_targets <- "../../PEPFAR/COUNTRIES/Nigeria/DataPack"

  # DIR - Project ----
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_geodata <- si_path("path_vector")
  dir_graphics <- "Graphics"

  # Files ----
  file_msd_psnu <- return_latest(
    folderpath = dir_merdata,
    pattern = "PSNU_IM.*_N.*"
  )

  file_msd_nat <- return_latest(
    folderpath = dir_merdata,
    pattern = "NAT_SUBNAT.*"
  )

  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons*"
  )

  # Other params ----
  cntry <- "Nigeria"
  agency <- "USAID"
  inds <- c("TX_CURR", "TX_PVLS")

# DATA ----

  # SUBNAT ----
  df_nat <- file_msd_nat %>% read_msd()

  # Priotization
  df_prio <- get_prioritization(df_nat, 2022)

  # Pops
  pops <- datim_pops(ou = "Nigeria", level = "psnu")

  # PSNU ----
  df_psnu <- file_msd_psnu %>% read_msd()

  df_psnu %<>% clean_agency()
  df_psnu %>% distinct(fiscal_year)
  df_psnu %>% distinct(fundingagency)

  curr_fy <- df_psnu %>% identifypd(pd_type = "year")

  # GEO ----
  spdf_pepfar <- file_shp %>% read_sf()

  spdf_adm0 <- gisr::extract_boundaries(spdf_pepfar, cntry, level = 3)
  spdf_adm1 <- gisr::extract_boundaries(spdf_pepfar, cntry, level = 4)
  spdf_adm2 <- gisr::extract_boundaries(spdf_pepfar, cntry, level = 5)

  spdf_fac <- gisr::extract_locations(cntry, level = 6) %>%
    extract_facilities()

# MUNGE ----

  pops <- pops %>%
    group_by(orgunituid, orgunit, indicator) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    arrange(desc(indicator)) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    mutate(HIV_PREV = round(PLHIV / POP_EST * 100, 1))

  # Prio + Pops
  df_prio <- df_prio %>%
    left_join(pops, by = c("psnuuid" = "orgunituid", "psnu" = "orgunit"))


  df_tx_curr <- df_psnu %>%
    filter()

# VIZ ----

  spdf_adm0 %>% gview()

  spdf_adm1 %>%
    filter(orgunit == "Akwa Ibom") %>%
    gview()

  spdf_adm2 %>%
    st_join(spdf_adm1) %>%

    filter(orgunit == "Akwa Ibom") %>%
    gview()
