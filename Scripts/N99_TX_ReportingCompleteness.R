##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: TX_CURR - Reporting Completeness
##  LICENCE: MIT
##  DATE:    2021-07-12


# DEPENDENCIES ----

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(ICPIutilities)
  library(glitr)
  library(glamr)
  library(gisr)
  library(sf)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(tidytext)
  library(glue)
  library(gt)


# SETUP ----

  # Directories
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  ## Reporting Filters
  rep_agency = "USAID"
  rep_agencies <- c("USAID", "CDC")

  rep_fy <- NULL
  rep_pd <- NULL

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im_i <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v1_\\d{1}_N.*.zip$")

  file_site_im_c <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v2_\\d{1}_N.*.zip$")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

# LOAD DATA ----

  # Site Data
  df_site_i <- file_site_im_i %>%
    read_msd() %>%
    clean_agency()

  df_site_c <- file_site_im_c %>%
    read_msd() %>%
    clean_agency()

  df_site_i %>% glimpse()

  # Current Reporting Period (from dataset)
  rep_fy <- df_site_i %>% identifypd(pd_type = "year")
  rep_pd <- df_site_i %>% identifypd(pd_type = "full")
  rep_qtr <- df_site_i %>% identifypd(pd_type = "quarter")

  # PSNU FY21 Targets
  df_psnu <- file_psnu_im %>%
    read_msd() %>%
    clean_agency()

  df_tx_psnu <- df_psnu %>%
    filter(fiscal_year == rep_fy,
           indicator %in% c('TX_CURR', 'TX_NEW', 'TX_RTT', 'TX_NET_NEW'),
           standardizeddisaggregate == 'Total Numerator') %>%
    group_by(fundingagency, mech_code, mech_name, primepartner, psnu, indicator) %>%
    summarize(across(targets, sum, na.rm = TRUE))


# Treatment Sites

  # MSDi
  df_tx_sites_i <- df_site_i %>%
    filter(fiscal_year == rep_fy,
           indicator %in% c('TX_CURR', 'TX_NEW', 'TX_RTT', 'TX_NET_NEW'),
           standardizeddisaggregate == 'Total Numerator',
           sitename != 'Data reported above Site level') %>%
    group_by(fundingagency, mech_code, psnu, sitename, indicator) %>%
    summarise(across(cumulative, sum, na.rm = TRUE)) %>%
    ungroup()

  df_tx_sites_i %>%
    left_join(df_tx_psnu,
              by = c("fundingagency", "mech_code", "psnu", "indicator")) %>%
    filter(indicator == 'TX_CURR') %>%
    relocate(cumulative, .before = targets) %>%
    group_by(fundingagency, mech_code, mech_name, primepartner, psnu, indicator) %>%
    summarise(sites_nor = n_distinct(sitename[cumulative == 0]),
              sites_all = n_distinct(sitename),
              completeness = percent((sites_all - sites_nor) / sites_all, 0.1),
              psnu_targets = sum(targets, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(sites_nor > 0) %>%
    select(-c(mech_code, mech_name)) %>%
    gt()

  # MSDc
  df_tx_sites_c <- df_site_c %>%
    filter(fiscal_year == rep_fy,
           indicator %in% c('TX_CURR', 'TX_NEW', 'TX_RTT', 'TX_NET_NEW'),
           standardizeddisaggregate == 'Total Numerator',
           sitename != 'Data reported above Site level') %>%
    group_by(fundingagency, mech_code, psnu, sitename, indicator) %>%
    summarise(across(qtr1:cumulative, sum, na.rm = TRUE)) %>%
    ungroup()

  df_tx_sites_c %>%
    left_join(df_tx_psnu,
              by = c("fundingagency", "mech_code", "psnu", "indicator")) %>%
    filter(indicator == 'TX_CURR') %>%
    relocate(cumulative, .before = targets) %>%
    group_by(fundingagency, mech_code, psnu, indicator) %>%
    summarise(across(qtr1:cumulative, sum, na.rm = T)) %>%
    gt()

  df_tx_sites_c %>%
    left_join(df_psnu,
              by = c("fundingagency", "mech_code", "psnu", "indicator")) %>%
    filter(indicator == 'TX_CURR') %>%
    relocate(cumulative, .before = targets) %>%
    group_by(fundingagency, mech_code, mech_name, primepartner, psnu, indicator) %>%
    summarise(sites_nor = n_distinct(sitename[cumulative == 0]),
              sites_all = n_distinct(sitename),
              completeness = percent((sites_all - sites_nor) / sites_all, 0.1),
              targets = sum(targets, na.rm = TRUE),
              results = sum(cumulative, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(sites_nor > 0) %>%
    select(-c(mech_code, mech_name)) %>%
    gt()
