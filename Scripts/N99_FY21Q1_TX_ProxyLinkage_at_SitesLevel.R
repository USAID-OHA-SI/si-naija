##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: TX_CURR Results Trend by Gender
##  LICENCE: MIT
##  DATE:    2021-03-08


# DEPENDENCIES ----

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glitr)
  library(glamr)
  library(gisr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(sf)
  library(ggrepel)
  library(ggnewscale)
  library(patchwork)
  library(glue)
  library(ICPIutilities)

# SETUP ----

  # Directories
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_20210212_v1_1_N.*.zip$")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_20210212_v1_1_N.*.zip$")

# LOAD DATA ----

  # Data
  df_site <- file_site_im %>% read_msd()

  df_psnu <- file_psnu_im %>% read_msd()

# MUNGING

  # Available fields
  df_site %>% glimpse()

  df_site %>% distinct(indicator) %>% pull()

  df_site %>% distinct(sitetype) %>% pull()

  # Proxy linkage


  df_site %>%
    filter(fiscal_year == 2021,
           fundingagency == 'USAID',
           psnu == 'Akwa Ibom',
           indicator %in% c('TX_NEW', 'HTS_TST_POS'),
           sitetype %in% c('Facility', 'Community'),
           standardizeddisaggregate == 'Total Numerator') %>%
    group_by(psnu, community, facility, sitetype, mech_name, indicator) %>%
    summarise(qtr1 = sum(qtr1, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = qtr1) %>%
    mutate(linkage = round(TX_NEW / HTS_TST_POS * 100, 2)) %>%
    view()
