##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: DataPack COP21 / FY22 Data
##  LICENCE: MIT
##  DATE:    2021-05-10

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
  library(tidytext)
  library(glue)
  library(ICPIutilities)
  library(tameDP)

# SETUP ----

  # Directories
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"

  dir_merdata <- si_path("path_msd")

  # COP21 - FY22 Targets
  file_dp <- dir_data %>%
    paste0("/DP") %>%
    return_latest(pattern = ".*_COP21_DP_05_10_.*.xlsx")

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

# LOAD DATA ----

  ## FY21 Targets
  df_targets <- file_psnu_im %>% read_msd()

  df_targets %>% glimpse()

  df_targets %>%
    distinct(indicator) %>%
    arrange(indicator) %>% prinf()

  df_targets %>%
    filter(fiscal_year == 2021,
           standardizeddisaggregate == 'Total Numerator') %>%
    group_by(fiscal_year, fundingagency, psnu, indicator) %>%
    summarise(across(targets, sum, na.rm = TRUE)) %>%
    ungroup() %>% view()




  ## FY22 Targets

  key_cols <- c("psnu", "psnuuid", "indicator_code",
                "indicatortype", "age", "sex", "keypop")

  #file_dp %>% tame_dp(psnu_lvl = TRUE) %>% prinf()

  df_dp <- file_dp %>% tame_dp()

  df_dp <- df_dp %>%
    clean_agency() %>%
    filter(!str_detect(str_to_lower(fundingagency), "^dedup.*"))

  df_dp %>% glimpse()

  df_dp %>% head() %>% view()

  df_dp %>% distinct(fundingagency) %>% pull()
  df_dp %>% distinct(psnu) %>% pull()

  df_dp %>% distinct(indicator) %>%
    arrange(indicator) %>%
    prinf()

  df_dp %>% distinct(disagg)
  df_dp %>% distinct(numeratordenom)

  df_dp %>%
    filter(disagg == 'Total') %>%
    distinct(numeratordenom)

  df_dp %>%
    filter(disagg == 'Total') %>%
    distinct(indicator)

  df_dp %>%
    filter(indicator %in% c('HTS_TST', 'OVC_HIVSTAT')) %>%  view()


  df_dp %>%
    select(fundingagency, psnu) %>%
    distinct() %>%
    arrange(psnu) %>%
    prinf()

  df_dp %>%
    group_by(fundingagency, psnu, indicator) %>%
    summarise(across(targets, sum, na.rm = TRUE)) %>%
    ungroup() %>% view()


  df_dp %>%
    group_by(fundingagency, psnu, indicator) %>%
    summarise(across(targets, sum, na.rm = TRUE)) %>%
    ungroup() %>% view()













