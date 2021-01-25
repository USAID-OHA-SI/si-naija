##  PROJECT: TX_CURR Historical Datasets
##  AUTHOR:  jdavis | USAID
##  PURPOSE: TX_CURR PSNU x IM
##  LICENCE: MIT
##  DATE:    2021-01-25


# PACKAGES -------------------------------------------------

library(tidyverse)
library(readxl)
library(glitr)
library(scales)
library(glamr)
library(gisr)
library(janitor)
library(extrafont)

# GLOBAL --------------------------------------------------

  # Load configs
  source("./Scripts/N00_Config.R")

  # file
  file_targets <- list.files(
    path = data,
    pattern = "^Site Tool_Nig.*_\\d{14}_F.*.xlsx$",
    full.names = TRUE
  )

  # Latest MSD PSNU x IM File - Curr release
  file_msd_curr <- list.files(
      path = merdata,
      pattern = "MER_S.*_PSNU_IM_FY18-21_\\d{8}_v.*.*.zip",
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

  # Latest MSD PSNU x IM File - Prev release
  file_msd_prev <- list.files(
      path = merdata,
      pattern = "MER_S.*_PSNU_IM_FY15-17_\\d{8}_v.*.*.zip",
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

  # Latest MSD Site x IM File
  file_msd_sites <- list.files(
      path = merdata,
      pattern = "MER_S.*_Site_IM_.*_\\d{8}_v.*_N.*.zip",
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

# DATA ----------------------------------------------------

  # MSD Data

  # PSNU - Current
  df_msd_curr <- file_msd_curr %>%
    read_msd() %>%
    reshape_msd(clean = TRUE) %>%
    filter(operatingunit == country)

  df_msd_curr %>% glimpse()

  # PSNU - Previous
  df_msd_prev <- file_msd_prev %>%
    read_msd() %>%
    reshape_msd(clean = TRUE) %>%
    filter(operatingunit == country)

  df_msd_prev %>% glimpse()

  df_msd_prev %>%
    filter(indicator == "TX_CURR") %>%
    distinct(fundingagency, period, period_type) %>%
    prinf()

  df_msd_prev %>%
    filter(indicator == "TX_CURR") %>%
    distinct(fundingagency, snu1) %>%
    prinf()

  # PSNU
  df_msd <- df_msd_curr %>%
    bind_rows(df_msd_prev)

  df_msd %>% glimpse()

  df_msd %>%
    distinct(period) %>%
    prinf()

  df_msd_tx <- df_msd %>%
    clean_agency() %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(fundingagency, snu1, indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup()

  df_msd_tx %>% glimpse()

  df_msd_tx %>% distinct(period) %>% pull() %>% sort()

  df_msd_tx_results <- df_msd_tx %>%
    filter(period_type == "results") %>%
    select(-period_type) %>%
    mutate(period = str_replace(period, "FY", "FY20")) %>%
    arrange(period) %>%
    pivot_wider(names_from = period, values_from = val) %>%
    relocate(indicator, .after = fundingagency) %>%
    arrange(fundingagency, indicator, snu1)

  df_msd_tx_results %>% glimpse()

  df_msd_tx_results %>% View()

  # Export results tbl
  write_csv(x = df_msd_tx_results,
            file = file.path(dataout,
                             paste0(country,
                                    " - Historical Treatment Results",
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")

  # Targets
  df_msd_tx_targets <- df_msd_tx %>%
    filter(period_type != "results") %>%
    mutate(period = str_replace(period, "FY", "FY20"),
           period = paste0(period, "_", period_type),) %>%
    select(-period_type) %>%
    arrange(period) %>%
    pivot_wider(names_from = period, values_from = val) %>%
    relocate(indicator, .after = fundingagency) %>%
    arrange(fundingagency, indicator, snu1)

  df_msd_tx_targets %>% glimpse()

  df_msd_tx_targets %>% View()

  # Export targets tbl
  write_csv(x = df_msd_tx_results,
            file = file.path(dataout,
                             paste0(country,
                                    " - Historical Treatment Targets",
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


