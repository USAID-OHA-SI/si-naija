##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: Update TX Targets
##  LICENCE: MIT
##  DATE:    2021-02-05


# PACKAGES -------------------------------------------------

library(tidyverse)
library(readxl)
library(glitr)
library(scales)
library(glamr)
library(gisr)
library(janitor)
library(extrafont)
library(ICPIutilities)

# GLOBAL --------------------------------------------------

  # Load configs
  source("./Scripts/N00_Config.R")

  # Country name
  country <- "Nigeria"

  # file
  file_fy20_targets <- return_latest(
      folderpath = data,
      pattern = "^Site Tool_Nig.*_\\d{14}_F.*.xlsx$"
    )

  # file
  file_fy21_targets <- return_latest(
    folderpath = data,
    pattern = "FY21 DATIM.*Nov 2020.xlsx$"
  )

  # Latest MSD PSNU x IM File
  file_msd <- list.files(
      path = merdata,
      pattern = "MER_S.*_PSNU_IM_.*_\\d{8}_v.*_N.*.zip",
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


  # PSNU
  df_msd <- file_msd %>%
    read_msd() %>%
    reshape_msd(clean = TRUE)

  df_msd %>% glimpse()

  # MSD PSNU Targets
  df_msd_psnu_trgts <- df_msd %>%
    clean_agency() %>%
    filter(operatingunit == country,
           fundingagency %in% c("USAID", "CDC"),
           indicator %in% c("TX_CURR", "TX_NEW"),
           period == "FY20",
           period_type %in% c("targets", "cumulative"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(psnu, indicator, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup()

  df_msd_psnu_trgts %>% glimpse()

  # MSD IM Targets
  df_msd_im_trgts <- df_msd %>%
    filter(operatingunit == country,
           #fundingagency == "USAID",
           fundingagency %in% c("USAID", "CDC"),
           indicator %in% c("TX_CURR", "TX_NEW"),
           period == "FY20",
           period_type %in% c("targets", "cumulative"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(mech_code, indicator, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup()

  df_msd_im_trgts %>% glimpse()



  # Sites
  df_msd_sites <- file_msd_sites %>%
    read_msd() %>%
    reshape_msd(clean = TRUE)

  df_msd_sites <- df_msd_sites %>%
    filter(str_to_lower(fundingagency) != "dedup")

  df_msd_sites %>% glimpse()

  # IMs
  df_ims <- df_msd_sites %>%
    filter(period_type == "targets",
           period %in% c("FY20", "FY21")) %>%
    select(period, primepartner, mech_code, mech_name) %>%
    distinct(period, primepartner, mech_code, mech_name) %>%
    arrange(period, mech_code)

  df_ims %>% prinf()


  # FY20 Updated targets

  df_targets <- file_fy20_targets %>%
    read_excel(sheet = "TX", skip = 4) %>%
    clean_names() %>%
    rename(tx_curr_n = tx_curr_n_age_sex_hiv_status_20t,
           tx_new_n = tx_new_n_age_sex_hiv_status_20t,
           tx_pvls_n = tx_pvls_n_age_sex_indication_hiv_status_20t_routine,
           tx_pvls_d = tx_pvls_d_age_sex_indication_hiv_status_20t_routine)

  df_targets %>% glimpse()

  df_trgts <- df_targets %>%
    separate(site, into = c("psnu", "facility"), sep = " > ", remove = FALSE) %>%
    mutate(site_type = str_extract(site, " (?<=\\[#).*(?=\\] )"),
           facility = if_else(is.na(facility), psnu, facility),
           psnu = if_else(str_detect(psnu, "_Mil"), "_Military Nigeria", psnu)) %>%
    separate(facility, into = c("facility", NA, "orgunituid"), sep = " \\[") %>%
    mutate_at(vars(facility, orgunituid), str_remove, pattern = "\\]") %>%
    mutate(mechanism = str_replace(mechanism, " - ", " -- ")) %>%
    separate(mechanism, into = c("mech_code", "partner"), sep = " -- ", remove = F) %>%
    mutate(mechanism = str_replace(mechanism, " -- ", " - ")) %>%
    relocate(site_type, .after = site) %>%
    pivot_longer(cols = starts_with("tx_"),
                 names_to = "indicator",
                 values_to = "val") %>%
    mutate(denominatornumerator = str_sub(indicator, -1),
           denominatornumerator = str_to_upper(denominatornumerator)) %>%
    mutate(indicator = str_sub(indicator, 1, -3),
           indicator = str_to_upper(indicator),
           period_type = "cop19") %>%
    relocate(val, .after = last_col())


  df_trgts %>% glimpse()

  View(df_trgts)

  # PSNU targets
  df_psnu_trgts <- df_trgts %>%
    filter(status == "Active") %>%
    group_by(psnu, indicator, period_type, denominatornumerator) %>%
    summarise_at(vars(val), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           denominatornumerator == "N") %>%
    select(-denominatornumerator) %>%
    bind_rows(df_msd_psnu_trgts) %>%
    mutate(indicator = paste0(indicator, "_", period_type),
           indicator = str_to_lower(indicator)) %>%
    select(-period_type) %>%
    pivot_wider(names_from = indicator,
                values_from = val) %>%
    mutate(
      tx_curr_ach1 = round(tx_curr_cumulative / tx_curr_targets * 100),
      tx_curr_ach2 = round(tx_curr_cumulative / tx_curr_cop19 * 100),
      tx_new_ach1 = round(tx_new_cumulative / tx_new_targets * 100),
      tx_new_ach2 = round(tx_new_cumulative / tx_new_cop19 * 100)
    ) %>%
    relocate(tx_curr_cumulative, tx_curr_targets, tx_curr_cop19,
             tx_curr_ach1, tx_curr_ach2,
             tx_new_cumulative, tx_new_targets, tx_new_cop19,
             tx_new_ach1, tx_new_ach2,
             .after = psnu)

  df_psnu_trgts %>% glimpse()

  df_psnu_trgts %>% prinf()

  #df_psnu_trgts %>% View()


  # FY21 Targets

  df_fy21_targets <- file_fy21_targets %>%
    read_excel(sheet = 3, skip = 4)

  df_fy21_targets <- df_fy21_targets %>%
    pivot_longer(cols = -c(State, Mech, `Partner Name`),
                 names_to = "indicator",
                 values_to = "val") %>%
    clean_names() %>%
    rename(primepartner = partner_name)

  df_fy21_targets %>% glimpse()

