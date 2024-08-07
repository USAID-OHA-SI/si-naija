##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: PANO Data Extraction
##  LICENCE: MIT
##  DATE:    2021-11-22
##  UPDATED: 2024-04-23

# PACKAGES ----

library(tidyverse)
library(gagglr)

# Params & Options ----

options(digits = 7)

dir_mer <- glamr::si_path("path_msd")

file_ou1 <- dir_mer %>% return_latest("OU_IM_FY15")
file_ou2 <- dir_mer %>% return_latest("OU_IM_FY22")

meta <- get_metadata(file_ou2)

# Data ----

# USAID Targets
df_usaid <- file_ou1 %>%
  c(file_ou2) %>%
  map_dfr(function(.x) {
    read_psd(.x) %>%
      filter(
        fiscal_year == meta$curr_fy,
        str_detect(indicator, "TX_CURR|PrEP|PREV|OVC_SERV"),
        str_detect(standardizeddisaggregate, "Total N|Age/Sex|KeyPop"),
      )
  })

df_usaid %>%
  distinct(indicator, standardizeddisaggregate) %>%
  prinf()


df_usaid %>%
  filter(indicator %in% c("TX_CURR", "OVC_SERV_UNDER_18",
                          "PrEP_CT", "PrEP_NEW", "KP_PREV"),
         standardizeddisaggregate %in% c("Total Numerator")) %>%
  summarise(targets = sum(targets, na.rm =T),
            .by = c(fiscal_year, funding_agency, indicator)) %>%
  arrange(indicator, desc(targets)) %>%
  prinf()


## OVC SERV
df_ovc <- si_path() %>%
  return_latest("PSNU_IM_FY21.*_Nig") %>%
  read_psd() %>%
  filter(fiscal_year == meta$curr_fy,
         operatingunit == cntry,
         funding_agency == agency,
         str_detect(indicator, "OVC_"))

df_ovc %>% glimpse()

df_ovc %>% distinct(indicator)

df_ovc_serv <- df_ovc %>%
  filter(standardizeddisaggregate == "Total Numerator") %>%
  summarise(across(cumulative, \(x) sum(x, na.rm = T)),
            .by = c(fiscal_year, funding_agency, country, indicator))


# HTS

  df_hts <- si_path() %>%
    return_latest("PSNU_IM_FY21.*_Nig") %>%
    read_psd() %>%
    filter(#fiscal_year == meta$curr_fy,
           operatingunit == cntry,
           funding_agency %ni% c("Dedup", "Default"),
           str_detect(indicator, "HTS_"))


  df_hts %>% distinct(indicator)
  df_hts %>% distinct(indicator, standardizeddisaggregate)


  df_hts %>% distinct(modality) %>% pull()

  df_hts_idx <- df_hts %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           modality %in% c("Index", "IndexMod"),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, indicator, modality))

  df_hts_idx <- df_hts_idx %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, indicator, modality)) %>%
    mutate(funding_agency = "OU") %>%
    bind_rows(df_hts_idx, .)

  df_hts_idx <- df_hts_idx %>%
    rowwise() %>%
    mutate(achievement = cumulative / targets * 100) %>%
    ungroup() %>%
    pivot_longer(names_to = "metric", values_to = "value",
                 cols = c(cumulative, targets, achievement))

  df_hts_idx %>% glimpse()

  df_hts_idx <- df_hts_idx %>%
    filter(metric == 'cumulative') %>%
    group_by(fiscal_year, funding_agency, modality) %>%
    summarise(positivity = (value[indicator == 'HTS_TST_POS'] /
                         value[indicator == 'HTS_TST'] * 100)) %>%
    ungroup() %>%
    rename(value = positivity) %>%
    mutate(metric = "positivity", indicator = NA) %>%
    bind_rows(df_hts_idx, .) %>%
    select(fiscal_year, funding_agency, modality, metric, indicator, value) %>%
    arrange(fiscal_year, funding_agency, modality, metric) %>%
    pivot_wider(names_from = fiscal_year, values_from = value)

  df_hts_idx %>%
    clean_agency() %>%
    write_csv(na = "",
              file = file.path(dir_dataout, "Nigeria - HTS Historical Performance.csv"))




