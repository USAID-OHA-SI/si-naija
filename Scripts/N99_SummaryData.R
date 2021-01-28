##  PROJECT: Performance Summary Tables
##  AUTHOR:  J.Davis, B.Kagniniwa | USAID
##  PURPOSE: Performance Summary Tables
##  LICENCE: MIT
##  DATE:    2020-6-16
##  UPDATED: 2021-01-19


# PACKAGES -------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(glamr)

# GLOBAL --------------------------------------------------

  # DIRs

  merdata <- glamr::si_path("path_msd")
  data <- "./Data"
  dataout <- "./Dataout"
  images <- "./Images"
  graphics <- "./Graphics"

  # OU/Country

  country <- "Nigeria"

  agencies <- c("USAID", "HHS/CDC")

  agency <- agencies %>% first()

  curr_pd <- 4

  curr_fy <- 2020

  fiscal_years <- c(curr_fy - 1, curr_fy)


  # Latest MSD File
  file_msd <- list.files(
      path = merdata,
      pattern = "MER_S.*_PSNU_IM_.*_\\d{8}_v.*_N.*.zip",
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

# Indicators ----------------------------------------------------

  # Indicators of interest
  ind_list1 <- c("TX_CURR",
                "TX_NET_NEW",
                "TX_NEW",
                "HTS_TST",
                "HTS_TST_POS",
                "TX_RTT",
                "TX_ML",
                "OVC_SERV",
                "PrEP_CURR",
                "PrEP_NEW")

  # Used to order indicators
  ind_list2 <- c("TX_CURR",
                 "TX_NET_NEW",
                 "retention",
                 "TX_NEW",
                 "linkage",
                 "HTS_TST",
                 "HTS_TST_POS",
                 "positivity",
                 "TX_PVLS_D",
                 "coverage",
                 "suppression",
                 "TX_ML",
                 "TX_RTT",
                 "OVC_SERV",
                 "PrEP_CURR",
                 "PrEP_NEW")


  # Periods order
  pd_list <- c("fy2019q1",
               "fy2019q2",
               "fy2019q3",
               "fy2019q4",
               "fy2019cumulative",
               "fy2019_targets",
               "fy2020q1",
               "fy2020q2",
               "fy2020q3",
               "fy2020q4",
               "fy2020cumulative",
               "fy2020_targets")

  # TODO:
  qtrs <- 1:4
  refs <- c("cumulative", "_targets")

  pds <- qtrs %>%
    paste0("q", .) %>%
    append(refs)

  pds_list <- cross3("fy", fiscal_years, pds) %>%
    map_chr(.x, .f = ~ paste0(.x, collapse = ""))

  pds_list


# DATA ------------------------------------------------------

  # MSD
  df <- file_msd %>% read_msd()

  #df %>% glimpse()

  #step 1: everything other than vl/retention

  df1 <- df %>%
    filter(operatingunit == country,
           indicator %in% ind_list1,
           fiscal_year %in% fiscal_years,
           fundingagency != "Dedup") %>%
    dplyr::select(fundingagency, primepartner, snu1, psnu, psnuuid,
                  standardizeddisaggregate, otherdisaggregate, sex,
                  trendsfine, indicator, fiscal_year, targets:cumulative) %>%
    group_by(fundingagency, primepartner, snu1, psnu, psnuuid,
             standardizeddisaggregate, otherdisaggregate, sex,
             trendsfine, fiscal_year, indicator) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd(direction = "long")

  df1 %>% glimpse()

  #step 2: retention/vl

  df_vl <- df %>%
    filter(operatingunit == country,
           indicator %in% c("TX_CURR", "TX_PVLS"),
           fiscal_year %in% fiscal_years,
           fundingagency != "Dedup") %>%
    mutate(indicator = case_when(
             numeratordenom %in% c("N", "D") ~ paste0(indicator, "_", numeratordenom),
             TRUE ~ indicator
           ),
           indicator = case_when(
             indicator == "TX_CURR_N" ~ "TX_CURR",
             TRUE ~ indicator
           )) %>%
    dplyr::select(fundingagency, primepartner, snu1, psnu, psnuuid,
                  standardizeddisaggregate, sex,
                  trendsfine, indicator, fiscal_year, starts_with("qtr")) %>%
    group_by(fundingagency, primepartner, snu1, psnu, psnuuid,
             standardizeddisaggregate, sex,
             trendsfine, fiscal_year, indicator) %>%
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%
    ungroup() %>%
    reshape_msd(direction = "long") %>%
    spread(indicator, val) %>%
    group_by(fundingagency, primepartner, snu1, psnu, psnuuid,
             standardizeddisaggregate, sex, trendsfine) %>%
    mutate(TX_CURR_lag2 = lag(TX_CURR, 2),
           TX_CURR_lag1 = lag(TX_CURR, 1)) %>%
    dplyr::select(-TX_CURR) %>%
    ungroup() %>%
    gather(indicator, value = val, TX_PVLS_D:TX_CURR_lag1, na.rm = TRUE)


  #df_vl %>% glimpse()

  ##  step 3: append

  df_cntry <- bind_rows(df1, df_vl) %>%
    mutate(val = as.integer(val))

  df_cntry %>%
    distinct(indicator) %>%
    pull()

  #df_cntry %>% glimpse()

  #View(df_cntry)

  ##  Step 4: create summary table,
  ##  create vl suppression, coverage, retention

  ## Country summary table
  df_cntry_all <- df_cntry %>%
    filter(
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
      fundingagency %in% agencies
    ) %>%
    group_by(fundingagency, indicator, period) %>%
    summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
    spread(indicator, val) %>%
    mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
           coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
           retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
           linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
           positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
    ungroup %>%
    dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
    gather(indicator, val, HTS_TST:positivity) %>%
    mutate(indicator = factor(indicator, ind_list2),
           period = factor(period, pd_list),
           fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = factor(fundingagency, levels = c("USAID", "CDC"))) %>%
    spread(period, val)

  #df_cntry_all %>% glimpse()

  #View(df_cntry_all)

  # Export Country summary tbl
  write_csv(x = df_cntry_all,
            file = file.path(dataout,
                             paste0(country,
                                    " - Overview_tbl_q",
                                    curr_pd,
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")

  # SNU1 Summary
  df_snu1_all <- df_cntry %>%
    filter(
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
      fundingagency %in% agencies
    ) %>%
    group_by(fundingagency, snu1, indicator, period) %>%
    summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
    spread(indicator, val) %>%
    mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
           coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
           retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
           linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
           positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
    ungroup %>%
    dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
    gather(indicator, val, HTS_TST:positivity) %>%
    mutate(indicator = factor(indicator, ind_list2),
           period = factor(period, pd_list),
           fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
    spread(period, val)

  # Export snu1 summary tbl
  write_csv(x = df_snu1_all,
            file = file.path(dataout,
                             paste0(country,
                                    " - Overview_snu1_tbl_q",
                                    curr_pd,
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")

  # PSNU Summary
  df_psnu_all <- df_cntry %>%
    filter(
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
      fundingagency %in% agencies
    ) %>%
    group_by(fundingagency, snu1, psnu, psnuuid, indicator, period) %>%
    summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
    spread(indicator, val) %>%
    mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
           coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
           retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
           linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
           positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
    ungroup %>%
    dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
    gather(indicator, val, HTS_TST:positivity) %>%
    mutate(indicator = factor(indicator, ind_list2),
           period = factor(period, pd_list),
           fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
    spread(period, val)

  # Export psnu summary tbl
  write_csv(x = df_psnu_all,
            file = file.path(dataout,
                             paste0(country,
                                    " - Overview_psnu_tbl_q",
                                    curr_pd,
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


  # Partners Summary
  df_partner_all <- df_cntry %>%
    filter(
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
      fundingagency %in% agencies
    ) %>%
    group_by(fundingagency, primepartner, indicator, period) %>%
    summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
    ungroup %>%
    spread(indicator, val) %>%
    mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
           coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
           retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
           linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
           positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
    dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
    gather(indicator, val, HTS_TST:positivity) %>%
    mutate(indicator = factor(indicator, ind_list2),
           period = factor(period, pd_list),
           fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
    spread(period, val)

  # Export psnu summary tbl
  write_csv(x = df_partner_all,
            file = file.path(dataout,
                             paste0(country,
                                    " - Overview_partners_tbl_q",
                                    curr_pd,
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")

  ## table for peds
  df_cntry_peds <- df_cntry %>%
    filter(trendsfine %in% c("<01", "01-09", "10-14"),
           fundingagency %in% agencies) %>%
    group_by(fundingagency, indicator, period) %>%
    summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
    ungroup %>%
    spread(indicator, val) %>%
    mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
           coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
           retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
           linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
           positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
    dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
    gather(indicator, val, HTS_TST:positivity) %>%
    mutate(indicator = factor(indicator, ind_list2),
           period = factor(period, pd_list),
           fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
    spread(period, val)

  # Export
  write_csv(x = df_cntry_peds,
            file = file.path(dataout,
                             paste0(country,
                                    " - Overview_peds_tbl_q",
                                    curr_pd,
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


  ## table for peds x snu1
  df_cntry_peds_by_snu1 <- df_cntry %>%
    filter(trendsfine %in% c("<01", "01-09", "10-14"),
           fundingagency %in% agencies) %>%
    group_by(fundingagency, snu1, indicator, period) %>%
    summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
    ungroup %>%
    spread(indicator, val) %>%
    mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
           coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
           retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
           linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
           positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
    dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
    gather(indicator, val, HTS_TST:positivity) %>%
    mutate(indicator = factor(indicator, ind_list2),
           period = factor(period, pd_list),
           fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
    spread(period, val)

  # Export
  write_csv(x = df_cntry_peds_by_snu1,
            file = file.path(dataout,
                             paste0(country,
                                    " - Overview_peds_x_snu1_tbl_q",
                                    curr_pd,
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


  ## table for peds x psnu
  df_cntry_peds_by_psnu <- df_cntry %>%
    filter(trendsfine %in% c("<01", "01-09", "10-14"),
           fundingagency %in% agencies) %>%
    group_by(fundingagency, snu1, psnu, psnuuid, indicator, period) %>%
    summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
    ungroup %>%
    spread(indicator, val) %>%
    mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
           coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
           retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
           linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
           positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%
    dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
    gather(indicator, val, HTS_TST:positivity) %>%
    mutate(indicator = factor(indicator, ind_list2),
           period = factor(period, pd_list),
           fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
    spread(period, val)

  # Export
  write_csv(x = df_cntry_peds_by_psnu,
            file = file.path(dataout,
                             paste0(country,
                                    " - Overview_peds_x_psnu_tbl_q",
                                    curr_pd,
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


  ## table for peds x partners
  df_cntry_peds_by_partners <- df_cntry %>%
    filter(trendsfine %in% c("<01", "01-09", "10-14"),
           fundingagency %in% agencies) %>%
    group_by(fundingagency, primepartner, indicator, period) %>%
    summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
    ungroup %>%
    spread(indicator, val) %>%
    mutate(suppression = round(TX_PVLS_D / TX_CURR_lag2 * 100, 2),
           coverage = round(TX_PVLS_N / TX_PVLS_D * 100, 2),
           retention = round(TX_CURR / (TX_CURR_lag1 + TX_NEW) * 100, 2),
           linkage = round(TX_NEW / HTS_TST_POS * 100, 2),
           positivity = round(HTS_TST_POS / HTS_TST * 100, 2)) %>%

    dplyr::select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>%
    gather(indicator, val, HTS_TST:positivity) %>%
    mutate(indicator = factor(indicator, ind_list2),
           period = factor(period, pd_list),
           fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>%
    spread(period, val)

  # Export
  write_csv(x = df_cntry_peds_by_partners,
            file = file.path(dataout,
                             paste0(country,
                                    " - Overview_peds_x_partners_tbl_q",
                                    curr_pd,
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")








