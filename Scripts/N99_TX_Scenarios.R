##  PROJECT: TX_CURR Historical Datasets
##  AUTHOR:  B.Kagniniwa | USAID
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

  # file
  file_draft_results <- list.files(
    path = data,
    pattern = "^COP21 Planning_DATA Request_12Jan21_UPDATED_Jan 14_PCO.xlsx$",
    full.names = TRUE
  )

  # Latest MSD PSNU x IM File - Curr release
  file_msd <- list.files(
      path = merdata,
      pattern = "MER_S.*_PSNU_IM_FY18-21_\\d{8}_v.*.*.zip",
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

# DATA ----------------------------------------------------

  # MSD Data

  # PSNU - FY21

  df_msd_draft <- file_draft_results %>%
    read_excel(sheet = 3, skip = 3) %>%
    clean_names() %>%
    clean_agency()

  df_msd_draft %>% glimpse()

  df_msd_draft %>% view()

  df_msd_draft2 <- df_msd_draft %>%
    mutate(
      tx_curr_q2fy19_shift = case_when(
        state == "Rivers" & fundingagency == "USAID" ~ -(.$tx_curr_q2fy19[state == "Rivers" & fundingagency == "USAID"]),
        state == "Rivers" & fundingagency == "CDC" ~ +(.$tx_curr_q2fy19[state == "Rivers" & fundingagency == "USAID"]),
        state == "Kano" & fundingagency == "USAID" ~ +(.$tx_curr_q2fy19[state == "Kano" & fundingagency == "CDC"]),
        state == "Kano" & fundingagency == "CDC" ~ -(.$tx_curr_q2fy19[state == "Kano" & fundingagency == "CDC"]),
        TRUE ~ NA_real_),
      tx_curr_q4fy19_shift = case_when(
        state == "Anambra" & fundingagency == "USAID" ~ -(.$tx_curr_q4fy19[state == "Anambra" & fundingagency == "USAID"]),
        TRUE ~ NA_real_),
      tx_curr_q2fy19_revised = case_when(
        state == "Rivers" & fundingagency == "CDC" ~ .$tx_curr_q2fy19[state == "Rivers" & fundingagency == "USAID"],
        state == "Rivers" & fundingagency == "USAID" ~ NA_real_,
        state == "Kano" & fundingagency == "USAID" ~ .$tx_curr_q2fy19[state == "Kano" & fundingagency == "USAID"] + .$tx_curr_q2fy19[state == "Kano" & fundingagency == "CDC"],
        state == "Kano" & fundingagency == "CDC" ~ NA_real_,
        TRUE ~ tx_curr_q2fy19),
      tx_curr_q4fy19_revised = case_when(
        state == "Anambra" & fundingagency == "USAID" ~ NA_real_,
        TRUE ~ tx_curr_q4fy19)
    ) %>%
    relocate(tx_curr_q2fy19_shift, tx_curr_q2fy19_revised, .after = tx_curr_q2fy19) %>%
    relocate(tx_curr_q4fy19_shift, tx_curr_q4fy19_revised, .after = tx_curr_q4fy19)

  #S1 - Agency
  df_msd_agency_tx_s6qtr <- df_msd_draft2 %>%
    select(fundingagency, state, contains(c("fy19", "fy20")), tx_curr_q1fy21, -starts_with("tx_pvls")) %>%
    group_by(fundingagency) %>%
    summarise_if(is_double, sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(tx_net_new_res6qtr = tx_curr_q4fy20 - tx_curr_q2fy19_revised,
           tx_net_new_trg6qtr = tx_curr_fy20 - tx_curr_q2fy19_revised,
           tx_net_new_ach6qtr = round(tx_net_new_res6qtr / tx_net_new_trg6qtr * 100, 2)) %>%
    relocate(tx_net_new_res6qtr, tx_net_new_trg6qtr, tx_net_new_ach6qtr,
             .after = tx_curr_q1fy21) %>%
    rowwise() %>%
    mutate(tx_new_res6qtr = sum(tx_new_q2fy19, tx_new_q4fy19,
                                tx_new_q2fy20, tx_new_q4fy20, na.rm = TRUE),
           tx_new_trg6qtr = tx_new_fy20,
           tx_new_ach6qtr = round(tx_new_res6qtr / tx_new_trg6qtr * 100, 2)) %>%
    ungroup() %>%
    relocate(tx_new_res6qtr, tx_new_trg6qtr, tx_new_ach6qtr,
             .after = tx_net_new_ach6qtr) %>%
    mutate(
      tx_gainloss6qtr = tx_net_new_res6qtr - tx_new_res6qtr,
      tx_prc_gainloss6qtr = round(tx_gainloss6qtr / tx_new_res6qtr * 100, 2),
      retention_proxy_6qtr = round(tx_curr_q4fy20 / (tx_curr_q2fy19_revised + tx_new_res6qtr) * 100, 2),
      cumulative_growth = round(tx_net_new_res6qtr / tx_curr_q4fy20 * 100, 2)
    ) %>%
    select(-contains(c("tx_new_q", "tx_new_fy", "tx_curr_fy"))) %>%
    filter(fundingagency != "GON")

  df_msd_agency_tx_s6qtr %>% glimpse()
  df_msd_agency_tx_s6qtr %>% View()


  # Export scenario 1 by agency
  write_csv(x = df_msd_agency_tx_s6qtr,
            file = file.path(dataout,
                             paste0(country,
                                    " - TX Scenario 1 - 6qtr - Agency summary",
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


  #S1
  df_msd_tx_s6qtr <- df_msd_draft2 %>%
    select(fundingagency, state, contains(c("fy19", "fy20")), tx_curr_q1fy21, -starts_with("tx_pvls")) %>%
    mutate(tx_net_new_res6qtr = tx_curr_q4fy20 - tx_curr_q2fy19_revised,
           tx_net_new_trg6qtr = tx_curr_fy20 - tx_curr_q2fy19_revised,
           tx_net_new_ach6qtr = round(tx_net_new_res6qtr / tx_net_new_trg6qtr * 100, 2)) %>%
    relocate(tx_net_new_res6qtr, tx_net_new_trg6qtr, tx_net_new_ach6qtr,
             .after = tx_curr_q1fy21) %>%
    rowwise() %>%
    mutate(tx_new_res6qtr = sum(tx_new_q2fy19, tx_new_q4fy19,
                                tx_new_q2fy20, tx_new_q4fy20, na.rm = TRUE),
           tx_new_trg6qtr = tx_new_fy20,
           tx_new_ach6qtr = round(tx_new_res6qtr / tx_new_trg6qtr * 100, 2)) %>%
    ungroup() %>%
    relocate(tx_new_res6qtr, tx_new_trg6qtr, tx_new_ach6qtr,
             .after = tx_net_new_ach6qtr) %>%
    mutate(
      tx_gainloss6qtr = tx_net_new_res6qtr - tx_new_res6qtr,
      tx_prc_gainloss6qtr = round(tx_gainloss6qtr / tx_new_res6qtr * 100, 2),
      retention_proxy_6qtr = round(tx_curr_q4fy20 / (tx_curr_q2fy19_revised + tx_new_res6qtr) * 100, 2),
      cumulative_growth = round(tx_net_new_res6qtr / tx_curr_q4fy20 * 100, 2)
    ) %>%
    select(-contains(c("tx_new_q", "tx_new_fy", "tx_curr_fy"))) %>%
    arrange(fundingagency, state)

  df_msd_tx_s6qtr %>% glimpse()
  df_msd_tx_s6qtr %>% View()


  # Export scenario 1
  write_csv(x = df_msd_tx_s6qtr,
            file = file.path(dataout,
                             paste0(country,
                                    " - TX Scenario 1 - 6qtr",
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")



  #S2 - Agency
  df_msd_agency_tx_sfy20 <- df_msd_draft2 %>%
    select(fundingagency, state, contains(c("fy19", "fy20")), tx_curr_q1fy21, -starts_with("tx_pvls")) %>%
    group_by(fundingagency) %>%
    summarise_if(is_double, sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(tx_net_new_res4qtr = tx_curr_q4fy20 - tx_curr_q4fy19_revised,
           tx_net_new_trg4qtr = tx_curr_fy20 - tx_curr_q4fy19_revised,
           tx_net_new_ach4qtr = round(tx_net_new_res4qtr / tx_net_new_trg4qtr * 100, 2)) %>%
    relocate(tx_net_new_res4qtr, tx_net_new_trg4qtr, tx_net_new_ach4qtr,
             .after = tx_curr_q1fy21) %>%
    rowwise() %>%
    mutate(tx_new_res4qtr = sum(tx_new_q2fy20, tx_new_q4fy20, na.rm = TRUE),
           tx_new_trg4qtr = tx_new_fy20,
           tx_new_ach4qtr = round(tx_new_res4qtr / tx_new_trg4qtr * 100, 2)) %>%
    ungroup() %>%
    relocate(tx_new_res4qtr, tx_new_trg4qtr, tx_new_ach4qtr,
             .after = tx_net_new_ach4qtr) %>%
    mutate(
      tx_gainloss4qtr = tx_net_new_res4qtr - tx_new_res4qtr,
      tx_prc_gainloss4qtr = round(tx_gainloss4qtr / tx_new_res4qtr * 100, 2),
      retention_proxy_4qtr = round(tx_curr_q4fy20 / (tx_curr_q4fy19_revised + tx_new_res4qtr) * 100, 2),
      cumulative_growth = round(tx_net_new_res4qtr / tx_curr_q4fy20 * 100, 2)
    ) %>%
    select(-contains(c("tx_new_q", "tx_new_fy", "tx_curr_q2", "tx_curr_fy"))) %>%
    filter(fundingagency != "GON")

  df_msd_agency_tx_sfy20 %>% glimpse()
  df_msd_agency_tx_sfy20 %>% View()


  # Export scenario 2
  write_csv(x = df_msd_agency_tx_sfy20,
            file = file.path(dataout,
                             paste0(country,
                                    " - TX Scenario 2 - FY20 - Agency Summary",
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


  #S2
  df_msd_tx_sfy20 <- df_msd_draft2 %>%
    select(fundingagency, state, contains(c("fy19", "fy20")), tx_curr_q1fy21, -starts_with("tx_pvls")) %>%
    mutate(tx_net_new_res4qtr = tx_curr_q4fy20 - tx_curr_q4fy19_revised,
           tx_net_new_trg4qtr = tx_curr_fy20 - tx_curr_q4fy19_revised,
           tx_net_new_ach4qtr = round(tx_net_new_res4qtr / tx_net_new_trg4qtr * 100, 2)) %>%
    relocate(tx_net_new_res4qtr, tx_net_new_trg4qtr, tx_net_new_ach4qtr,
             .after = tx_curr_q1fy21) %>%
    rowwise() %>%
    mutate(tx_new_res4qtr = sum(tx_new_q2fy20, tx_new_q4fy20, na.rm = TRUE),
           tx_new_trg4qtr = tx_new_fy20,
           tx_new_ach4qtr = round(tx_new_res4qtr / tx_new_trg4qtr * 100, 2)) %>%
    ungroup() %>%
    relocate(tx_new_res4qtr, tx_new_trg4qtr, tx_new_ach4qtr,
             .after = tx_net_new_ach4qtr) %>%
    mutate(
      tx_gainloss4qtr = tx_net_new_res4qtr - tx_new_res4qtr,
      tx_prc_gainloss4qtr = round(tx_gainloss4qtr / tx_new_res4qtr * 100, 2),
      retention_proxy_4qtr = round(tx_curr_q4fy20 / (tx_curr_q4fy19_revised + tx_new_res4qtr) * 100, 2),
      cumulative_growth = round(tx_net_new_res4qtr / tx_curr_q4fy20 * 100, 2)
    ) %>%
    select(-contains(c("tx_new_q", "tx_new_fy", "tx_curr_q2", "tx_curr_fy"))) %>%
    arrange(fundingagency, state)

  df_msd_tx_sfy20 %>% glimpse()
  df_msd_tx_sfy20 %>% View()


  # Export scenario 2
  write_csv(x = df_msd_tx_sfy20,
            file = file.path(dataout,
                             paste0(country,
                                    " - TX Scenario 2 - FY20",
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


  #S3
  df_msd_agency_tx_s4qtr <- df_msd_draft2 %>%
    select(fundingagency, state, contains(c("fy20")), tx_curr_q1fy21, tx_new_q1fy21, -starts_with("tx_pvls")) %>%
    group_by(fundingagency) %>%
    summarise_if(is_double, sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(tx_net_new_res4qtr = tx_curr_q1fy21 - tx_curr_q2fy20,
           tx_net_new_trg4qtr = tx_curr_fy20 - tx_curr_q2fy20,
           tx_net_new_ach4qtr = round(tx_net_new_res4qtr / tx_net_new_trg4qtr * 100, 2)) %>%
    relocate(tx_net_new_res4qtr, tx_net_new_trg4qtr, tx_net_new_ach4qtr,
             .after = tx_curr_q1fy21) %>%
    rowwise() %>%
    mutate(tx_new_res4qtr = sum(tx_new_q2fy20, tx_new_q4fy20, tx_new_q1fy21, na.rm = TRUE),
           tx_new_trg4qtr = tx_new_fy20,
           tx_new_ach4qtr = round(tx_new_res4qtr / tx_new_trg4qtr * 100, 2)) %>%
    ungroup() %>%
    relocate(tx_new_res4qtr, tx_new_trg4qtr, tx_new_ach4qtr,
             .after = tx_net_new_ach4qtr) %>%
    mutate(
      tx_gainloss4qtr = tx_net_new_res4qtr - tx_new_res4qtr,
      tx_prc_gainloss4qtr = round(tx_gainloss4qtr / tx_new_res4qtr * 100, 2),
      retention_proxy_4qtr = round(tx_curr_q1fy21 / (tx_curr_q2fy20 + tx_new_res4qtr) * 100, 2),
      cumulative_growth = round(tx_net_new_res4qtr / tx_curr_q1fy21 * 100, 2)
    ) %>%
    select(-contains(c("tx_new_q", "tx_new_fy", "tx_curr_q2", "tx_curr_fy"))) %>%
    filter(fundingagency != "GON")

  df_msd_agency_tx_s4qtr %>% glimpse()
  df_msd_agency_tx_s4qtr %>% View()


  # Export scenario 3
  write_csv(x = df_msd_agency_tx_s4qtr,
            file = file.path(dataout,
                             paste0(country,
                                    " - TX Scenario 3 - 4qtr - Agency summary",
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


  #S3
  df_msd_tx_s4qtr <- df_msd_draft2 %>%
    select(fundingagency, state, contains(c("fy20")), tx_curr_q1fy21, tx_new_q1fy21, -starts_with("tx_pvls")) %>%
    mutate(tx_net_new_res4qtr = tx_curr_q1fy21 - tx_curr_q2fy20,
           tx_net_new_trg4qtr = tx_curr_fy20 - tx_curr_q2fy20,
           tx_net_new_ach4qtr = round(tx_net_new_res4qtr / tx_net_new_trg4qtr * 100, 2)) %>%
    relocate(tx_net_new_res4qtr, tx_net_new_trg4qtr, tx_net_new_ach4qtr,
             .after = tx_curr_q1fy21) %>%
    rowwise() %>%
    mutate(tx_new_res4qtr = sum(tx_new_q2fy20, tx_new_q4fy20, tx_new_q1fy21, na.rm = TRUE),
           tx_new_trg4qtr = tx_new_fy20,
           tx_new_ach4qtr = round(tx_new_res4qtr / tx_new_trg4qtr * 100, 2)) %>%
    ungroup() %>%
    relocate(tx_new_res4qtr, tx_new_trg4qtr, tx_new_ach4qtr,
             .after = tx_net_new_ach4qtr) %>%
    mutate(
      tx_gainloss4qtr = tx_net_new_res4qtr - tx_new_res4qtr,
      tx_prc_gainloss4qtr = round(tx_gainloss4qtr / tx_new_res4qtr * 100, 2),
      retention_proxy_4qtr = round(tx_curr_q1fy21 / (tx_curr_q2fy20 + tx_new_res4qtr) * 100, 2),
      cumulative_growth = round(tx_net_new_res4qtr / tx_curr_q1fy21 * 100, 2)
    ) %>%
    select(-contains(c("tx_new_q", "tx_new_fy", "tx_curr_q2", "tx_curr_fy"))) %>%
    arrange(fundingagency, state)

  df_msd_tx_s4qtr %>% glimpse()
  df_msd_tx_s4qtr %>% View()


  # Export scenario 3
  write_csv(x = df_msd_tx_s4qtr,
            file = file.path(dataout,
                             paste0(country,
                                    " - TX Scenario 3 - 4qtr",
                                    "_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


































  # # PSNU - FY20
  # df_msd <- file_msd %>%
  #   read_msd() %>%
  #   reshape_msd(clean = TRUE) %>%
  #   filter(operatingunit == country)
  #
  # df_msd %>% glimpse()
  #
  # df_msd %>%
  #   filter(str_dete)
  #
  # df_msd_s <- df_msd %>%
  #   clean_agency() %>%
  #   filter(operatingunit == country,
  #          indicator %in% c("TX_CURR", "TX_NEW"),
  #          #period_type == "results",
  #          standardizeddisaggregate == "Total Numerator",
  #          fundingagency != "Dedup") %>%
  #   group_by(fundingagency, snu1, indicator, period) %>%
  #   summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
  #   ungroup()
  #
  # df_msd_s %>% glimpse()
  #
  # # 6Qtrs Scenario
  # df_msd_s1_tx_curr <- df_msd_s %>%
  #   filter(indicator == "TX_CURR",
  #          period %in% c("FY19Q2", "FY19Q4") | period %in% c("FY20Q4", "FY21Q1")) %>%
  #   arrange(fundingagency, indicator, snu1) %>%
  #   pivot_wider(names_from = period, values_from = val) %>%
  #   mutate(
  #     FY19Q2_shift = case_when(
  #       snu1 == "Rivers" & fundingagency == "USAID" ~ -(.$FY19Q2[snu1 == "Rivers" & fundingagency == "USAID"]),
  #       snu1 == "Rivers" & fundingagency == "CDC" ~ +(.$FY19Q2[snu1 == "Rivers" & fundingagency == "USAID"]),
  #       snu1 == "Kano" & fundingagency == "USAID" ~ +(.$FY19Q2[snu1 == "Kano" & fundingagency == "CDC"]),
  #       snu1 == "Kano" & fundingagency == "CDC" ~ -(.$FY19Q2[snu1 == "Kano" & fundingagency == "CDC"]),
  #       TRUE ~ NA_real_),
  #     FY19Q4_shift = case_when(
  #       snu1 == "Anambra" & fundingagency == "USAID" ~ -(.$FY19Q4[snu1 == "Anambra" & fundingagency == "USAID"]),
  #       TRUE ~ NA_real_),
  #     FY19Q2_revised = case_when(
  #       snu1 == "Rivers" & fundingagency == "CDC" ~ .$FY19Q2[snu1 == "Rivers" & fundingagency == "USAID"],
  #       snu1 == "Rivers" & fundingagency == "USAID" ~ NA_real_,
  #       snu1 == "Kano" & fundingagency == "USAID" ~ .$FY19Q2[snu1 == "Kano" & fundingagency == "USAID"] + .$FY19Q2[snu1 == "Kano" & fundingagency == "CDC"],
  #       snu1 == "Kano" & fundingagency == "CDC" ~ NA_real_,
  #       TRUE ~ FY19Q2),
  #     FY19Q4_revised = case_when(
  #       snu1 == "Anambra" & fundingagency == "USAID" ~ NA_real_,
  #       TRUE ~ FY19Q4)
  #   ) %>%
  #   relocate(FY19Q2_shift, FY19Q2_revised, .after = FY19Q2) %>%
  #   relocate(FY19Q4_shift, FY19Q4_revised, .after = FY19Q4) %>%
  #   mutate(FY20_NET_NEW = FY20Q4 - FY19Q2_revised) %>% View()
  #
  #
  # df_msd_s1_tx_new <- df_msd_s %>%
  #   filter(indicator == "TX_CURR",
  #          period %in% c("FY19Q2", "FY19Q4") | str_detect(period, "FY20")) %>%
  #   arrange(fundingagency, indicator, snu1) %>%
  #   pivot_wider(names_from = period, values_from = val) %>%
  #   mutate(
  #     FY19Q2_shift = case_when(
  #       snu1 == "Rivers" & fundingagency == "USAID" ~ -(.$FY19Q2[snu1 == "Rivers" & fundingagency == "USAID"]),
  #       snu1 == "Rivers" & fundingagency == "CDC" ~ +(.$FY19Q2[snu1 == "Rivers" & fundingagency == "USAID"]),
  #       snu1 == "Kano" & fundingagency == "USAID" ~ +(.$FY19Q2[snu1 == "Kano" & fundingagency == "CDC"]),
  #       snu1 == "Kano" & fundingagency == "CDC" ~ -(.$FY19Q2[snu1 == "Kano" & fundingagency == "CDC"]),
  #       TRUE ~ NA_real_),
  #     FY19Q4_shift = case_when(
  #       snu1 == "Anambra" & fundingagency == "USAID" ~ -(.$FY19Q4[snu1 == "Anambra" & fundingagency == "USAID"]),
  #       TRUE ~ NA_real_),
  #     FY19Q2_revised = case_when(
  #       snu1 == "Rivers" & fundingagency == "CDC" ~ .$FY19Q2[snu1 == "Rivers" & fundingagency == "USAID"],
  #       snu1 == "Rivers" & fundingagency == "USAID" ~ NA_real_,
  #       snu1 == "Kano" & fundingagency == "USAID" ~ .$FY19Q2[snu1 == "Kano" & fundingagency == "USAID"] + .$FY19Q2[snu1 == "Kano" & fundingagency == "CDC"],
  #       snu1 == "Kano" & fundingagency == "CDC" ~ NA_real_,
  #       TRUE ~ FY19Q2),
  #     FY19Q4_revised = case_when(
  #       snu1 == "Anambra" & fundingagency == "USAID" ~ NA_real_,
  #       TRUE ~ FY19Q4)
  #   ) %>%
  #   relocate(FY19Q2_shift, FY19Q2_revised, .after = FY19Q2) %>%
  #   relocate(FY19Q4_shift, FY19Q4_revised, .after = FY19Q4)


