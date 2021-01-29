##  PROJECT: TX_CURR Historical Datasets
##  AUTHOR:  jdavis | USAID
##  PURPOSE: TX_CURR PSNU x IM
##  LICENCE: MIT
##  DATE:    2021-01-25


# PACKAGES -------------------------------------------------

library(tidyverse)
library(readxl)
library(vroom)
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

  # Pre-FY15 Data
  file_pre_datim <- list.files(
      path = data,
      pattern = "Country and Regional Targets_Results 2004-2016.csv",
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

  df_msd %>%
    clean_agency() %>% #View()
    filter(indicator %in% c("TX_CURR", "TX_NEW")) %>%
    distinct(standardizeddisaggregate, period) %>% prinf()

  # Verify counts: thx TE

  df_check <- df_msd %>%
    clean_agency() %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           str_to_lower(fundingagency) != "dedup")

  df_check %>% count(period_type)

  # Verify SNU1
  df_check %>%
    filter(period_type == "results") %>%
    group_by(fundingagency, period, indicator, snu1) %>%
    summarise(snu1_totals_period = sum(val, na.rm = TRUE)) %>%
    spread(period, snu1_totals_period) %>%
    View()

  # Verify Agency
  df_check %>%
    filter(period_type == "results") %>%
    group_by(fundingagency, period, indicator) %>%
    summarise(agency_totals_period = sum(val, na.rm = TRUE)) %>%
    spread(period, agency_totals_period) %>%
    View(title = "agency")


  # TX Data
  df_msd_tx <- df_msd %>%
    clean_agency() %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           str_to_lower(fundingagency) != "dedup") %>%
    group_by(fundingagency, snu1, indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup()


  df_msd_tx %>%
    distinct(period) %>%
    pull() %>%
    sort()


  df_msd_tx_results <- df_msd_tx %>%
    filter(period_type == "results") %>%
    dplyr::select(-period_type) %>%
    mutate(period = str_replace(period, "FY", "FY20")) %>%
    arrange(period) %>%
    pivot_wider(names_from = period, values_from = val) %>%
    relocate(indicator, .after = fundingagency) %>%
    arrange(fundingagency, indicator, snu1)


  # Export results tbl
  # write_csv(x = df_msd_tx_results,
  #           file = file.path(dataout,
  #                            paste0(country,
  #                                   " - Historical Treatment Results",
  #                                   "_", format(Sys.Date(), "%Y%m%d"),
  #                                   ".csv")), na = "")

  # Double check numbers
  df <- df_msd_tx_results %>%
    pivot_longer(cols = starts_with("FY"),
                 names_to = "period",
                 values_to = "val") %>%
    separate(period, c("fiscal_year", "rep_qtr"), sep = "Q") %>%
    mutate(rep_qtr = paste0("Q", rep_qtr)) %>%
    group_by(fundingagency, indicator, snu1, fiscal_year) %>%
    summarise(val = sum(val, na.rm = TRUE)) %>%
    ungroup()

  df %>% View()

  df %>%
    filter(fiscal_year == "FY2015",
           fundingagency == "USAID",
           indicator == "TX_CURR") %>%
    group_by(fundingagency, indicator) %>%
    summarise(val = sum(val, na.rm = T)) %>% View()

  df %>%
    filter(fiscal_year == "FY2015",
           fundingagency == "USAID",
           indicator == "TX_CURR",
           snu1 %in% c("Abia", "Taraba")) %>%
    group_by(fundingagency, indicator) %>%
    summarise(val = sum(val, na.rm = T)) %>% View()


  df %>%
    filter(fiscal_year == "FY2019",
           fundingagency == "USAID",
           indicator == "TX_CURR",
           snu1 %in% c("Rivers")) %>%  View()
    group_by(fundingagency, indicator) %>%
    summarise(val = sum(val, na.rm = T)) %>% View()

    df %>%
      filter(fiscal_year == "FY2019",
             fundingagency == "USAID",
             indicator == "TX_CURR",
             snu1 %in% c("Anambra")) %>%  View()
    group_by(fundingagency, indicator) %>%
      summarise(val = sum(val, na.rm = T)) %>% View()



  df %>%
    filter(fiscal_year == "FY2015",
           indicator == "TX_NEW",
           snu1 %in% c("Abia", "Taraba")) %>% View()


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


  # Export targets tbl
  # write_csv(x = df_msd_tx_results,
  #           file = file.path(dataout,
  #                            paste0(country,
  #                                   " - Historical Treatment Targets",
  #                                   "_", format(Sys.Date(), "%Y%m%d"),
  #                                   ".csv")), na = "")

  # OU TX Data
  df_msd_cntry_tx <- df_msd %>%
    clean_agency() %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           str_to_lower(fundingagency) != "dedup") %>%
    group_by(indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    filter(period_type %in% c("cumulative", "targets"),
           !period %in% c("FY15", "FY16", "FY21")) %>%
    pivot_wider(names_from = period_type, values_from = val) %>%
    mutate(achieve = cumulative / targets * 100)


  # Pre-datim data
  df <- file_pre_datim %>%
    vroom() %>%
    clean_names()

  df_tx_nga <- df %>%
    filter(country_region == "Nigeria",
           bundle == "HIV Treatment",
           str_detect(indicator_short_name,
                      "Currently Receiving|Newly Receiving"),
           dsd_ta == "DSD+TA") %>%
    mutate(measure_value = as.integer(measure_value)) %>%
    arrange(year, indicator_short_name) %>%
    pivot_wider(names_from = "measure_name", values_from = "measure_value") %>%
    mutate(Achieve = Results / Targets * 100)


  # Export targets tbl
  # write_csv(x = df_tx_nga,
  #           file = file.path(dataout,
  #                            paste0(country,
  #                                   " - Historical Treatment Data - Pre Datim",
  #                                   "_", format(Sys.Date(), "%Y%m%d"),
  #                                   ".csv")), na = "")

  # Align with MSD
  df_cntry_tx <- df_tx_nga %>%
    dplyr::select(indicator = indicator_short_name,
           period = year,
           cumulative = Results,
           targets = Targets,
           achieve = Achieve) %>%
    mutate(
      indicator = case_when(
        indicator == "Patients Currently Receiving ART" ~ "TX_CURR",
        indicator == "Patients Newly Receiving ART" ~ "TX_NEW",
        TRUE ~ NA_character_
      ),
      period = paste0("FY", str_sub(period, 3, 4)))

  # Merge data
  df_cntry_tx <- df_cntry_tx %>%
    bind_rows(df_msd_cntry_tx)

  # Export targets tbl
  # write_csv(x = df_cntry_tx,
  #           file = file.path(dataout,
  #                            paste0(country,
  #                                   " - Historical Treatment Data - Entire History",
  #                                   "_", format(Sys.Date(), "%Y%m%d"),
  #                                   ".csv")), na = "")




# Data Call
# last 3 fy

  df_tx_hts <- df_msd_curr %>%
    clean_agency() %>%
    filter(indicator %in% c("HTS_TST", "TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           str_to_lower(fundingagency) != "dedup",
           period_type %in% c("cumulative", "targets")) %>%
    group_by(fundingagency, indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(indicator = paste0(indicator, "_", period_type)) %>%
    dplyr::select(-period_type) %>%
    pivot_wider(names_from = indicator, values_from = val) %>%
    mutate(TX_CURR_achieve = TX_CURR_cumulative / TX_CURR_targets * 100,
           HTS_TST_achieve = HTS_TST_cumulative / HTS_TST_targets * 100) %>%
    relocate(HTS_TST_achieve, .after = HTS_TST_targets) %>%
    rename(fiscal_year = period)

  # Export tx & hts tbl
  # write_csv(x = df_tx_hts,
  #           file = file.path(dataout,
  #                            paste0(country,
  #                                   " - OU Treatment and Testing Data - FY18toFY21",
  #                                   "_", format(Sys.Date(), "%Y%m%d"),
  #                                   ".csv")), na = "")


  # Data Call
  # Full history of all indicators

  df_hist <- df_msd %>%
    clean_agency() %>%
    filter(standardizeddisaggregate == "Total Numerator",
           str_to_lower(fundingagency) != "dedup",
           ) %>%
    group_by(fundingagency, snu1, indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    rename(value = val)

  # Export history tbl
  # write_csv(x = df_hist,
  #           file = file.path(dataout,
  #                            paste0(country,
  #                                   " - All Indicators History Data - FY15toFY21",
  #                                   "_", format(Sys.Date(), "%Y%m%d"),
  #                                   ".csv")), na = "")

# VIZ ----

  tmax <- df_tx_nga %>%
    filter(!is.na(Targets)) %>%
    pull(Targets) %>%
    max()


  df_tx_nga %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = Targets, group = country_region), color = usaid_red) +
    geom_area(aes(y = Targets), fill = usaid_red, alpha = .7) +
    geom_point(aes(y = Targets), fill = "white", color = usaid_red,
               shape = 21, size = 3) +
    geom_line(aes(y = Results, group = country_region), color = "white") +
    geom_area(aes(y = Results), fill = usaid_lightblue, alpha = .7) +
    geom_point(aes(y = Results), fill = "white", color = usaid_lightblue,
               shape = 21, size = 3) +
    geom_text(aes(y = Results, label = paste0(round(Achieve), "%")),
              color = usaid_darkgrey, nudge_x = .5, size = 4) +
    scale_fill_si(palette = "genoas") +
    scale_size_area() +
    scale_y_continuous(labels = comma, breaks = seq(0, tmax, 100000)) +
    scale_x_continuous(breaks = seq(2004, 2016, 1)) +
    facet_wrap(~ indicator_short_name) +
    si_style_ygrid() +
    labs(x = "", y = "",
         title = "NIGERIA - Historical TX Achievements (FY04 to FY16)",
         subtitle = "Targets are in Red and results in light blue. There was no target set in 2004",
         caption = paste0("Data source: Historical data from DATIM\nOHA/SIEI - Produced on: ", format(Sys.Date(), "%Y%m%d")))


  # Save output
  ggsave(file.path(graphics, paste0("Nigeria - Historical TX Achievements FY04_to_FY16 - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.3, dpi = 350,
         width = 10, height = 7, units = "in")


  # Entire history
  htmax <- df_cntry_tx %>%
    filter(!is.na(targets)) %>%
    pull(targets) %>%
    max()

  df_cntry_tx %>%
    mutate(period = as.integer(str_replace(period, "FY", "20"))) %>%
    ggplot(aes(x = period)) +
    geom_line(aes(y = targets, group = 1), color = usaid_red) +
    geom_area(aes(y = targets), fill = usaid_red, alpha = .7) +
    geom_point(aes(y = targets), fill = "white", color = usaid_red,
               shape = 21, size = 2) +
    geom_line(aes(y = cumulative, group = 1), color = "white") +
    geom_area(aes(y = cumulative), fill = usaid_lightblue, alpha = .7) +
    geom_point(aes(y = cumulative), fill = "white", color = usaid_lightblue,
               shape = 21, size = 2) +
    geom_text(aes(y = cumulative, label = paste0(round(achieve), "%")),
              color = usaid_darkgrey, nudge_x = .5, size = 3.5) +
    scale_fill_si(palette = "genoas") +
    scale_size_area() +
    scale_y_continuous(labels = comma, breaks = seq(0, 1300000, 200000)) +
    scale_x_continuous(breaks = seq(2004, 2020, 2)) +
    facet_wrap(~ indicator) +
    si_style_ygrid() +
    labs(x = "", y = "",
         title = "NIGERIA - Historical TX Achievements (FY04 to FY20)",
         subtitle = "Targets are in Red and results in light blue. There was no target set in 2004",
         caption = paste0("Data source: Historical data from DATIM\nOHA/SIEI - Produced on: ", format(Sys.Date(), "%Y%m%d")))

  # Save output
  ggsave(file.path(graphics, paste0("Nigeria - Historical TX Achievements FY04_to_FY20 - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.3, dpi = 350,
         width = 10, height = 7, units = "in")
