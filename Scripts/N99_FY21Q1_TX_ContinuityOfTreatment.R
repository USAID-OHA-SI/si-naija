##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: TX_CURR / NEW Targets by IP and IM
##  LICENCE: MIT
##  DATE:    2021-06/29


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

  ## Reporting Filters
  rep_agency = "USAID"
  rep_agencies <- c("USAID", "CDC")

  cntry <- "Nigeria"

  rep_fy <- 2021

  rep_qtr <- 2

  rep_fy2 <- rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_fys <- c(rep_fy - 1, rep_fy)

  rep_fys2 <- rep_fys %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_pd <- rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q", rep_qtr)

  rep_ref_pd <- rep_fys %>%
    first() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q4")

  rep_init_pd <- rep_fys %>%
    first() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q1")

  rep_pds <- c(rep_ref_pd, rep_pd)
  rep_pds2 <- c(rep_init_pd, rep_pd)

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

# LOAD DATA ----

  # Data
  df_site <- file_site_im %>%
    read_msd() %>%
    clean_agency()

  df_psnu <- file_psnu_im %>%
    read_msd() %>%
    clean_agency()

# MUNGING ----

  # Available fields
  df_site %>% glimpse()

  # Indicators connected at site levels
  df_site %>%
    filter(sitetype == "Above Site") %>%
    distinct(indicator) %>%
    pull()

  # TX Indicators / site types
  df_site %>%
    filter(str_detect(indicator, "^TX_.*")) %>%
    distinct(indicator, sitetype) %>%
    arrange(indicator) %>%
    prinf()

  df_site %>%
    filter(str_detect(indicator, "^TX_.*")) %>%
    distinct(indicator, standardizeddisaggregate) %>%
    prinf()

  df_site %>%
    filter(fundingagency == rep_agency,
           fiscal_year == rep_fy,
           indicator %in% c("TX_CURR", "TX_NEW")) %>%
    distinct(mech_name) %>%
    pull()

  # TX_CURR / NEW
  df_site <- df_site %>%
    mutate(mech_name = case_when(
      mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 3" ~ "SHARP TO 3",
      mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 2" ~ "SHARP TO 2",
      mech_name == "SHARP Task Order 1" ~ "SHARP TO 1",
      mech_name == "Reaching Impact, Saturation and Epidemic Control (RISE)" ~ "RISE",
      mech_name == "STRENGHTENING INTERGRATED DELIVERY OF HIV/AIDS SERVICES(SIDHAS)" ~ "SIDHAS",
      mech_name == "Meeting Targets and Maintaining Epidemic Control (EpiC)" ~ "EpiC",
      TRUE ~ mech_name
    ))

  df_psnu <- df_psnu %>%
    mutate(mech_name = case_when(
      mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 3" ~ "SHARP TO 3",
      mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 2" ~ "SHARP TO 2",
      mech_name == "SHARP Task Order 1" ~ "SHARP TO 1",
      mech_name == "Reaching Impact, Saturation and Epidemic Control (RISE)" ~ "RISE",
      mech_name == "STRENGHTENING INTERGRATED DELIVERY OF HIV/AIDS SERVICES(SIDHAS)" ~ "SIDHAS",
      mech_name == "Meeting Targets and Maintaining Epidemic Control (EpiC)" ~ "EpiC",
      TRUE ~ mech_name
    ))

  # BY IP
  df_tx_partners <- df_psnu %>%
    filter(fundingagency == rep_agency,
           fiscal_year == rep_fy,
           indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(indicator, primepartner) %>%
    summarise(across(targets, sum, rm.na = TRUE)) %>%
    ungroup() %>%
    arrange(indicator, primepartner) %>%
    rename(value = targets)

  # BY Mech
  df_tx_mechs <- df_psnu %>%
    filter(fundingagency == "USAID",
           fiscal_year == rep_fy,
           indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, indicator, mech_name) %>%
    summarise(across(targets, sum, rm.na = TRUE)) %>%
    ungroup() %>%
    arrange(indicator, mech_name) %>%
    rename(value = targets)


  # Sites
  df_org_sites <- df_site %>%
    distinct(psnu, psnuuid, facility, orgunituid) %>%
    select(psnuuid, psnu, orgunituid, facility) %>%
    filter(str_detect(facility, pattern = "above Facility level", negate = TRUE))

  df_tx_curr <- df_site %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>%
    reshape_msd() %>%
    filter(period_type == 'results') %>%
    select(fundingagency, operatingunit, orgunituid, period, mech_code, value)

  df_tx_curr %>% distinct(period) %>% arrange(period)

  df_tx_curr %>%
    complete(period, nesting(fundingagency, operatingunit, orgunituid, mech_code, value)) %>%
    arrange(orgunituid, mech_code) %>%
    mutate(flag_loneobs = !is.na(value) &
             is.na(lead(value, order_by = period)) &
             is.na(lag(value, order_by = period))) %>%
    ungroup() %>%
    # Stopped reporting
    mutate(flag_loneobs = if_else(period %in% c(min(df_tx_curr$period),
                                                max(df_tx_curr$period)),
                                  FALSE, flag_loneobs)) %>%
    filter(!is.na(value)) %>%
    # Milti Mech sites
    group_by(orgunituid, period) %>%
    mutate(flag_multimech_site = n() > 1) %>%
    ungroup() %>%
    # Last Obs sites
    group_by(orgunituid) %>%
    mutate(last_obs_site = max(period)) %>%
    ungroup() %>%
    group_by(orgunituid, mech_code) %>%
    mutate(last_obs_sitexmech = max(period)) %>%
    ungroup() %>%
    mutate(flag_end_sitexmech = period == last_obs_sitexmech & period != max(df_tx_curr$period)) %>%
    group_by(orgunituid) %>%
    mutate(end_type = case_when(flag_end_sitexmech == TRUE & last_obs_sitexmech == last_obs_site ~ "Transition out of PEPFAR",
                                flag_end_sitexmech == TRUE & flag_multimech_site == TRUE ~ "Consolidate multi-mechanism site",
                                flag_end_sitexmech == TRUE & fundingagency != lead(fundingagency, order_by = period) ~ "Transition to other agency",
                                flag_end_sitexmech == TRUE & mech_code != lead(mech_code, order_by = period) ~ "Transition to other mechanism")) %>%
    ungroup()



# VIZ ----

  # Partners
  df_tx_partners %>%
    filter(!is.na(value)) %>%
    mutate(label_color = if_else(value < 1000, usaid_darkgrey, grey20k))  %>%
    ggplot(aes(reorder_within(primepartner, value, indicator), value)) +
    geom_hline(yintercept = 0, size = .5, color = grey50k) +
    geom_col(fill = genoa) +
    geom_text_repel(aes(y = 0, label = comma(value), color = label_color),
                    hjust = 1, size = 3) +
    scale_x_reordered() +
    scale_color_identity() +
    scale_y_continuous(labels = comma, position = "right") +
    coord_flip() +
    facet_wrap(~indicator, scales = "free") +
    labs(x = "", y = "",
         title = "NIGERIA - FY21 TREATMENT TARGETS BY PARTNER",
         subtitle = "NOTE: These targets do not include the latest COP20 OPU",
         caption = paste0("Source: DATIM MSD FY21Q1c - Produced on ",
                          format(Sys.Date(), "%Y-%m-%d"))) +
    si_style_nolines() +
    theme(strip.placement = "outside",
          axis.ticks.x = element_line(size = 1, color = grey50k))

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - FY21 Treatment Targets by Partner - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = last_plot(),
    width = 10,
    height = 5)

  # Mechanisms
  df_tx_mechs %>%
    filter(!is.na(value)) %>%
    mutate(label_color = if_else(value < 1000, usaid_darkgrey, grey20k))  %>%
    ggplot(aes(reorder_within(mech_name, value, indicator), value)) +
    geom_hline(yintercept = 0, size = .5, color = grey50k) +
    geom_col(fill = genoa) +
    geom_text_repel(aes(y = 0, label = comma(value), color = label_color),
                    hjust = 1, size = 3) +
    scale_x_reordered() +
    scale_color_identity() +
    scale_y_continuous(labels = comma, position = "right") +
    coord_flip() +
    facet_wrap(~indicator, scales = "free", ) +
    labs(x = "", y = "",
         title = "NIGERIA - FYI21 TREATMENT TARGETS BY MECHANISM",
         subtitle = "NOTE: These targets do not include the latest COP20 OPU",
         caption = paste0("Source: DATIM MSD FY21Q1c - Produced on ",
                          format(Sys.Date(), "%Y-%m-%d"))) +
    si_style_nolines() +
    theme(strip.placement = "outside",
          axis.ticks.x = element_line(size = 1, color = grey50k))

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - FY21 Treatment Targets by Mechanisms - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = last_plot(),
    width = 10,
    height = 5)

