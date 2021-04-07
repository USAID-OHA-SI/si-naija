##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: TX_CURR Results Trend by Gender
##  LICENCE: MIT
##  DATE:    2021-04-07


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
    reshape_msd()

  df_psnu <- file_psnu_im %>%
    read_msd() %>%
    reshape_msd()

# MUNGING ----

  # Available fields
  df_site %>% glimpse()

  df_site %>% distinct() %>% pull()

  df_site %>% distinct(indicator) %>% pull()

  df_site %>% distinct(sitetype) %>% pull()

  df_site %>% distinct(standardizeddisaggregate) %>% pull()

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
    filter(fundingagency == "USAID",
           period == 'FY21',
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

  # BY IP
  df_tx_partners <- df_site %>%
    filter(fundingagency == "USAID",
           indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           period_type == 'targets',
           period == 'FY21') %>%
    group_by(indicator, primepartner) %>%
    summarise(across(value, sum, rm.na = TRUE)) %>%
    ungroup() %>%
    arrange(indicator, primepartner)

  # BY Mech
  df_tx_mechs <- df_site %>%
    filter(fundingagency == "USAID",
           indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           period_type == 'targets',
           period == 'FY21') %>%
    group_by(indicator, mech_name) %>%
    summarise(across(value, sum, rm.na = TRUE)) %>%
    ungroup() %>%
    arrange(indicator, mech_name)


# VIZ ----

  # Partners
  df_tx_partners %>%
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
         title = "NIGERIA - FYI21 TREATMENT TARGETS BY PARTNER",
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

