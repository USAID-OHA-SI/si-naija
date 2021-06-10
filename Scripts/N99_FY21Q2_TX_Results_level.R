##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Treatments Results
##  LICENCE: MIT
##  DATE:    2021-06-03
##  UPDATE:  2021-06-08

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
  library(here)
  library(ICPIutilities)
  library(gt)

# SETUP ----

  # Directories
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"

  dir_merdata <- si_path("path_msd")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")


# LOAD DATA ----

  ## FY21 Targets
  df_psnu <- file_psnu_im %>% read_msd()

  df_psnu <- df_psnu %>%
    clean_agency() %>%
    reshape_msd(direction = "quarters")

  df_psnu %>% glimpse()

# MUNGING

  df_psnu %>%
    filter(str_detect(indicator, "TX_.*")) %>%
    distinct(indicator, standardizeddisaggregate)

  df_tx = df_psnu %>%
    filter(fiscal_year == "2021",
           indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(period, fundingagency, psnu, indicator) %>%
    summarise(across(c(targets, results), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(net_new = results - lag(results, 1),
           achieve = if_else(targets == 0, NA_real_, (results / targets))) %>%
    ungroup()

  df_tx %>%
    pivot_longer(cols = targets:achieve,
                 names_to = "metric",
                 values_to = "value")

# VIZ

  df_tx %>%
    filter(fundingagency != "DOD") %>%
    ggplot(aes(reorder_within(psnu, results, fundingagency), results)) +
    geom_col() +
    scale_x_reordered() +
    facet_grid(fundingagency ~ period, scales = "free") +
    coord_flip()

  df_tx %>%
    filter(fundingagency == "USAID",
           indicator == "TX_CURR") %>%
    ggplot(aes(reorder(psnu, results), results, group = period, fill = period)) +
    geom_col(position = "dodge") +
    facet_grid(indicator ~ ., scales = "free_x") +
    coord_flip()
