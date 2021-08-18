##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Recency Testing Reporting
##  LICENCE: MIT
##  DATE:    2021-08-17


# DEPENDENCIES ----

  library(tidyverse)
  library(janitor)
  library(glamr)
  library(glitr)
  library(gisr)
  library(sf)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(tidytext)
  library(ggrepel)
  library(glue)
  library(gophr)

# GLOBAL VARS ----

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

  ## Reporting Filters
  rep_pd <- file_site_im %>% identify_pd()


# LOAD DATA ----

  # Data
  df_site <- file_site_im %>%
    read_msd() %>%
    clean_agency()

  df_site %>% glimpse()

  inds <- df_site %>%
    distinct(indicator) %>%
    pull()

  inds_hts <- c("HTS_TST_POS", "HTS_RECENT")

  df_site %>%
    filter(indicator %in% inds_hts) %>%
    distinct(standardizeddisaggregate) %>%
    pull()

  df_hts <- df_site %>%
    filter(fiscal_year == 2021,
           indicator %in% inds_hts,
           standardizeddisaggregate %in% c(
             "Modality/Age/Sex/RTRI/HIVStatus",
             "Modality/Age/Sex/Result")) %>%
    reshape_msd()

  df_hts %>% glimpse()

  df_hts %>% distinct(period, indicator, standardizeddisaggregate, otherdisaggregate)

  df_hts %>%
    filter(indicator == 'HTS_TST_POS') %>%
    distinct(period, standardizeddisaggregate, otherdisaggregate)

  df_hts %>%
    filter(indicator == 'HTS_RECENT') %>%
    distinct(period, standardizeddisaggregate, otherdisaggregate)

  df_hts <- df_hts %>%
    filter(str_detect(str_to_lower(facility),
                      pattern = "reported above facility",
                      negate = TRUE),
           period_type != 'targets') %>%
    group_by(period, fundingagency, psnu, facilityuid, facility,
             indicator, standardizeddisaggregate, otherdisaggregate) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup()

  df_hts_rtri <- df_hts %>%
    group_by(period, fundingagency, psnu, facilityuid, facility) %>%
    summarise(hts_pos = sum(value[otherdisaggregate == 'Newly Identified'], na.rm = TRUE),
              hts_recent = sum(value[otherdisaggregate %in% c('Recent RTRI', 'Long-Term RTRI')])) %>%
    ungroup() %>%
    mutate(hts_recent_share = hts_recent/hts_pos) %>%
    filter(hts_recent_share > .90)



  df_hts_rtri %>%
    filter(facility %in% c('Oron General Hospital (Iquita)',
                         'Kafin Hausa General Hospital'))

  # Reporting rates
  df_hts_recency_sites <- df_hts_rtri %>%
    filter(str_detect(period, "^FY21")) %>%
    mutate(recency = if_else(!is.na(HTS_RECENT), 1, 0)) %>%
    group_by(period, fundingagency, psnu) %>%
    summarise(recency = sum(recency, na.rm = TRUE),
              facilities = n(),
              recency_report = recency / facilities) %>%
    ungroup()


  df_hts_recency_issues <- df_hts_rtri %>%
    filter(str_detect(period, "^FY21"), !is.na(HTS_RECENT)) %>%
    mutate(rtri = HTS_RECENT / HTS_TST_POS) %>%
    filter(rtri > 1)


# VIZ ----

  # Reporting rates
  df_hts_recency_sites %>%
    mutate(color = if_else(fundingagency == "USAID", usaid_blue, usaid_lightblue)) %>%
    ggplot(aes(x = reorder_within(psnu, recency_report, list(period, fundingagency)),
               y = recency_report,
               fill = color)) +
    geom_col() +
    scale_y_continuous(labels = percent, position = "right") +
    scale_x_reordered() +
    scale_fill_identity() +
    facet_wrap(period ~ fundingagency, scales = "free_y") +
    coord_flip() +
    labs(x = "", y = "",
         title = "% OF FACILITIES REPORTING HTS_RECENCY BY AGENCY, PSNU AND PERIOD",
         subtitle = "Only 5 USAID States and <30% of the facilities have been reporting HTS_RECENCY") +
    si_style_xgrid() +
    theme(strip.placement = "outsite")


  df_hts_rtri %>%
    filter(fundingagency == "USAID", period != "FY21Q1") %>%
    mutate(
      name = paste0(facility, " (", psnu, ")"),
      color = if_else(fundingagency == "USAID", usaid_blue, usaid_lightblue)) %>%
    ggplot(aes(x = reorder_within(name, hts_recent_share, list(period, fundingagency)),
               y = hts_recent_share, fill = color)) +
    geom_col() +
    scale_y_continuous(labels = percent, position = "right") +
    scale_x_reordered() +
    scale_fill_identity() +
    facet_wrap(period ~ fundingagency, scales = "free_y") +
    coord_flip() +
    labs(x = "", y = "",
         title = "% RECENT RTRI of HTS_POS (USAID ONLY)",
         subtitle = "These sites have % Recent RTRI > 90%") +
    si_style_xgrid() +
    theme(strip.placement = "outsite")
