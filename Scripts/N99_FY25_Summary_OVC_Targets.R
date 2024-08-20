# PURPOSE: SI-Naija
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: OVC Targets for FY25
# REF ID:  3442242d
# LICENSE: MIT
# DATE:    2024-07-30
# UPDATE:  2024-08-01
# NOTES:   For new OVC IMs

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(grabr)
  library(glitr)
  library(glamr)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(systemfonts)

  source("./Scripts/N00_Utilities.R")

# LOCALS & SETUP ====

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_cntry <- file.path("../../PEPFAR/COUNTRIES/Nigeria")

  # Files

  # file_psnu <- dir_mer %>%
  #   return_latest("PSNU_IM_FY22.*.Nigeria")

  file_psnu <- dir_data %>%
    file.path("Genie") %>%
    return_latest("Genie-PSNUByIMs")

  files_ovc <- file.path("../../PEPFAR/COUNTRIES/Nigeria/To-Be-Reviewed/Targets/OVC") %>%
    list.files("Targets.*.csv$", full.names = T)

  meta <- get_metadata(file_psnu)

  # Set Params

  ref_id <- "d269a16d"
  agency <- "USAID"
  cntry <- "Nigeria"

# LOAD DATA ====

  df_msd <- file_psnu %>%
    read_psd() %>%
    filter(funding_agency == agency,
           indicator %in% c("OVC_HIVSTAT", "OVC_SERV"))

  df_msd %>% glimpse()

  df_msd %>% distinct(fiscal_year)

  df_msd %>%
    clean_indicator() |>
    filter(fiscal_year %in% c(meta$curr_fy, meta$curr_fy +1),
           #use_for_age == "Y",
           str_detect(standardizeddisaggregate, "Total", negate = T)) %>%
    distinct(fiscal_year, indicator, standardizeddisaggregate, otherdisaggregate)





## MUNGE

  df_ovc <- df_msd %>%
    filter(
      fiscal_year %in% c(meta$curr_fy, meta$curr_fy +1),
      standardizeddisaggregate == "Total Numerator") %>%
    clean_indicator() |>
    reshape_msd() %>%
    filter(period_type %in% c("cumulative", "targets")) %>%
    summarise(across(value, ~sum(.x, na.rm = T)),
              .by = c(period, period_type, psnu, indicator)) %>%
    relocate(period, .before = 1) %>%
    mutate(period_type = paste0(period, "_", period_type)) %>%
    select(-period) %>%
    pivot_wider(names_from = period_type, values_from = value)

  df_targets_adj <- df_ovc %>%
    filter(!is.na(FY25_targets)) %>%
    mutate(
      targets = case_when(
        indicator == "OVC_SERV" & FY25_targets <= FY24_cumulative ~ FY24_cumulative,
        indicator == "OVC_SERV" & FY25_targets > FY24_cumulative ~ FY25_targets,
        TRUE ~ FY25_targets
      )
    ) %>%
    select(psnu, indicator, targets) %>%
    pivot_wider(names_from = indicator,
                values_from = targets)

  df_targets_adj <- df_targets_adj %>%
    mutate(
      region = case_when(
        psnu %in% c('Jigawa',
                    'Kano',
                    'Kebbi',
                    'Niger',
                    'Sokoto',
                    'Zamfara') ~ "THRIVE NORTH-WEST",
        psnu %in% c('Adamawa',
                    'Bauchi',
                    'Borno',
                    'Taraba',
                    'Yobe') ~ "THRIVE NORTH-EAST",
        psnu %in% c('Akwa Ibom',
                    'Bayelsa',
                    'Cross River',
                    'Edo',
                    'Lagos') ~ "THRIVE SOUTH",
        TRUE ~ NA_character_
      )
    ) %>%
    relocate(region, .before = 1) %>%
    arrange(region, psnu)

  # State totals
  df_targets_adj <- df_targets_adj %>%
    summarise(across(starts_with("OVC"), ~sum(.x, na.rm = T)),
              .by = region) %>%
    mutate(psnu = "Total") %>%
    bind_rows(df_targets_adj, .)

  # Agency Total
  df_targets_adj <- df_targets_adj %>%
    filter(psnu == "Total") %>%
    summarise(across(starts_with("OVC"), ~sum(.x, na.rm = T))) %>%
    mutate(region = "AGENCY", psnu = "Total") %>%
    bind_rows(df_targets_adj, .)

  df_targets_adj %>%
    mutate(region = factor(
      region,
      levels = c(
        "THRIVE NORTH-EAST",
        "THRIVE NORTH-WEST",
        "THRIVE SOUTH",
        "AGENCY"
      ),
      ordered = T)) %>%
    arrange(region, psnu) %>%
    rename(state = psnu) %>%
    rename_with(toupper) %>%
    write_csv(
      na = "",
      file = file.path(
        dir_dataout,
        "FY25_OVC_Adjusted_Targets_for_THRIVE_IMs.csv"
      ))

  file.path(
    dir_dataout,
    "FY25_OVC_Adjusted_Targets_for_THRIVE_IMs.csv"
  ) %>% open_path()


  #
df_ovc_sum <- df_ovc %>%
  filter(indicator %in% c("OVC_SERV", "OVC_HIVSTAT")) %>%
  mutate(
    mechanism = case_when(
      psnu %in% c('Jigawa',
                  'Kano',
                  'Kebbi',
                  'Niger',
                  'Sokoto',
                  'Zamfara') ~ "THRIVE NORTH-WEST",
      psnu %in% c('Adamawa',
                  'Bauchi',
                  'Borno',
                  'Taraba',
                  'Yobe') ~ "THRIVE NORTH-EAST",
      psnu %in% c('Akwa Ibom',
                  'Bayelsa',
                  'Cross River',
                  'Edo',
                  'Lagos') ~ "SOUTH",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(mechanism, psnu) %>%
  summarise(across(targets, ~sum(.x, na.rm = T)),
            .by = c(mechanism, psnu, indicator)) %>%
  pivot_wider(names_from = indicator, values_from = targets)


df_ovc_sum %>%
  summarise(across(starts_with("OVC"), ~sum(.x, na.rm = T)))

# Mech summary
df_ovc_sum <- df_ovc_sum %>%
  summarise(across(starts_with("OVC"), ~sum(.x, na.rm = T)),
            .by = mechanism) %>%
  mutate(psnu = NA) %>%
  bind_rows(df_ovc_sum, .)

# OU/Agency summary
df_ovc_sum <- df_ovc_sum %>%
  filter(is.na(psnu)) %>%
  summarise(across(starts_with("OVC"), ~sum(.x, na.rm = T))) %>%
  bind_rows(df_ovc_sum, .)

df_ovc_sum %>%
  write_csv(na = "", file = file.path("Dataout/FY25_OVC_Targets for THRIVE IMS.csv"))

file.path("Dataout/FY25_OVC_Targets for THRIVE IMS.csv") %>% open_path()
