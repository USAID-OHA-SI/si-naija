##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: COP20 OPU - OVC Targets and Achivements
##  LICENCE: MIT
##  DATE:    2022-01-24


# PACKAGES ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(tameDP)
  library(glitr)
  library(glamr)
  library(janitor)
  library(gt)

  source("./Scripts/N00_Utilties.R")

# Paths ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_opu <- "./../../PEPFAR/COUNTRIES/Nigeria/OPUs/COP20-FY21"
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

# Files ----

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v2_1_N.*.zip$")

  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v2_1_N.*.zip$")

# Params ----

  cntry <- "Nigeria"
  agency <- "USAID"
  agencies <- c("USAID", "CDC")

  rep_pd <- file_psnu_im %>% identify_pd()

  rep_fy <- rep_pd %>%
    str_sub(3, 4) %>%
    paste0("20", .) %>%
    as.integer()

# Data import ----

  # COP20 OPU

  df_opu <- dir_opu %>%
    return_latest("Updated OPU Data Pack_Nigeria_20210609_Final_v9.xlsx") %>%
    read_excel(sheet = "PSNUxIM", skip = 13)

  df_opu %>% glimpse()

  dir_opu %>%
    return_latest("Updated OPU Data Pack_Nigeria_20210609_Final_v9.xlsx") %>%
    tame_dp(type = "PSNUxIM")

  df_opu_clean <- df_opu %>%
    #select(c(PSNU:KeyPop, contains("DSD..."))) %>%
    select(c(PSNU:KeyPop, tidyselect::matches("DSD[.]{3}\\d{3}$"))) %>%
    rename_with(~stringr::str_replace(., "...[:digit:]{3}$", "_value")) %>%
    #rename_with(~stringr::str_replace(., "...[:digit:]{1,2}$", "_share")) %>%
    rename_all(tolower) %>%
    select(c(psnu:keypop, ends_with("value"))) %>%
    pivot_longer(cols = ends_with("value"),
                 names_to = "mechanism",
                 values_drop_na = TRUE) %>%
    separate(mechanism, into = c("mech_code", "indicatortype", "ind_value"), sep = "_") %>%
    select(-ind_value) %>%
    mutate(indicatortype = str_to_upper(indicatortype)) %>%
    filter(str_detect(indicator_code, "OVC_")) %>%
    # Clean indicators
    mutate(
      indicator_code = stringr::str_remove(indicator_code, "\\.(T_1|T)$"),
      indicator = stringr::str_extract(indicator_code, "[^\\.]+") %>% toupper,
      indicator = dplyr::recode(indicator, "VL_SUPPRESSED" = "VL_SUPPRESSION_SUBNAT"),
      numeratordenom = ifelse(stringr::str_detect(indicator_code, "\\.D\\.|\\.D$"), "D", "N"),
      statushiv = stringr::str_extract(indicator_code, "(Neg|Pos|Unk)$"),
      statushiv = dplyr::recode(statushiv, "Neg" = "Negative" , "Pos" = "Positive", "Unk" = "Unknown"),
      ageasentered = dplyr::case_when(stringr::str_detect(indicator_code, "12") ~ "02 - 12 Months",
                                      stringr::str_detect(indicator_code, "\\.2") ~ "<=02 Months",
                                      TRUE ~ age),
      otherdisaggregate =
        stringr::str_extract(indicator_code,
                             "(Act|Grad|Prev|DREAMS|Already|New\\.Neg|New\\.Pos|New|KnownNeg|KnownPos|Known.Pos|Routine|\\.S(?=\\.)|\\.S$|PE)") %>%
        stringr::str_remove("\\.")
    ) %>%
    # Orgunits
    split_psnu()

  df_opu_clean %>% distinct(mech_code)


  df_psnu_ovc <- df_opu_clean %>%
    filter(str_detect(indicator, "OVC_")) %>%
    group_by(psnuuid, psnu, indicator) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()



  # PSNU
  df_psnu <- file_psnu_im %>% read_msd()

  df_psnu %>%
    filter(str_detect(indicator, "OVC")) %>%
    distinct(indicator) %>% pull() %>% sort()

  df_psnu %>%
    filter(str_detect(indicator, "OVC")) %>%
    distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>%
    arrange(indicator)

  df_psnu %>%
    filter(str_detect(indicator, "OVC")) %>%
    distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>%
    arrange(indicator)

  df_psnu %>%
    filter(str_detect(indicator, "DREAMS"))

  df_ovc_serv <- df_psnu %>%
    filter(fiscal_year == rep_fy,
           indicator %in% c("OVC_SERV"),
           standardizeddisaggregate == "Age/Sex/ProgramStatus",
           ageasentered != "18+") %>%
    group_by(fiscal_year, fundingagency, mech_code, psnu, indicator) %>%
    summarise(across(c(targets, cumulative),
    sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      targets_opu = case_when(
        fundingagency == "USAID" & indicator == "OVC_SERV" & psnu == "Lagos" ~ 87569,
        TRUE ~ targets
      ),
      achv = round(cumulative / targets * 100, 2),
      achv_opu = round(cumulative / targets_opu * 100, 2)) %>%
    relocate(cumulative, .before = targets) %>%
    rename(results = cumulative) %>%
    clean_agency()

  df_ovc_serv_ou <- df_psnu %>%
    filter(fiscal_year == rep_fy,
           indicator == "OVC_SERV" & standardizeddisaggregate == "Age/Sex/ProgramStatus" & ageasentered != "18+" |
           indicator == "OVC_HIVSTAT" & standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, fundingagency, mech_code, psnu, indicator) %>%
    summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      targets_opu = case_when(
        fundingagency == "USAID" & indicator == "OVC_SERV" & psnu == "Lagos" ~ 87569,
        TRUE ~ targets
      )) %>%
    group_by(fiscal_year, fundingagency, indicator) %>%
    summarise(across(c(targets, targets_opu, cumulative), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      achv = round(cumulative / targets * 100, 2),
      achv_opu = round(cumulative / targets_opu * 100, 2)
    ) %>%
    relocate(cumulative, .before = targets) %>%
    rename(results = cumulative) %>%
    clean_agency()

  df_ovc <- df_psnu %>%
    filter(fiscal_year == rep_fy,
           indicator %in% c("OVC_SERV", "OVC_HIVSTAT"),
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, fundingagency, mech_code, psnu, indicator) %>%
    summarise(across(c(targets, cumulative
                       #,starts_with("qtr")
                       ),
                     sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      targets_opu = case_when(
        fundingagency == "USAID" & indicator == "OVC_SERV" & psnu == "Lagos" ~ 87569,
        TRUE ~ targets
      ),
      achv = round(cumulative / targets * 100, 2),
      achv_opu = round(cumulative / targets_opu * 100, 2)) %>%
    relocate(cumulative, .before = targets) %>%
    rename(results = cumulative) %>%
    clean_agency()

  df_ovc %>%
    filter(fundingagency == "USAID") %>%
    select(-c(fiscal_year, mech_code)) %>%
    group_by(fundingagency, indicator) %>%
    gt() %>%
    tab_header(title = "USAID - FY21 OVC Achievements") %>%
    opt_all_caps(
      all_caps = TRUE
    ) %>%
    fmt_number(
      columns = c(results, targets, targets_opu),
      decimals = 0
    ) %>%
    gtsave(filename = file.path(dir_graphics, "USAID-FY21-OVC Achievements.png"))


  df_ovc %>%
    filter(fundingagency == "CDC") %>%
    select(-c(fiscal_year, mech_code)) %>%
    group_by(fundingagency, indicator) %>%
    gt() %>%
    tab_header(title = "CDC - FY21 OVC Achievements") %>%
    opt_all_caps(
      all_caps = TRUE
    ) %>%
    fmt_number(
      columns = c(results, targets, targets_opu),
      decimals = 0
    ) %>%
    gtsave(filename = file.path(dir_graphics, "CDC-FY21-OVC Achievements.png"))


  df_ovc  %>%
    group_by(fundingagency, indicator) %>%
    summarise(across(c(results, targets, targets_opu),
      sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      achv = round(results / targets * 100, 2),
      achv_opu = round(results / targets_opu * 100, 2)) %>%
    group_by(indicator) %>%
      gt() %>%
      tab_header(title = "NIGERIA - FY21 OVC Achievements") %>%
      opt_all_caps(
        all_caps = TRUE
      ) %>%
      fmt_number(
        columns = c(results, targets, targets_opu),
        decimals = 0
      ) %>%
      gtsave(filename = file.path(dir_graphics, "NIGERIA-FY21-OVC Achievements.png"))


  df_ovc_serv_ou %>%
    group_by(indicator) %>%
    gt() %>%
    tab_header(title = "NIGERIA - FY21 OVC Under 18yo Achievements") %>%
    opt_all_caps(
      all_caps = TRUE
    ) %>%
    fmt_number(
      columns = c(results, targets, targets_opu),
      decimals = 0
    ) %>%
    gtsave(filename = file.path(dir_graphics, "NIGERIA-FY21-OVC Under 18yo Achievements.png"))












