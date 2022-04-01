##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: FY22 OPU Targets validation
##  LICENCE: MIT
##  DATE:    2022-01-05
##  UPDATED: 2022-03-28

## Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glamr)
  library(tameDP)
  library(janitor)
  library(fuzzyjoin)
  library(glue)

  library(datapackr)
  library(datimutils)
  library(datimvalidation)

  source("./Scripts/N00_Utilities.R")

## GLOBALS ----

  # Directories

  dir_merdata <- si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_cop21 <- "../../PEPFAR/COUNTRIES/Nigeria/OPUs/COP21-FY22"

  #dir_cop21 %>% open_path()

  # Files
  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY20.*_N")

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*_N")

  file_opu_dp <- dir_cop21 %>%
    return_latest("^OPU Data Pack_Nigeria_\\d{8}.*.xlsx$")

  file_opu_checks <- dir_cop21 %>%
    return_latest("^OPU target.*.xlsx$")

  file_opu_flats <- dir_cop21 %>%
    return_latest("^OPU_Targets_Summary.*.xlsx$")

  # Params
  cntry <- "Nigeria"
  agency <- "USAID"

  curr_pd <- file_psnu_im %>% identify_pd()

  curr_fy <- curr_pd %>%
    str_sub(1, 4) %>%
    str_replace("FY", "20") %>%
    as.numeric()

  prev_fy <- curr_fy - 1

  inds <- c("HTS_TST", "HTS_TST_POS",
            "TX_NEW", "TX_CURR",
            "TX_PVLS", "TX_PVLS_D")


## FUNCTION ----


## DATA ----

  # MSDs - Nigeria ----
  df_psnu <- file_psnu_im %>% read_msd()

  df_psnu %>% glimpse()

  df_psnu %>%
    distinct(indicator) %>%
    arrange(indicator) %>%
    prinf()

  # Prioritization
  df_psnu %>%
    get_prioritization(fy = curr_fy) %>%
    prinf()

    #pull(standardizeddisaggregate) %>% unique()

  # List of msd indicators => 109
  inds_all <- df_psnu %>%
    distinct(indicator) %>%
    arrange(indicator)

  #inds_all %>% prinf()

  # List of required indicators => 35
  inds_req <- df_psnu %>%
    filter(source_name == "DATIM") %>%
    distinct(indicator) %>%
    arrange(indicator)

  #inds_req %>% prinf()

  # Map base to calculated indicators
  df_inds <- inds_req %>%
    rename(base_indicator = indicator) %>%
    fuzzy_join(x = inds_all, y = .,
               by = c("indicator" = "base_indicator"),
               match_fun = str_detect,
               mode = "left") %>%
    select(base_indicator, indicator) %>%
    mutate(
      base_indicator = case_when(
        is.na(base_indicator) & indicator == "TX_NET_NEW" ~ "TX_NEW",
        TRUE ~ base_indicator
      )
    ) %>%
    filter(str_detect(indicator, "^LAB|^HRF", negate = TRUE))


  # Indicators containing Age/Sex disagg
  df_msd_inds <- df_psnu %>%
    filter(str_detect(standardizeddisaggregate, "^Total.*tor$", negate = TRUE),
           str_detect(standardizeddisaggregate, "Age/Sex") |
           str_detect(standardizeddisaggregate, "KeyPop")) %>%
    select(indicator, numeratordenom, indicatortype,
           disaggregate, standardizeddisaggregate,
           ageasentered, sex,
           statushiv, statustb, statuscx,
           statustx = hiv_treatment_status,
           otherdisaggregate, modality, source_name) %>%
    distinct()


  # TX_PVLS Targets
  df_psnu %>%
    filter(fiscal_year == curr_fy,
           indicator == "TX_PVLS",
           str_detect(standardizeddisaggregate, "Total ")) %>%
    clean_indicator() %>%
    group_by(fundingagency, psnu, indicator) %>%
    summarise(across(targets, sum, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = targets) %>%
    filter(!(fundingagency == agency & str_detect(psnu, "_Mil"))) %>%
    arrange(fundingagency, psnu)

  # Indicator Type x IM
  df_psnu %>%
    filter(fiscal_year == curr_fy,
           indicator %in% inds,
           str_detect(standardizeddisaggregate, "Total ")) %>%
    clean_indicator() %>%
    clean_agency() %>%
    distinct(fundingagency, psnu, mech_code, indicator, indicatortype) %>%
    mutate(mech_code_dp = paste0(mech_code, "_", indicatortype))

  df_agency_im <- df_psnu %>%
    filter(fiscal_year == curr_fy) %>%
    clean_indicator() %>%
    clean_agency() %>%
    distinct(fundingagency, psnu, mech_code) %>%
    filter(!(str_detect(psnu, "_Mil") & fundingagency == "USAID")) %>%
    arrange(psnu, mech_code)


  # IM Targets Share
  df_psnu_im <- df_psnu %>%
    filter(fiscal_year == curr_fy,
           str_detect(standardizeddisaggregate, "Total ")) %>%
    clean_indicator() %>%
    filter(indicator %in% inds) %>%
    group_by(fiscal_year, fundingagency, psnu, indicator, indicatortype, mech_code) %>%
    summarise(across(targets, sum, na.rm = TRUE), .groups = "drop") %>%
    filter(targets > 0) %>%
    group_by(fiscal_year, fundingagency, psnu, indicator, indicatortype) %>%
    mutate(targets_psnu = sum(targets),
           targets_psnu_share = targets / sum(targets),
           targets_psnu_share_ttl = sum(targets_psnu_share)) %>%
    ungroup() %>%
    group_by(fiscal_year, fundingagency, indicator, indicatortype) %>%
    mutate(targets_agency = sum(targets),
           targets_agency_share = targets / sum(targets),
           targets_agency_share_ttl = sum(targets_agency_share)) %>%
    ungroup() %>%
    group_by(fiscal_year, indicator, indicatortype) %>%
    mutate(targets_ou = sum(targets),
           targets_ou_share = targets / sum(targets),
           targets_agency_share_ttl = sum(targets_ou_share)) %>%
    ungroup() %>%
    select(fiscal_year:mech_code, targets,
           ends_with(c("psnu", "agency", "ou")),
           ends_with(c("share")))


  # OPU - Targets ----

  # OPU - Flat file ----
  file_opu_flats %>% excel_sheets()

  # Orgs Reference
  df_ref <- file_opu_flats %>%
    read_excel(sheet = "PSNU-Lookup") %>%
    mutate(psnuuid = str_extract(PSNU, "(?<=\\] \\[).*(?=\\])"))

  # Flat file
  df_flat_raw <- file_opu_flats %>% read_excel(sheet = 1)

  df_flat_indicators <- df_flat_raw %>%
    distinct(indicator_code) %>%
    arrange(indicator_code)

  df_flat_indicators %>% prinf()

  df_flat <- df_flat_raw %>%
    left_join(df_ref, by = c("PSNU" = "STATE")) %>%
    rename(STATE = PSNU, PSNU = PSNU.y) %>%
    relocate(PSNU, .before = 1) %>%
    relocate(psnuuid, .after = STATE)

  df_flat <- df_flat %>%
    mutate(ID = paste0(PSNU, "|",
                       ifelse(is.na(Age), "", paste0(Age, "|")),
                       ifelse(is.na(Sex), "", Sex))) %>%
    relocate(ID, .after = KeyPop)


  # Check Age/Sex Options => Fix age/sex options
  df_flat %>% distinct(Age) %>% arrange(Age)
  df_flat %>% distinct(Sex) %>% arrange(Sex)

  df_flat %>%
    mutate(row = row_number()) %>%
    filter(Age %in% c("<1", "<01"))

  df_flat %>%
    mutate(row = row_number()) %>%
    filter(str_detect(Sex, "s$"))

  # Clean up Age/Sex
  df_flat <- df_flat %>%
    mutate(Age = str_remove(Age, "\\'"),
           Age = case_when(
             Age == "<1" ~ "<01",
             Age == "1-4" ~ "01-04",
             Age == "5-9" ~ "05-09",
             TRUE ~ Age),
           Sex = str_replace(Sex, "s", ""),
           Sex = str_replace(Sex, "Fa", "Fe"))

  # Check Age/Sex Options
  df_flat %>% distinct(Age) %>% arrange(Age)
  df_flat %>% distinct(Sex) %>% arrange(Sex)

  # Calculate Target Share at PSNU x IM x Indicator x Age x Sex
  df_flat <- df_flat %>%
    distinct() %>%
    group_by_at(vars(-c(Attribute, Value))) %>%
    mutate(Share = Value / sum(Value, na.rm = TRUE),
           DataPackTarget = sum(Value, na.rm = TRUE),
           Rollup = NA_integer_) %>%
    ungroup()

  # Check duplicates => Should be unique
  which(duplicated(df_flat))

  df_flat %>%
    distinct(STATE, indicator_code, Attribute, Share) %>%
    pivot_wider(names_from = Attribute,
                values_from = Share,
                values_fn = min)

  # Reshape data
  df_flat_to_dp <- df_flat %>%
    pivot_wider(names_from = Attribute,
                names_glue = "{Attribute}_{.value}",
                names_sort = TRUE,
                values_from = c(Share, Value),
                values_fn = first)

  df_flat_to_dp %>% glimpse()

  df_flat_to_dp %>%
    write_csv(file = "./Dataout/COP21_OPU_Targets_Shares.csv", na = "")

  open_path("./Dataout/COP21_OPU_Targets_Shares.csv")














## ----


  # TameDP
  df_dp_raw <- file_opu_dp %>% import_dp(tab = "PSNUxIM")

  df_dp_raw %>% glimpse()

  df_dp_raw_clean <- df_dp_raw %>%
    rename_with(tolower) %>%
    rename_with(~str_replace(., "deduplicated (dsd|ta) rollup.*", "dedup_\\1_value")) %>%
    split_psnu()

  df_dp_raw_clean %>% glimpse()

  key_cols <- c("psnu","indicator_code", "age", "sex", "keypop", "datapacktarget", "rollup")

  mechs <- df_dp_raw_clean %>%
    select(starts_with("dedup"), matches("^(1|2|3|4|5|6|7|8|9).")) %>%
    names()

  df_dp_raw_clean %>%
    select(key_cols, mechs) %>%
    glimpse()

  df_dp_raw_clean_share <- df_dp_raw_clean %>%
    select(key_cols, ends_with("_share")) %>% #glimpse()
    pivot_longer(cols = -key_cols,
                 names_to = c("mech_code", "indicatortype", ".value"),
                 names_sep = "_") %>%
    filter(!is.na(share))

  df_dp_raw_clean_value <- df_dp_raw_clean %>%
    select(key_cols, ends_with("_value")) %>%
    pivot_longer(cols = -key_cols,
                 names_to = c("mech_code", "indicatortype", ".value"),
                 names_sep = "_") %>%
      filter(!is.na(value))

  ## Compare DP vs Rollup => All match
  df_dp_raw_clean_value %>%
    mutate(check_values = datapacktarget == rollup) %>%
    filter(!check_values)

  ## Check DP IM Shares
  df_dp_share_checks <- df_dp_raw_clean_share %>%
    filter(mech_code != "dedup") %>%
    mutate(psnu = str_trim(psnu, side = "both")) %>%
    left_join(df_agency_im, by = c("psnu", "mech_code")) %>%
    mutate_at(vars(ends_with(c("value", "share"))), as.numeric) %>%
    group_by(psnu, indicator_code, age, sex, keypop) %>%
    mutate(share_ttl = sum(share, na.rm = TRUE),
           share_valid = share_ttl == 1.0000) %>%
    ungroup() %>%
    filter(share_valid != TRUE)

  df_dp_share_checks %>%
    distinct(share_ttl) %>%
    arrange(share_ttl) %>%
    prinf()

  df_dp_share_checks %>%
    filter(share_ttl < .9)

  df_dp_checks <- df_dp_raw_clean_value %>%
    left_join(df_dp_raw_clean_share) %>%
    mutate(psnu = str_trim(psnu, side = "both")) %>%
    left_join(df_agency_im, by = c("psnu", "mech_code")) %>%
    mutate_at(vars(ends_with(c("value", "share"))), as.numeric) %>%
    filter(mech_code != "dedup") %>%
    group_by(psnu, indicator_code, age, sex, keypop) %>%
    mutate(share_ttl = sum(share, na.rm = TRUE),
           share_valid = share_ttl == 1.0000) %>%
    ungroup()

  df_dp_checks_errors <- df_dp_checks %>%
    filter(share_valid != TRUE)

  df_dp_checks %>%
    distinct(share_ttl) %>%
    arrange(share_ttl) %>%
    prinf()

  df_dp_checks %>% filter(share_ttl < .99)

  # Check usaid mechs
  df_dp_checks %>%
    filter(mech_code %in% (df_agency_im %>%
             filter(fundingagency == "USAID") %>%
             pull(mech_code) %>%
             unique())) %>%
    distinct(psnu)





