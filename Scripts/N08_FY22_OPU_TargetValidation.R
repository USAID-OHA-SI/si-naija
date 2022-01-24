##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: FY22 OPU Targets validation
##  LICENCE: MIT
##  DATE:    2022-01-05
##

## Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glamr)
  library(janitor)
  library(glue)

## GLOBALS ----

  dir_merdata <- glamr::si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"

  cntry <- "Nigeria"

  inds <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS_N")

## DATA ----

  # MSDs - Nigeria
  df_psnu <- glamr::return_latest(
    folderpath = dir_merdata,
    pattern = "PSNU_IM_.*_Nigeria") %>%
    read_msd()

  df_psnu %>% glimpse()
  df_psnu %>% distinct(indicator)

  df_psnu_im <- df_psnu %>%
    filter(fiscal_year == 2022,
           fundingagency == "USAID",
           standardizeddisaggregate == "Total Numerator") %>%
    clean_indicator() %>%
    filter(indicator %in% inds) %>%
    #group_by(fiscal_year, fundingagency, psnuuid, psnu, mech_code, indicator) %>%
    group_by(fiscal_year, fundingagency, mech_code, indicator) %>%
    summarise(across(targets, sum, na.rm = TRUE), .groups = "drop")


  # OPU - Targets
  df_opu <- return_latest(
    folderpath = paste0(si_path(), "/../PEPFAR/COUNTRIES/Nigeria/FY22 - OPU"),
    pattern = "OPU.*_Nigeria_\\d{8}.xlsx") %>%
    read_excel(sheet = "PSNUxIM",
               skip = 13,
               col_types = "text",
               .name_repair = "unique") %>%
    datapackr::unPackDataPack()

  df_opu <- return_latest(
      folderpath = paste0(si_path(), "/../PEPFAR/COUNTRIES/Nigeria/FY22 - OPU"),
      pattern = "OPU.*_Nigeria_\\d{8}.xlsx") %>%
    read_excel(sheet = "PSNUxIM",
               skip = 13,
               col_types = "text",
               .name_repair = "unique")

  df_opu <- df_opu %>%
    select(-starts_with("...")) %>%
    rename_with(str_to_lower) %>%
    rename_with(~str_replace(., "...[:digit:]{3}$", "_value")) %>%
    rename_with(~str_replace(., "...[:digit:]{1,2}$", "_share")) %>%
    tameDP::reshape_dp() %>%
    tameDP::convert_dedups()

  # Summary by PSNU
  df_opu_psnu <- df_opu %>%
    tameDP::agg_dp(psnu_lvl = T) %>%
    tameDP::clean_indicators()

  # Summary by
  df_opu_im <- df_opu %>%
    tameDP::agg_dp(psnu_lvl = F) %>%
    tameDP::clean_indicators()


  df_opu_usaid <- df_opu_im %>%
    filter(indicator %in% inds) %>%
    group_by(mech_code, indicator) %>%
    summarise(across(targets, sum, na.rm = T), .groups = "drop")


  df_nga <- df_psnu_im %>%
    full_join(df_opu_usaid, by = c("indicator", "mech_code")) %>%
    rename(datim = targets.x, revised = targets.y) %>%
    filter(fundingagency == "USAID") %>%
    select(-c(fundingagency, fiscal_year)) %>%
    arrange(mech_code, indicator) %>%
    pivot_wider(names_from = indicator, values_from = c(datim, revised)) %>%
    arrange(mech_code)







# OLD

  names(df_opu) %>% sort()

  key_cols <-

  mechs <- df_opu %>%
    select(starts_with("dedup"),
           matches(glue("^({paste(1:9, collapse='|')})."))) %>%
    names()

  df_opu <- df_opu %>%

    pivot_longer(cols = matches("\\d"),
                 names_to = "mechanism_id",
                 values_to = "target_share") %>%
    clean_names() %>%
    separate(col = mechanism_id,
             into = c("mech_code", "indicatortype", "col_idx"),
             sep = "[^[:alnum:]]+",
             remove = F) %>%
    select(-c(mechanism_id, col_idx))

  df_opu <- df_opu %>%
    mutate(psnu = str_extract(psnu, "(?<=>).*"),
           psnu = str_remove(psnu, " \\[.*"),
           id = str_extract(id, "(?<=\\]).*"),
           id = str_remove(id, "\\].*"),
           id = str_remove(id, "\\[")) %>%
    rename(psnuuid = id) %>%
    relocate(psnuuid, .before = 1)

  df_opu %>% distinct(indicator_code)

  df_opu <- df_opu %>%
    mutate(
      indicator_code = str_remove(indicator_code, ".T$"),
      indicator = str_extract(indicator_code, "[^\\.]+"),
      numeratordenom = ifelse(str_detect(indicator_code, "\\.D\\."), "D", "N"),
      statushiv = str_extract(indicator_code, "(?<=\\.)(Neg|Pos|Unk)(?=\\.)"),
      statushiv = recode(statushiv,  "Neg" = "Negative" , "Pos" = "Positive", "Unk" = "Unknown"),
      otherdisaggregate = str_extract(indicator_code, "(Act|Grad|Prev|DREAMS|Already|New\\.Neg|New\\.Pos|New|KnownNeg|KnownPos|Routine|\\.S(?=\\.)|PE)"),
      otherdisaggregate = str_remove(otherdisaggregate, "\\."))

  df_opu_im <- df_opu %>%
    filter(indicator %in% inds) %>%
    mutate(data_pack_target = as.integer(data_pack_target)) %>%
    group_by(mech_code, indicator) %>%
    summarise(data_pack_target = sum(data_pack_target, na.rm = T)) %>%
    ungroup()

  df_nga <- df_opu_im %>%
    left_join(df_psnu_im, by = c("indicator", "mech_code")) %>%
    filter(fundingagency == "USAID") %>%
    select(-c(fundingagency, fiscal_year)) %>%
    pivot_wider(names_from = indicator, values_from = c(targets, data_pack_target)) %>%
    arrange(mech_code)


