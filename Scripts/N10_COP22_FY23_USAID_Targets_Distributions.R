##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: FY23 Targets Summary
##  LICENCE: MIT
##  DATE:    2022-10-13
##  UPDATED: 2022-10-13

# Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(grabr)
  library(glamr)
  library(gisr)
  library(sf)
  library(janitor)
  library(glue)
  library(gt)
  library(openxlsx)
  library(googlesheets4)
  library(googledrive)


  source("./Scripts/N00_Utilities.R")
  source("./Scripts/N00_Viz_Utilities.R")

# GLOBALS ----

  ## Dirs ----

  dir_merdata <- si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_images <- "Images"

  #dir_merdata %>% open_path()

  ## Files ----

  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY20.*_Nig")

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*_Nig")

  file_targets <- dir_data %>%
    return_latest(pattern = "Data for Targeted Travel")

  file_date <- file_psnu_im %>%
    str_extract("(?<=-\\d{2}_).*(?=_v)")

  file_date <- file_date %>%
    str_sub(1, 4) %>%
    paste0("-",
           str_sub(file_date, 5, 6), "-",
           str_sub(file_date, -2)) %>%
    lubridate::ymd()

  #open_path(file_radet)

  file_dgrive_id <- "1LaFmVQkVlFFH_zLG06oHrNAEAoW5seBy"

  ## Params ----

  agency <- "USAID"

  cntry <- "Nigeria"

  ou_uid <- get_ouuid(cntry)

  src_msd <- file_psnu_im %>% source_info(return = "source")
  curr_pd <- file_psnu_im %>% source_info(return = "period")
  curr_fy <- file_psnu_im %>% source_info(return = "fiscal_year")

  ages1 <- list(
    "Children" = c("<01", "01-04", "05-09", "10-14"),
    "Youth" = c("15-19"),
    "Young Adult" = c("20-24"),
    "Adult" = c("25-29", "30-34", "35-39", "40-44", "45-49",
                "50+", "50-54", "55-59", "60-64", "65+")
  )

  ages2 <- list(
    "<15" = c("<01", "01-04", "05-09", "10-14"),
    "15-19" = c("15-19"),
    "24-24" = c("20-24"),
    "25+" = c("25-29", "30-34", "35-39", "40-44", "45-49",
              "50+", "50-54", "55-59", "60-64", "65+")
  )

# FUNCTIONS ----

  #' @title Reshape Headers
  #'
  reshape_headers <- function(.df_sum) {
    .df_sum %>%
      names() %>%
      tibble(colname = .) %>%
      mutate(tech_area = str_remove(colname, "(?<=.)_.*"),
             indicator = str_remove(colname, "(.target|.trgt_prop)"),
             indicator = case_when(
               indicator %in% c("mech_code", "mech_name", "psnu", "age_group") ~ NA_character_,
               TRUE ~ indicator
             ),
             value = str_remove(colname, ".*(?=target|trgt_prop)"),
             value = str_to_upper(str_remove(value, "trgt_"))) %>%
      mutate(
        tech_area = case_when(
          tech_area == "mech_code" ~ NA_character_,
          tech_area == "CXCA" ~ "Cervical Cancer Screening",
          tech_area == "GEND" ~ "Gender-base Violence",
          str_detect(indicator, "HTS_INDEX|HTS_INDEX.*") ~ "HIV Index Testing",
          str_detect(indicator, "HTS_RECENT") ~ "HIV Recency Testing",
          str_detect(indicator, "HTS_SELF") ~ "HIV SELF Testing",
          str_detect(indicator, "HTS_TST") ~ "HIV Testing",
          str_detect(indicator, "PMTCT_STAT|PMTCT_EID") ~ "HIV Testing for ANC Clients & Infants",
          str_detect(indicator, "PMTCT_ART") ~ "HIV Treatment for ANC Clients",
          str_detect(indicator, "PrEP") ~ "HIV Prevention",
          str_detect(indicator, "TB_STAT") ~ "TB Testing",
          str_detect(indicator, "TB_PREV") ~ "TB Prevention",
          str_detect(indicator, "TB_ART") ~ "TB Treatment",
          str_detect(indicator, "TX_TB") ~ "TB Screening",
          str_detect(indicator, "TX_NEW|TX_CURR") ~ "HIV Treatment",
          str_detect(indicator, "TX_PVLS") ~ "VL Monitoring",
        )
      ) %>%
      relocate(colname, .after = last_col())
  }


  #' @title Mark Header Dissolution Points
  #'
  mark_dissolution <- function(.df_headers, colname = "tech_area") {
    cols <- names(.df_headers)

    if (colname == "tech_area") {
      cols <- colname
    }
    else {
      idx = which(cols == colname)
      cols = cols[1:idx]
    }

    .df_headers %>%
      mutate(start = row_number()) %>%
      group_by_at(.vars = all_of(cols)) %>%
      summarise(
        row = which(names(.df_headers) == {{colname}}),
        start = min(start),
        end = start + n() -1,
        .groups = "drop"
      ) %>%
      arrange(start)
  }


# Import Data ----

  file_dgrive_id %>%
    as_sheets_id() %>%
    drive_browse()

  # df_inds <- file_dgrive_id %>%
  #   as_sheets_id() %>%
  #   read_sheet()

  df_inds <- file_targets %>%
    read_excel(sheet = 2) %>%
    clean_names()

  df_inds <- df_inds %>%
    mutate(mer_indicator = case_when(
      mer_indicator == "PREP_NEW" ~ "PrEP_NEW",
      mer_indicator == "PREP_CT" ~ "PrEP_CT",
      TRUE ~ mer_indicator
    ))

  df_targets <- file_targets %>%
    read_excel(sheet = 1, skip = 4) %>%
    clean_names()

  df_psnu <- file_psnu_im %>%
    read_msd() %>%
    filter(fiscal_year == curr_fy + 1,
           funding_agency == agency) %>%
    clean_indicator() %>%
    filter(indicator %in% df_inds$mer_indicator)

  # MECHS
  df_mechs <- df_psnu %>%
    distinct(mech_code, mech_name) %>%
    mutate(
      mech_name = case_when(
        str_detect(mech_name, "Accelerating Control.*") ~ paste0("ACE ", str_extract(mech_name, "\\d")),
        str_detect(mech_name, "RISE") ~ "RISE",
        TRUE ~ mech_name
    )) %>%
    arrange(mech_name)

  # SUMMARY ----

  df_psnu_sum <- df_psnu %>%
    group_ages() %>%
    group_by(mech_code, psnu, indicator,
             standardizeddisaggregate, age_group) %>%
    summarise(target = sum(targets, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      age_group = case_when(
        is.na(age_group) & str_detect(standardizeddisaggregate, "KeyPop") ~ "Key Pop",
        is.na(age_group) & str_detect(standardizeddisaggregate, "^Total") ~ "Total",
        TRUE ~ age_group
      )
    )


  # REPORTS - IM Targets ----

  df_im_sum <- df_psnu_sum %>%
    filter(str_detect(standardizeddisaggregate, "Total")) %>%
    group_by(mech_code, indicator, standardizeddisaggregate, age_group) %>%
    summarise(across(target, sum, na.rm = TRUE), .groups = "drop") %>%
    select(-standardizeddisaggregate, -age_group)

  df_im_sum <- df_im_sum %>%
    group_by(indicator) %>%
    summarise(across(target, sum, na.rm = T), .groups = "drop") %>%
    mutate(mech_code = "") %>%
    bind_rows(df_im_sum) %>%
    group_by(indicator) %>%
    mutate(
      trgt_prop = percent(target / sum(target[mech_code != ""], na.rm = T), 1)
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator,
                names_prefix = "MSD",
                names_sort = T,
                names_glue = "{indicator}.{.value}",
                values_from = c(target, trgt_prop)) %>%
    left_join(df_mechs, by = "mech_code") %>%
    select(mech_code, mech_name, all_of(sort(names(select(., -mech_code)))))

  ## Table Header Section
  df_im_sum_header <- df_im_sum %>%
    reshape_headers()

  df_im_sum_header_ta <- df_im_sum_header %>%
    mark_dissolution(colname = "tech_area")

  df_im_sum_header_ind <- df_im_sum_header %>%
    mark_dissolution(colname = "indicator")

  df_im_sum_header <- df_im_sum_header %>%
    t() %>%
    as_tibble()


  # REPORTS - PSNUxIM Targets ----

  df_psnuxim_sum <- df_psnu_sum %>%
    filter(str_detect(standardizeddisaggregate, "Total")) %>%
    select(-standardizeddisaggregate, -age_group) %>%
    group_by(mech_code, psnu, indicator) %>%
    summarise(across(target, sum, na.rm = TRUE), .groups = "drop")

  df_psnuxim_sum <- df_psnuxim_sum %>%
    group_by(mech_code, indicator) %>%
    summarise(across(target, sum, na.rm = T), .groups = "drop") %>%
    mutate(psnu = "") %>%
    bind_rows(df_psnuxim_sum, .) %>%
    arrange(mech_code, indicator) %>%
    group_by(mech_code, indicator) %>%
    mutate(
      trgt_prop = percent(target / sum(target[psnu != ""], na.rm = T), 1)
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator,
                names_sort = T,
                names_glue = "{indicator}.{.value}",
                values_from = c(target, trgt_prop)) %>%
    left_join(df_mechs, by = "mech_code") %>%
    select(mech_code, mech_name, psnu, all_of(sort(names(select(., -mech_code)))))

  ## Table Header Section
  df_psnuxim_header <- df_psnuxim_sum %>%
    reshape_headers()

  df_psnuxim_header_ta <- df_psnuxim_header %>%
    mark_dissolution(colname = "tech_area")

  df_psnuxim_header_ind <- df_psnuxim_header %>%
    mark_dissolution(colname = "indicator")

  df_psnuxim_header <- df_psnuxim_header %>%
    t() %>%
    as_tibble()

  # REPORTS - PSNUxIMxAge Targets ----

  df_psnuximxage_sum <- df_psnu_sum %>%
    select(-standardizeddisaggregate) %>%
    group_by(mech_code, psnu, indicator) %>%
    mutate(
      trgt_prop = case_when(
        age_group %ni% c("Total", NA_character_) ~ percent(
          target/ sum(target[age_group %ni% c("Total", NA_character_)],
                      na.rm = T), 1),
        TRUE ~ percent(target/target, 1))
    ) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator,
                names_sort = T,
                names_glue = "{indicator}.{.value}",
                values_from = c(target, trgt_prop)) %>%
    left_join(df_mechs, by = "mech_code") %>%
    select(mech_code, mech_name, psnu, all_of(sort(names(select(., -mech_code)))))


  ## Table Header Section
  df_psnuximxage_header <- df_psnuximxage_sum %>%
    reshape_headers()

  df_psnuximxage_header_ta <- df_psnuximxage_header %>%
    mark_dissolution(colname = "tech_area")

  df_psnuximxage_header_ind <- df_psnuximxage_header %>%
    mark_dissolution(colname = "indicator")

  df_psnuximxage_header <- df_psnuximxage_header %>%
    t() %>%
    as_tibble()

# EXPORT ----

  ## Output

  file_im_sum <- file.path(dir_dataout,
                           paste0("FY23 - USAID Nigeria Targets Distribution - ",
                                  curr_date(), ".xlsx"))

  wb <- createWorkbook()

  ## IM Targets Summary

  sheet_im <- "IM Summary"

  addWorksheet(wb, sheetName = sheet_im)

  df_im_sum_header_ta %>%
    pwalk(function(tech_area, row, start, end){
      mergeCells(wb = wb, sheet = sheet_im, rows = row, cols = c(start, end))
    })

  df_im_sum_header_ind %>%
    pwalk(function(tech_area, indicator, row, start, end){
      mergeCells(wb = wb, sheet = sheet_im, rows = row, cols = c(start, end))
    })

  #writeDataTable(wb = wb, sheet = sheet_im, x = df_im_sum_header, colNames = F)
  writeData(wb = wb, sheet = sheet_im, x = df_im_sum_header, colNames = F)

  writeData(wb = wb, sheet = sheet_im, x = df_im_sum, colNames = F,
            startCol = 1, startRow = nrow(df_im_sum_header) + 1)


  ## PSNUxIM Targets Summary

  sheet_psnuxim <- "PSNUxIM"

  addWorksheet(wb, sheetName = sheet_psnuxim)

  df_psnuxim_header_ta %>%
    pwalk(function(tech_area, row, start, end){
      mergeCells(wb = wb, sheet = sheet_psnuxim, rows = row, cols = c(start, end))
    })

  df_psnuxim_header_ind %>%
    pwalk(function(tech_area, indicator, row, start, end){
      mergeCells(wb = wb, sheet = sheet_psnuxim, rows = row, cols = c(start, end))
    })

  writeData(wb = wb, sheet = sheet_psnuxim,
            x = df_psnuxim_header, colNames = F)

  writeData(wb = wb, sheet = sheet_psnuxim,
            x = df_psnuxim_sum, colNames = F,
            startCol = 1, startRow = nrow(df_psnuxim_header) + 1)

  ## PSNUxIMxAge Targets Summary

  sheet_psnuximxage <- "PSNUxIMxAge"

  addWorksheet(wb, sheetName = sheet_psnuximxage)

  df_psnuximxage_header_ta %>%
    pwalk(function(tech_area, row, start, end){
      mergeCells(wb = wb, sheet = sheet_psnuximxage, rows = row, cols = c(start, end))
    })

  df_psnuximxage_header_ind %>%
    pwalk(function(tech_area, indicator, row, start, end){
      mergeCells(wb = wb, sheet = sheet_psnuximxage, rows = row, cols = c(start, end))
    })

  writeData(wb = wb, sheet = sheet_psnuximxage,
            x = df_psnuximxage_header, colNames = F)

  writeData(wb = wb, sheet = sheet_psnuximxage,
            x = df_psnuximxage_sum, colNames = F,
            startCol = 1, startRow = nrow(df_psnuximxage_header) + 1)

  # Save Workbook

  saveWorkbook(wb = wb, file = file_im_sum, overwrite = TRUE)

  open_path(file_im_sum)
















