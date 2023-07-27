##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: FY22 OPU Targets validation
##  LICENCE: MIT
##  DATE:    2023-07-27
##  UPDATED: 2023-07-27

## Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glamr)
  library(glue)
  library(openxlsx)

  source("./Scripts/N00_Utilities.R")

## GLOBALS ----

  # Directories

  dir_merdata <- si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_cop21 <- "../../PEPFAR/COUNTRIES/Nigeria/OPUs/COP22-FY23"

  #dir_cop21 %>% open_path()

  # Files
  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY21.*_N")

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY21.*_N")

  file_opu_dp <- dir_cop21 %>%
    return_latest("^OPU Data Pack_Nigeria_\\d{8}.*.xlsx$")

  # Params
  cntry <- "Nigeria"
  agency <- "USAID"

  curr_pd <- "FY23Q2"

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
  df_psnu <- file_psnu_im %>% read_psd()

  df_psnu %>% glimpse()

  # OVC and GBV Indicators
  df_inds <- df_psnu %>%
    filter(str_detect(indicator, "OVC|GBV")) %>%
    distinct(indicator, standardizeddisaggregate) %>%
    arrange(indicator)

  #
  df_targets <- df_inds %>%
    rename(ind = indicator, disagg = standardizeddisaggregate) %>%
    pmap_dfr(function(ind, disagg) {
      df_psnu %>%
        filter(fiscal_year == curr_fy,
               indicator == ind,
               standardizeddisaggregate == disagg) %>%
        summarise(targets = sum(targets, na.rm = T),
                 .by = c(funding_agency, mech_code, mech_name, prime_partner_name,
                         psnu, indicator, standardizeddisaggregate)) %>%
        filter(funding_agency == agency, targets > 0)
    })

  df_targets_gen_ovc <- df_targets %>%
    summarise(psnu = paste0(unique(psnu), collapse = ", "),
              targets = sum(targets, na.rm = T),
              .by = c(funding_agency, mech_code, mech_name, prime_partner_name,
                      indicator, standardizeddisaggregate)) %>%
    arrange(mech_code, indicator, standardizeddisaggregate)

## Outputs

  ## Save look up tables
  wb <- createWorkbook()

  sheet_name <- "FY23 Targets - OVC & GBV"

  file_targets <- "FY23 Targets - OVC & GBV - Budget OPU Validations.xlsx"

  addWorksheet(wb, sheetName = sheet_name)

  writeDataTable(wb = wb,
                 sheet = sheet_name,
                 x = df_targets_gen_ovc)

  saveWorkbook(wb = wb,
               file = file.path(dir_dataout, file_targets),
               overwrite = TRUE)

  file_targets %>% open_path()
