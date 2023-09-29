# PURPOSE: SI-Naija
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Match TX_CURR ARV Dispense with SCH ARV Issuance
# REF ID:  a56f752c
# LICENSE: MIT
# DATE:    2023-09-29
# UPDATE:  2023-09-29
# NOTES:   Getting the data right - explore ways to use SCH Data to validate TXC_CURR Numbers

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(glitr)
  library(glamr)
  library(gisr)
  library(gophr)
  library(scales)
  library(sf)
  library(lubridate)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)


# LOCALS & SETUP ====

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")

  dir_cntries <- file.path(dir_mer, "..", "PEPFAR/COUNTRIES")


  # Files

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21")
  file_site <- si_path() %>% return_latest("Site_IM_FY21")

  get_metadata(file_psnu)

  meta <- metadata

  # other Params

  ref_id <- "a56f752c"
  agency <- "USAID"
  cntry <- "Nigeria"

  dir_cntry <- file.path(dir_cntries, cntry)

  dir_sch <- file.path(dir_cntry, "SCH")
  dir_ll <- file.path(dir_cntry, "NDR/LineLists")

# Functions  =====

# LOAD DATA =====

  # Site x IM

  df_msd_sites <- file_site %>% read_psd()

  # Line List - Active Patients All

  dir_ll %>% list.files()

  df_ll <- dir_ll %>%
    return_latest(pattern = "Patient_Line_List - Active - ACE3 - Kebbi.csv") %>%
    read_csv()

  # SCH - LMIS

  dir_sch %>% list.files()

  df_sch_lmis <- dir_sch %>%
    return_latest(pattern = "export-lmis.*.xlsx$") %>%
    #excel_sheets()
    read_excel(sheet = "HIV")

# MUNGE =====

  ## TX

  df_tx <- df_msd_sites %>%
    filter(fiscal_year == meta$curr_fy,
           funding_agency == agency,
           str_detect(indicator, "TX_"))

  ## LL

  df_ll %>% glimpse()

  df_ll_report <- df_ll %>%
    select(-starts_with("...")) %>%
    clean_names()

  df_ll_report %>% glimpse()

  df_ll_report %>% distinct(date_of_birth)
  df_ll_report %>% distinct(art_start_date)
  df_ll_report %>% distinct(last_clinic_visit_date)
  df_ll_report %>% distinct(last_drug_pickup_date)

  df_ll_report <- df_ll_report %>%
    mutate(date_of_birth = ymd(date_of_birth),
           art_start_date = dmy(art_start_date),
           last_clinic_visit_date = dmy(last_clinic_visit_date),
           last_drug_pickup_date = dmy(last_drug_pickup_date),
           days_before_drug_pickup = last_drug_pickup_date - last_drug_pickup_date,
           months_since_art_start_date = interval(art_start_date, last_drug_pickup_date) %/% months(1),
           years_since_art_start_date = interval(art_start_date, last_drug_pickup_date) %/% years(1),
           last_drug_pickup_month = month(last_drug_pickup_date),
           last_drug_pickup_quarter = quarter(last_drug_pickup_date, fiscal_start = 10),
           last_drug_pickup_year = year(last_drug_pickup_date))

  ## LMIS

  df_sch_lmis %>% glimpse()

  df_sch_lmis %>%
    clean_names() %>%
    distinct(start_month, end_month)

# VIZ =====



# OUTPUTS =====

