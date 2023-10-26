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
  library(janitor)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(googlesheets4)


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

  file_regs <- "1Lytpw6apvq0-nGsa1K9M4wM8867-9oKI5tV4fr4uvwo"

# Functions  =====

# LOAD DATA =====

  # Site x IM

  df_msd_sites <- file_site %>% read_psd()

  # Line List - Active Patients All

  dir_ll %>% list.files()

  df_ll <- dir_ll %>%
    return_latest(pattern = "Patient_Line_List - Active - ACE3 - Kebbi.csv") %>%
    read_csv()

  dir_ll %>%
    return_latest(pattern = "Patient_Line_List - Active - All.csv") %>%
    open_path()


  # Write data to g-sheet
  dir_ll %>%
    return_latest(pattern = "Patient_Line_List - Active - All.csv") %>%
    read_csv() %>%
    clean_names() %>%
    distinct(last_regimen) %>%
    filter(str_detect(last_regimen, "^\\d|Adult|Child", negate = T)) %>%
    arrange(last_regimen) %>%
    sheet_write(
      data = .,
      ss = as_sheets_id(file_regs),
      sheet = "NDR - Regimens"
    )

  # SCH - LMIS

  dir_sch %>% list.files()

  df_sch_lmis <- dir_sch %>%
    return_latest(pattern = "export-lmis.*.xlsx$") %>%
    #excel_sheets()
    read_excel(sheet = "HIV")

  df_sch_lmis %>%
    clean_names() %>%
    distinct(product_code, product) %>%
    arrange(product) %>%
    sheet_write(
      data = .,
      ss = as_sheets_id(file_regs),
      sheet = "NHLMIS - Regimens"
    )

# MUNGE =====

  ## TX

  df_tx <- df_msd_sites %>%
    filter(fiscal_year == meta$curr_fy,
           funding_agency == agency,
           str_detect(indicator, "TX_"))

  ## LL

  df_ll_report <- df_ll %>%
    select(-starts_with("...")) %>%
    clean_names()

  # df_ll_report %>% glimpse()
  #
  # df_ll_report %>% distinct(date_of_birth)
  # df_ll_report %>% distinct(art_start_date)
  # df_ll_report %>% distinct(last_clinic_visit_date)
  # df_ll_report %>% distinct(last_drug_pickup_date)

  df_ll_report <- df_ll_report %>%
    arrange(ip, state, lga, facility, date_of_birth) %>%
    group_by(ip, state, lga, facility) %>%
    mutate(patient_uid = row_number()) %>%
    ungroup() %>%
    mutate(patient_uid = paste0(
      datim_code, "-",
      str_pad(as.character(patient_uid),
              width= max(nchar(.$patient_uid)),
              pad = "0",
              side = "left"))) %>%
    relocate(patient_uid, .after = datim_code) %>%
    mutate(date_of_birth = ymd(date_of_birth),
           art_start_date = dmy(art_start_date),
           last_clinic_visit_date = dmy(last_clinic_visit_date),
           last_drug_pickup_date = dmy(last_drug_pickup_date))

  ## Extract Facilities Info

  df_locs <- df_ll_report %>%
    select(facility_uid = datim_code, lga, state, ip) %>%
    distinct_all()

  ## Extract Patients Demo/Personal Info - assuming each record represent unique patient

  df_patients <- df_ll_report %>%
    select(patient_uid, facility_uid = datim_code, sex,
           date_of_birth, current_age,
           art_start_date, age_at_art_initiation) %>%
    distinct_all()


  ## Restrict data to ARV Dispensing only
  df_arv <- df_ll_report %>%
    select(patient_uid, facility_uid = datim_code,
           last_clinic_visit_date,
           pregnancy_status,
           last_drug_pickup_date,
           last_regimen,
           days_of_arv_refill) %>%
    mutate(
      last_drug_pickup_year = year(last_drug_pickup_date),
      last_drug_pickup_month = month(last_drug_pickup_date),
      last_drug_pickup_month_lbl = month(last_drug_pickup_date, label = TRUE),
      last_drug_pickup_quarter = quarter(last_drug_pickup_date, fiscal_start = 10, with_year = TRUE),
      last_drug_pickup_quarter = paste0(
        "FY", str_sub(last_drug_pickup_quarter, 3, 4), "Q", str_sub(last_drug_pickup_quarter, -1)
      )
    )

  df_arv_viz <- df_arv %>%
    summarise(
      patients = n(),
      days_of_arv_refill = sum(days_of_arv_refill, na.rm  = T),
      .by = c(last_regimen, last_drug_pickup_year,
              last_drug_pickup_month, last_drug_pickup_month_lbl)
    )


  ## LMIS

  df_sch_lmis %>% glimpse()

  df_sch_lmis <- df_sch_lmis %>%
    clean_names() %>%
    distinct(start_month, end_month)

# VIZ =====



# OUTPUTS =====

