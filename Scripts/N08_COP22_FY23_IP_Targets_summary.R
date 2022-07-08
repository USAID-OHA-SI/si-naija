##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: COP22/FY23 Targets Summary
##  LICENCE: MIT
##  DATE:    2022-0707
##

# Libraries ----

  library(tidyverse)
  library(gophr)
  library(tameDP)
  library(glamr)
  library(glitr)
  library(scales)
  library(extrafont)
  library(openxlsx)
  library(googledrive)

## GLOBALS ----

# Dirs ----

  dir_merdata <- si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_cop22 <- "../../PEPFAR/COUNTRIES/Nigeria/COPs/COP22"

  dir_cop22_targets <- file.path(dir_dataout, "COP22-IM-Targets")

  dir.create(dir_cop22_targets)

  #dir_cop22 %>% open_path()

# Files ----

file_site_im <- dir_merdata %>%
  glamr::return_latest(pattern = "Site_IM_FY20.*_N")

file_psnu_im <- dir_merdata %>%
  glamr::return_latest(pattern = "PSNU_IM_FY20.*_N")

file_cop22_dp <- dir_cop22 %>%
  return_latest("Nigeria_datapack.*Final.xlsx$", recursive = T)

# Params ----

  cntry <- "Nigeria"
  ou_uid <- get_ouuid(cntry)

  agency <- "USAID"

  curr_pd <- file_psnu_im %>% identify_pd()

  curr_fy <- curr_pd %>%
    str_sub(1, 4) %>%
    str_replace("FY", "20") %>%
    as.numeric()

  inds <- c("HTS_TST", "HTS_TST_POS",
            "TX_NEW", "TX_CURR",
            "TX_PVLS_D", "TX_PVLS_N")


# FUNCTION ----


# Load Data ----

  # MSD - PSNU x IM ----
  df_psnu <- file_psnu_im %>% read_msd()

  df_ims <- df_psnu %>%
    filter(fiscal_year == "2023", funding_agency == "USAID") %>%
    distinct(mech_code, mech_name, prime_partner_name)


  # MSD - Sites x IM ----
  #df_sites <- file_site_im %>% read_msd()

  # COP - Datapack
  df_dp <- file_cop22_dp %>%
    tame_dp(type = "PSNUxIM") %>%
    filter(mech_code != "00000") %>%
    clean_indicator()

  df_dp %>%
    distinct(indicator) %>%
    arrange(indicator) %>%
    pull()

  dp_disaggs <- df_dp %>%
    distinct(indicator, standardizeddisaggregate) %>%
    arrange(indicator) %>%
    mutate(source = "DP")

  msd_disaggs <- df_psnu %>%
    distinct(indicator, standardizeddisaggregate) %>%
    arrange(indicator) %>%
    mutate(source = "MSD")

  disaggs <- dp_disaggs %>%
    left_join(msd_disaggs,
              by = c("indicator", "standardizeddisaggregate"))

  df_clean <- df_dp %>%
    select(-c(mech_name, primepartner)) %>%
    left_join(df_ims, by = "mech_code") %>%
    filter(!is.na(mech_name)) %>%
    select(fiscal_year, fundingagency, countryname, psnuuid, psnu,
           mech_code, mech_name, prime_partner_name,
           indicator, standardizeddisaggregate, modality, statushiv,
           otherdisaggregate, ageasentered, sex, targets)


  # Calculate HTS_INDEX_NEWPOS & NEWNEG

  df_clean <- df_clean %>%
    filter(indicator == "HTS_INDEX", statushiv == "Positive") %>%
    mutate(indicator = "HTS_INDEX_NEWPOS") %>%
    bind_rows(df_clean, .)

  df_clean <- df_clean %>%
    filter(indicator == "HTS_INDEX", statushiv == "Negative") %>%
    mutate(indicator = "HTS_INDEX_NEWNEG") %>%
    bind_rows(df_clean, .)

  # Calculate HTS_TST_POS & NEG

  df_clean <- df_clean %>%
    filter(indicator == "HTS_TST",
           standardizeddisaggregate == "Modality/Age/Sex/HIVStatus",
           statushiv == "Positive",
           modality %ni% c("Index", "IndexMod", "PMTCT ANC", "TBClinic")) %>%
    mutate(indicator = "HTS_TST_POS") %>%
    bind_rows(df_clean, .)

  df_clean <- df_clean %>%
    filter(indicator == "HTS_TST", statushiv == "Negative") %>%
    mutate(indicator = "HTS_TST_NEG") %>%
    bind_rows(df_clean, .)

  # Calculate PMTCT_EID_Less_Equal_Two_Months

  df_clean <- df_clean %>%
    filter(indicator == "PMTCT_EID", ageasentered == "<=02 Months") %>%
    mutate(indicator = "PMTCT_EID_Less_Equal_Two_Months") %>%
    bind_rows(df_clean, .)

  # Calculate PMTCT_EID_D

  df_clean <- df_clean %>%
    filter(indicator == "PMTCT_STAT" & statushiv == "Positive" |
             indicator == "HTS_TST" & statushiv == "Positive" & modality == "Post ANC1") %>%
    mutate(indicator = "PMTCT_EID_D") %>%
    bind_rows(df_clean, .)

  # Calculate PMTCT_STAT_POS

  df_clean <- df_clean %>%
    filter(indicator == "PMTCT_STAT", statushiv == "Positive") %>%
    mutate(indicator = "PMTCT_STAT_POS") %>%
    bind_rows(df_clean, .)

  # Calculate PMTCT_ART_D

  df_clean <- df_clean %>%
    filter(indicator == "PMTCT_STAT_POS", statushiv == "Positive") %>%
    mutate(indicator = "PMTCT_ART_D") %>%
    bind_rows(df_clean, .)

  # Calculate TB_STAT_POS

  df_clean <- df_clean %>%
    filter(indicator == "TB_STAT", statushiv == "Positive") %>%
    mutate(indicator = "TB_STAT_POS") %>%
    bind_rows(df_clean, .)

  # Calculate TX_TB_POS & NEG

  df_clean <- df_clean %>%
    filter(indicator == "TX_TB_D", statushiv == "Positive") %>%
    mutate(indicator = "TX_TB_D_POS") %>%
    bind_rows(df_clean, .)

  df_clean <- df_clean %>%
    filter(indicator == "TX_TB_D", statushiv == "Negative") %>%
    mutate(indicator = "TX_TB_D_NEG") %>%
    bind_rows(df_clean, .)

  # Calculate OVC_SERV HIVSTAT, HIVSTAT_D, ACTIVE, GRADUATED, <18, 18+

  df_clean %>%
    filter(str_detect(indicator, "OVC_.*")) %>%
    distinct(indicator, standardizeddisaggregate, otherdisaggregate)

  df_clean <- df_clean %>%
    filter(indicator == "OVC_HIVSTAT") %>%
    mutate(indicator = "OVC_HIVSTAT_D") %>%
    bind_rows(df_clean, .)

  df_clean <- df_clean %>%
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Age/Sex/ProgramStatus") %>%
    mutate(indicator = "OVC_SERV_ALL") %>%
    bind_rows(df_clean, .)

  df_clean <- df_clean %>%
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Age/Sex/ProgramStatus",
           otherdisaggregate == "Act") %>%
    mutate(indicator = "OVC_SERV_ACTIVE") %>%
    bind_rows(df_clean, .)

  df_clean <- df_clean %>%
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Age/Sex/ProgramStatus",
           otherdisaggregate == "Grad") %>%
    mutate(indicator = "OVC_SERV_GRADUATED") %>%
    bind_rows(df_clean, .)

  df_clean <- df_clean %>%
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Age/Sex/ProgramStatus",
           ageasentered != "18+") %>%
    mutate(indicator = "OVC_SERV_UNDER_18") %>%
    bind_rows(df_clean, .)

  df_clean <- df_clean %>%
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Age/Sex/ProgramStatus",
           ageasentered == "18+") %>%
    mutate(indicator = "OVC_SERV_OVER_18") %>%
    bind_rows(df_clean, .)

  df_clean <- df_clean %>%
    filter(indicator != "OVC_SERV") %>%
    mutate(indicator = case_when(
      indicator == "OVC_SERV_ALL" ~ "OVC_SERV",
      TRUE ~ indicator
    ))


  # Targets Summary - IP x Indicator

  df_im_sum <- df_clean %>%
    group_by(fiscal_year, countryname, mech_code, mech_name,
             prime_partner_name, indicator) %>%
    summarise(across(targets, sum, na.rm = T), .groups = "drop")

  # Targets Summary - IP x PSNU x Indicator
  df_psnu_im_sum <- df_clean %>%
    group_by(fiscal_year, countryname, psnu, mech_code, mech_name,
             prime_partner_name, indicator) %>%
    summarise(across(targets, sum, na.rm = T), .groups = "drop")

  # Targets Summary - IP x PSNU x Indicator x age
  df_psnu_im_age_sum <- df_clean %>%
    mutate(
      age = case_when(
        indicator %in% c("CXCA_SCRN", "GEND_GBV") ~ NA_character_,
        ageasentered == "<=02 Months" ~ NA_character_,
        str_detect(indicator, "OVC_.*") &
          ageasentered %in% c("<01", "01-04", "05-09", "10-14", "15-17") ~ "<18",
        str_detect(indicator, "OVC_.*", negate = T) &
          ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "<15",
        str_detect(indicator, "OVC_.*", negate = T) &
          ageasentered %in% c("15-19", "15+", "20-24", "25-29",
                              "30-34", "35-39", "40-44", "45-49",
                              "50-54", "50+", "55-59", "60-64", "65+") ~ "15+",
        TRUE ~ ageasentered
    )) %>%
    group_by(fiscal_year, countryname, psnu, mech_code, mech_name,
             prime_partner_name, indicator, age) %>%
    summarise(across(targets, sum, na.rm = T), .groups = "drop")


# Export Targets

  df_ims %>%
    pull(mech_code) %>%
    walk(function(.x){

      # Output file name
      file_im_targets <- .x %>%
        paste0("COP22 - USAID Nigeria Targets IM-", ., ".xlsx") %>%
        file.path(dir_cop22_targets, .)

      # Create a workbook
      wb_trgts <- createWorkbook(creator = "Baboyma Kagniniwa",
                                 title = paste0("COP22 Targets for IM-", .x))

      # Write IM Level Targets

      addWorksheet(wb = wb_trgts, sheetName = "IM-Targets")

      df_im_sum %>%
        filter(mech_code == .x) %>%
        writeDataTable(
          wb = wb_trgts,
          sheet = "IM-Targets",
          x = .,
          startCol = "A",
          startRow = 1,
          colNames = TRUE
        )

      setColWidths(wb = wb_trgts,
                   sheet = "IM-Targets",
                   cols = 4:6,
                   widths = c(95, 55, 35))

      # Write PSNUxIM Level Targets

      addWorksheet(wb = wb_trgts, sheetName = "PSNUxIM-Targets")

      df_psnu_im_sum %>%
        filter(mech_code == .x) %>%
        writeDataTable(
          wb = wb_trgts,
          sheet = "PSNUxIM-Targets",
          x = .,
          startCol = "A",
          startRow = 1,
          colNames = TRUE
        )

      setColWidths(wb = wb_trgts,
                   sheet = "PSNUxIM-Targets",
                   cols = 5:7,
                   widths = c(95, 55, 35))

      # Write PSNUxIMxAge Level Targets

      addWorksheet(wb = wb_trgts, sheetName = "PSNUxIMxAge-Targets")

      df_psnu_im_age_sum %>%
        filter(mech_code == .x) %>%
        writeDataTable(
          wb = wb_trgts,
          sheet = "PSNUxIMxAge-Targets",
          x = .,
          startCol = 1,
          startRow = 1,
          colNames = TRUE
        )

      setColWidths(wb = wb_trgts,
                   sheet = "PSNUxIMxAge-Targets",
                   cols = 5:7,
                   widths = c(95, 55, 35))

      # Save Workbook to local drive
      saveWorkbook(
        wb = wb_trgts,
        file = file_im_targets,
        overwrite = TRUE
      )
    })

  # Check output files
  #open_path(dir_cop22_targets)

  # Load Files to GDrives
  trgts_files <- list.files(path = dir_cop22_targets,
                            pattern = "COP22 - .*\\d.xlsx$",
                            full.names = TRUE)

  gid <- "1LRMT7jb32M-33oGTAUC9yPFGIa-DTKqH"

  gdrive <- gdrive_folder(name = basename(dir_cop22_targets),
                          path = as_id(gid),
                          add = TRUE)

  walk(trgts_files, ~drive_upload(media = .x,
                                  path = gdrive,
                                  name = basename(.x),
                                  type = "spreadsheet"))






