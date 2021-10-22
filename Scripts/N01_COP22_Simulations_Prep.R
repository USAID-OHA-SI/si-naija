##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: COP22 Prep
##  LICENCE: MIT
##  DATE:    2021-09-14
##  UPDATED: 2021-10-18


# PACKAGES -------------------------------------------------

  library(tidyverse)
  library(readxl)
  #library(readxlsb)
  library(gophr)
  library(glitr)
  library(glamr)
  library(gisr)
  library(janitor)


# GLOBAL ----

# DIR - Global ----

  dir_merdata <- glamr::si_path("path_msd")
  dir_datapack <- "../../PEPFAR/COUNTRIES/Nigeria/DataPack"
  dir_dp22 <- paste0(dir_datapack, "/DP-Nigeria-Version")
  dir_geodata <- glamr::si_path("path_vector")
  dir_terr <- glamr::si_path("path_raster")

# DIR - Project ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

# Country name ----

  # Country name
  cntry <- "Nigeria"

  # Agency
  agency <- "USAID"

# FILES ----

  # file - COP DataPack
  file_cop22_targets <- return_latest(
    folderpath = dir_dp22,
    pattern = "^Facility Target Projection_.*.xlsb$")

  file_cop22_targets2 <- return_latest(
    folderpath = dir_dp22,
    pattern = "^Facility Target Projection2_.*.xlsx$")

  file_prev_cop_targets <- return_latest(
    folderpath = dir_dp22,
    pattern = "^Nigeria_COP21_DataPack_.*.xlsx$")

  file_spectrum <- return_latest(
    folderpath = dir_dp22,
    pattern = "^Spectrum extract .*.xlsx$")

  # Latest MSD PSNU x IM File
  file_msd_nat <- return_latest(
    folderpath = dir_merdata,
    pattern = "MER_S.*_NAT_SUBNAT_.*.zip")

  # Latest MSD PSNU x IM File
  file_msd_psnu <- return_latest(
    folderpath = dir_merdata,
    pattern = "MER_S.*_PSNU_IM_.*_N.*.zip")

  # Latest MSD Site x IM File
  file_msd_sites <- return_latest(
    folderpath = dir_merdata,
    pattern = "MER_S.*_Site_IM_.*_N.*.zip")

# FUNCTIONS ----

  #' @title Extract Country Prioritization
  #'
  #'
  extract_prioritization <- function(dp_file,
                                     sheet = "Prioritization") {
    dp_file %>%
      readxl::read_excel(sheet = sheet, skip = 13) %>%
      janitor::clean_names() %>%
      separate(psnu, into = c("psnu", "psnuuid"), sep = " \\[#SNU\\] \\[| \\[#Military\\] \\[") %>%
      mutate(psnuuid = str_remove_all(psnuuid, "\\]")) %>%
      rename_with(.cols = starts_with("impatt"), .fn = str_remove, pattern = "^impatt_")
  }

  #' @title Extract PLHIV Estimates
  #'
  #'
  extract_plhiv <- function(dp_file,
                            sheet = "Spectrum") {

    # Read Spectrum data
    df_plhiv <- dp_file %>%
      readxl::read_excel(sheet = sheet) %>%
      janitor::clean_names()

    # Position of psnu column
    idx_ref_col <- which(names(df_plhiv) == "psnu")

    # Remove junk cols
    if (idx_ref_col > 1) {
      rm_cols <- 1:(idx_ref_col - 1)

      df_plhiv <- df_plhiv %>%
        select(-all_of(rm_cols)) %>%
        group_by(psnu) %>%
        mutate(total_psnu = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        relocate(total_psnu, .after = value)
    }

    return(df_plhiv)
  }

  #' @title Define Surge States
  #'
  #'
  define_surge <- function(df_msd,
                           cntry = "Nigeria",
                           fy = "2021",
                           limit = 20000,
                           ...) {
    df_msd %>%
      filter(fiscal_year == fy,
             operatingunit == cntry,
             indicator %in% c("TX_CURR_SUBNAT", "PLHIV"),
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
      group_by(psnu, psnuuid, indicator, ...) %>%
      summarise(across(targets, sum, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = indicator, values_from = targets) %>%
      clean_names() %>%
      #rename(age = ageasentered) %>%
      mutate(gap = plhiv - tx_curr_subnat,
             surge = case_when(gap > limit ~ "Y", TRUE ~ "N"))
  }

  #' @title Global Funds Sites
  #'
  #'
  extract_assumptions <- function(trgt_file, sheet = 1) {

    # Read file/sheet content
    if (str_detect(trgt_file, ".xlsb$")) {
      df_assum <- trgt_file %>%
        readxlsb::read_xlsb(path = .,
                            sheet = sheet,
                            skip = 1,
                            package = "readxlsb")
    } else {
      df_assum <- trgt_file %>%
        readxl::read_excel(path = ., sheet = sheet, skip = 1)
    }

    # Clean data
    df_assum %>%
      janitor::clean_names() %>%
      select(state, lga,
             orgunit = organisation_unit_period,
             sex, age,
             site_high_vol = is_facility_a_high_volume_site_y_n,
             site_high_plhiv = is_facility_in_a_high_plhiv_burden_state_y_n,
             site_prev_gf = is_facility_a_previously_gf_site_y_n,
             site_surge_state = is_state_a_surge_state_y_n,
             site_sat_state = is_facility_in_a_saturated_state_y_n,
             site_more_fund = will_there_be_extra_funding_from_pepfar_for_the_state_y_n)

  }

# DATA ----

  # SubNat x IM
  df_msd_nat <- file_msd_nat %>% read_msd()

  # PLHIV ----
  # From Sample data
  df_plhiv <- file_prev_cop_targets %>%
    extract_plhiv()

  df_plhiv %>% distinct(psnu, total_psnu)

  # From MSD - SUBNAT x IM
  df_plhiv <- define_surge(df_msd = df_msd_nat)

  # Assumptions
  df_assum <- file_cop22_targets2 %>%
    extract_assumptions()

  df_assum %>% glimpse()

  ## A1: High Volume Site
  df_hvol <- df_assum %>%
    distinct(state, orgunit, site_high_vol) %>%
    rename(assum_high_vol = site_high_vol)

  ## A2: High PLHIV Burden
  df_hhiv <- df_assum %>%
    rename(assum_high_plhiv = site_high_plhiv) %>%
    group_by(state) %>%
    mutate(assum_high_plhiv = case_when(
      "Y" %in% assum_high_plhiv ~ "Y",
      TRUE ~ "N"
    )) %>%
    ungroup() %>%
    distinct(state, assum_high_plhiv)


  ## A3: Global Fund
  df_gf <- df_assum %>%
    rename(assum_gf_site = site_prev_gf) %>%
    group_by(state, orgunit) %>%
    mutate(assum_gf_site = case_when(
      "Y" %in% assum_gf_site ~ "Y",
      TRUE ~ "N"
    )) %>%
    ungroup() %>%
    distinct(state, orgunit, assum_gf_site) %>%
    filter(assum_gf_site == "Y")

  df_gf %>%
    write_csv(file = "Dataout/Simulation - GF Sites.csv")

  ## A4: Surge State
  df_surge <- df_assum %>%
    rename(assum_surge_state = site_surge_state) %>%
    group_by(state) %>%
    mutate(assum_surge_state = case_when(
      "Y" %in% assum_surge_state ~ "Y",
      TRUE ~ "N"
    )) %>%
    ungroup() %>%
    distinct(state, assum_surge_state)


  ## A5: Program Saturation
  df_sat <- df_assum %>%
    rename(assum_sat_state = site_sat_state) %>%
    group_by(state) %>%
    mutate(assum_sat_state = case_when(
      "Y" %in% assum_sat_state ~ "Y",
      TRUE ~ "N"
    )) %>%
    ungroup() %>%
    distinct(state, assum_sat_state)


  ## A6: More Funding
  df_funds <- df_assum %>%
    rename(assum_more_fund = site_more_fund) %>%
    group_by(state) %>%
    mutate(assum_more_fund = case_when(
      "Y" %in% assum_more_fund ~ "Y",
      TRUE ~ "N"
    )) %>%
    ungroup() %>%
    distinct(state, assum_more_fund)

  # ALL Assumptions
  df_assums <- df_hhiv %>%
    left_join(df_surge, by = "state") %>%
    left_join(df_sat, by = "state") %>%
    left_join(df_funds, by = "state")

  df_assums %>%
    write_csv(file = "Dataout/Simulation - Assumptions.csv")

  # Get list of sheet names from Target Facility Projection ---
  dt_sheets <- file_cop22_targets2 %>%
    readxl::excel_sheets()

  dt_sheets%>%
    tibble(indicator = .) %>%
    filter(str_detect(indicator, "HIV")) %>%
    gt::gt()

  ref_inds <- dt_sheets %>%
    str_remove("(?<=\\().*") %>%
    str_remove("\\(") %>%
    str_trim() %>%
    str_remove("_N$") %>%
    unique()

  ref_inds <- ref_inds[!str_detect(ref_inds, "HIV")]

  ref_inds <- ref_inds %>% c(., "HTS_TST") %>% sort()