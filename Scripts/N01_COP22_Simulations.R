##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: COP22 Prep
##  LICENCE: MIT
##  DATE:    2021-09-14
##  UPDATED: 2021-09-29


# PACKAGES -------------------------------------------------

  library(tidyverse)
  library(readxl)
  #library(readxlsb)
  library(gophr)
  library(glitr)
  library(glamr)
  library(janitor)
  library(extrafont)
  library(scales)
  library(gt)
  library(ggdist)
  library(ggridges)

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

  country <- "Nigeria"

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

#' @title Clean Mechs
#'
clean_mechs <- function(.data) {

  .data %>%
    mutate(
      mech_name = case_when(
        # USAID
        mech_name == "Meeting Targets and Maintaining Epidemic Control (EpiC)" ~ "EpiC",
        mech_name == "STRENGHTENING INTERGRATED DELIVERY OF HIV/AIDS SERVICES(SIDHAS)" ~ "SIDHAS",
        mech_name == "SHARP Task Order 1" ~ "SHARP TO1",
        mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 2" ~ "SHARP TO2",
        mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 3" ~ "SHARP TO3",
        mech_name == "Reaching Impact, Saturation and Epidemic Control (RISE)" ~ "RISE",
        mech_name == "Care and Treatment in Sustained Support (CaTSS)" ~ "CaTSS",
        mech_name == "KP CARE 1" ~ "KP CARE 1",
        mech_name == "KP CARE 2" ~ "KP CARE 2",
        mech_name == "MSH - Prevention Organisation Systems AIDS Care and Treatment(MSH -ProACT)" ~ "ProACT",
        mech_name == "Integrated MARPs HIV Prevention Program (IMHIPP)" ~ "IMHIPP",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 1)" ~ "ICHSSA 1",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 2)" ~ "ICHSSA 2",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 3)" ~ "ICHSSA 3",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 4)" ~ "ICHSSA 4",
        # CDC
        mech_name == "Partnering Effectively to end AIDS through Results and Learning (PEARL)_2097" ~ "PEARL",
        mech_name == "Global Action towards HIV Epidemic Control in Subnational units in Nigeria (4GATES PROJECT)_2100" ~ "4GATES",
        mech_name == "ACTION to Control HIV Epidemic through Evidence (ACHIEVE)_2099" ~ "ACHIEVE",
        mech_name == "Improving Comprehensive AIDS Response Enhanced for Sustainability (iCARES)_2098" ~ "iCARES",
        TRUE ~ mech_name
      )
    )
}

#' @title Clean Partners
#'
clean_partners <- function(.data) {

  .data %>%
    mutate(
      primepartner = case_when(
        # USAID
        primepartner == "Family Health International" ~ "FHI 360",
        primepartner == "Chemonics International, Inc." ~ "Chemonics",
        primepartner == "JHPIEGO CORPORATION" ~ "JHPIEGO",
        primepartner == "SOCIETY FOR FAMILY HEALTH" ~ "SFH",
        primepartner == "HEARTLAND ALLIANCE LTD-GTE" ~ "HEARTLAND ALLIANCE",
        primepartner == "Heartland Alliance International, LLC" ~ "Heartland Alliance",
        primepartner == "CENTER FOR CLINICAL CARE AND CLINICAL RESEARCH LTD GTE" ~ "C4C3R",
        primepartner == "ASSOCIATION FOR REPRODUCTIVE AND FAMILY HEALTH" ~ "A4RFH",
        primepartner == "PRO-HEALTH INTERNATIONAL" ~ "ProHI",
        primepartner == "Management Sciences For Health, Inc." ~ "MHS",
        # CDC
        primepartner == "APIN PUBLIC HEALTH INITIATIVES LTD/GTE" ~ "APHI",
        primepartner == "INSTITUTE OF HUMAN VIROLOGY" ~ "IHVN",
        primepartner == "CATHOLIC CARITAS FOUNDATION OF NIGERIA" ~ "CCFN",
        primepartner == "CENTRE FOR INTEGRATED HEALTH PROGRAMS" ~ "CIHP",
        TRUE ~ primepartner
      )
    )
}


#' @title Create labels for partners
#'
partners_label <- function(.data) {
  .data %>%
    clean_mechs() %>%
    clean_partners() %>%
    mutate(partner = paste0(primepartner, " - ", mech_name))
}

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


#' @title Extract Indicators
#'
#'
extract_indicators <- function(df_file) {
  readxl::excel_sheets(df_file)
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

#' @title Identify MSD Consecutive Periods
#'
#'
identify_pds <- function(df_msd,
                         pd_end = "FY21Q3",
                         len = 6,
                         pds_type = "qtr") {

  # Current fiscal year
  curr_fy <- df_msd %>% identifypd(pd_type = "year")

  # All fiscal years (up to curr fy)
  curr_fys <- df_sites %>%
    distinct(fiscal_year) %>%
    arrange(desc(fiscal_year)) %>%
    pull()

  curr_fys <- curr_fys[curr_fys <= curr_fy]

  # Quarters
  qtrs <- 1:4 %>% paste0("Q", .)

  # All Periods
  curr_pds <- curr_fys %>%
    str_sub(3, 4) %>%
    paste0("FY", .) %>%
    map(function(.x) {
      .x %>%
        paste0(qtrs) %>%
        rev()
    }) %>% unlist()

  idx_end <- which(curr_pds == pd_end)
  idx_start = idx_end + (len - 1)

  pds <- curr_pds[idx_end:idx_start]

  if (pds_type == "year") {
    yrs <- pds %>%
      str_sub(1, 4) %>%
      str_replace("FY", "20") %>%
      unique() %>%
      as.integer()

    return(yrs)
  }

  return(pds)
}


#' @title Unpack HTS Modalities
#'
#'
unpack_modalities <- function(df_msd) {

  # Modalities
  df_mods <- df_msd %>%
    filter(indicator == "HTS_TST",
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>%
    mutate(indicator = paste0(indicator, "_",
                              str_to_upper(sitetype), "_",
                              str_to_upper(modality)),
           standardizeddisaggregate = NA_character_)

  # Index POS
  df_index <- df_msd %>%
    filter(indicator == "HTS_TST",
           otherdisaggregate == "Newly Identified",
           modality %in% c("Index", "IndexMod")) %>%
    matate(indicator = "HTS_TST_INDEX",
           otherdisaggregate = NA_character_,
           modality = NA_character_)

  # Index POS
  df_index_pos <- df_index %>%
    filter(statushiv == "Positive") %>%
    mutate(indicator = "HTS_TST_INDEX_POS",
           statushiv = NA_character_)

  # Bind all together
  df_msd %>%
    bind_rows(df_mods) %>%
    bind_rows(df_index) %>%
    bind_rows(df_index_pos)
}


#' @title Calculate Results Increases
#'
#'
calc_increases <- function(df_msd,
                           ind = "TX_CURR",
                           disag = "Age/Sex/HIVStatus") {

    print(paste0(ind, " => ", disag))

    df_ind <- df_msd %>%
      filter(indicator == ind & standardizeddisaggregate == disag)

    print(df_ind %>% distinct(indicator) %>% pull())

    # Take care of the Modalities
    if (str_detect(disag, "Modality")) {
      print("unpacking modalities .... ")

      df_ind <- df_ind %>%
        mutate(indicator = paste0(indicator, "_",
                                  str_to_upper(sitetype), "_",
                                  str_to_upper(str_replace_all(modality, " ", "_"))),
               standardizeddisaggregate = NA_character_) %>%
        bind_rows(df_ind, .)
    }

    # Summarise and calculate increase
    df_ind_targets <- df_ind %>%
      group_by(fiscal_year, psnuuid, psnu, orgunituid, sitename, indicator, ageasentered, sex) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      reshape_msd() %>%
      select(-period_type) %>%
      rename(age = ageasentered) %>%
      group_by(psnuuid, psnu, orgunituid, sitename, indicator, age, sex) %>%
      arrange(period) %>%
      mutate(
        increase = case_when(
          ind %in% gophr::snapshot_ind ~ abs(last(value) - first(value)), # TODO: how about missing values at the bigin/end?
          TRUE ~ sum(value, na.rm = TRUE)),
        avg_increase = increase / length(value) # TODO: Exclude NA / Zero values?
      ) %>%
      ungroup() %>%
      pivot_wider(names_from = period, values_from = value) %>%
      relocate(increase, avg_increase, .after = last_col())

    return(df_ind_targets)
}

#' @title Update Results Increases
#'
#'
update_increases <- function(df_results, df_roi) {

  df_results %>%
    rowwise() %>%
    mutate(assum_count = sum(c_across(starts_with("assum")) == "Y"),
           assum_count = ifelse(is.na(assum_count) | !is.finite(assum_count), 0, assum_count),
           ind_roi = first(df_roi$agency_roi[indicator == indicator]),
           add_increase = ind_roi / 100 / assum_count,
           add_increase = ifelse(is.na(add_increase) | !is.finite(add_increase), 0, add_increase),
           final_increase = (1 + add_increase) * avg_increase,
           total_increase = final_increase * 12) %>%
    ungroup() %>%
    mutate(global_increase = sum(total_increase),
           prop_target = total_increase / global_increase)
}

#' @title Add Assumptions
#'
#'
add_assumptions <- function(df_results, assums) {

  df_clean <- df_results %>%
    left_join(assums$hvol, by = c("sitename" = "orgunit")) %>%
    left_join(assums$hhiv, by = c("psnu" = "state")) %>%
    left_join(assums$gf, by = c("sitename" = "orgunit")) %>%
    left_join(assums$surge, by = c("psnu" = "state")) %>%
    left_join(assums$saturation, by = c("psnu" = "state")) %>%
    left_join(assums$funds, by = c("psnu" = "state"))

  return(df_clean)
}


## DATA ----

  # Orgs hierarchy ----
  fac_lvl <- get_ouorglevel(operatingunit = country,
                            org_type = "facility")

  df_orgs <- gisr::extract_locations(
      country = country,
      level = fac_lvl,
      add_geom = T) %>%
    extract_facilities()

  df_orgs <- df_orgs %>%
    select(operatingunit, countryname,
           orgunit = name, facility_uid = id,
           ends_with("tude"))

  df_orgs %>% glimpse()

  # Prioritization ----
  df_prio <- file_prev_cop_targets %>%
    extract_prioritization()

  df_prio %>% glimpse()
  df_prio %>% distinct(priority_snu_translation) %>% pull()

  # PLHIV ----
  df_plhiv <- file_prev_cop_targets %>%
    extract_plhiv()

  df_plhiv %>% glimpse()

  df_plhiv %>% distinct(psnu, total_psnu)


  # Assumptions ----
  dt_sheets <- file_cop22_targets2 %>%
    extract_indicators()

  dt_sheets

  df_assum <- file_cop22_targets2 %>%
    extract_assumptions()

  df_assum %>% glimpse()

  df_assum_level <- tibble(
      assum_id = 1:6,
      assum_name = c("High volume site",
                     "Site in high PLHIV burden state",
                     "Previously a Global Fund site",
                     "Site in a surge state",
                     "Site in a saturated state",
                     "More funds for state")
    ) %>%
    mutate(assum_level = case_when(
      str_detect(assum_name, "state") ~ "state",
      TRUE ~ "site"
    ))

  ## A1: High Volume Site
  df_hvol <- df_assum %>%
    distinct(state, orgunit, site_high_vol) %>%
    rename(assum_high_vol = site_high_vol)

  ## A2: High PLHIV Burden
  df_hhiv <- df_assum %>%
    distinct(state, site_high_plhiv) %>%
    rename(assum_high_plhiv = site_high_plhiv)

  ## A3: Global Fund
  df_gf <- df_assum %>%
    distinct(state, orgunit, site_prev_gf) %>%
    rename(assum_gf_site = site_prev_gf)

  ## A4: Surge State
  df_surge <- df_assum %>%
    distinct(state, site_surge_state) %>%
    rename(assum_surge_state = site_surge_state)

  ## A5: Program Saturation
  df_sat <- df_assum %>%
    distinct(state, site_sat_state) %>%
    rename(assum_sat_state = site_sat_state)

  ## A6: More Funding
  df_funds <- df_assum %>%
    distinct(state, site_more_fund) %>%
    rename(assum_more_fund = site_more_fund)


  ## A: Combine all assumptions
  dfs_assums <- list(
    "hvol"       = df_hvol %>% select(-state),
    "hhiv"       = df_hhiv,
    "gf"         = df_gf %>% select(-state),
    "surge"      = df_surge,
    "saturation" = df_sat,
    "funds"      = df_funds
  )


  # MSD - Site x IM ----
  df_raw <- file_msd_sites %>% read_msd()

  df_sites <- df_raw %>%
    filter(fundingagency == "USAID",
           primepartner != "TBD",
           sitetype != "Above Site") %>%
    select(-c(pre_rgnlztn_hq_mech_code, prime_partner_duns,
              award_number, source_name, cumulative)) %>%
    clean_agency() %>%
    clean_mechs() %>%
    clean_partners() %>%
    partners_label()

  # Identify list of reference indicators ----
  ref_inds <- dt_sheets %>%
    str_remove("(?<=\\().*") %>%
    str_remove("\\(") %>%
    str_trim() %>%
    str_remove("_D$|_N$") %>%
    unique()

  ref_inds <- ref_inds[!str_detect(ref_inds, "HIV")]

  ref_inds <- ref_inds %>%
    c(., "HTS_TST") %>%
    sort()

  ref_inds

  ref_ind_list <- c("CXCA_SCRN",  "GEND_GBV",
                    "HTS_TST",    "HTS_RECENT", "HTS_SELF",
                    "PMTCT_ART",  "PMTCT_STAT",
                    "PrEP_CURR",  "PrEP_NEW",
                    "TB_ART",     "TB_PREV",    "TB_STAT",
                    "TX_CURR",    "TX_NEW",     "TX_PVLS",    "TX_TB")

  df_sites <- df_sites %>%
    filter(indicator %in% ref_ind_list) %>%
    clean_indicator()

  # Identify history period ----
  curr_pd <- df_sites %>% identifypd()

  hist_pds <- df_sites %>% identify_pds(pd_end = curr_pd, len = 6)
  hist_yrs <- df_sites %>% identify_pds(pd_end = curr_pd, len = 6, pds_type = "year")

  df_sites <- df_sites %>%
    filter(fiscal_year %in% hist_yrs)

  # Indicator disaggs ----

  df_disaggs <- df_sites %>%
    distinct(indicator, standardizeddisaggregate) %>%
    filter(str_detect(standardizeddisaggregate, "Age|Sex")) %>%
    select(indicator, standardizeddisaggregate) %>%
    group_by(indicator) %>%
    arrange(standardizeddisaggregate, .by_group = T) %>%
    mutate(
      len = nchar(standardizeddisaggregate),
      disagg = case_when(
        indicator == "TX_CURR" ~ "Age/Sex/HIVStatus",
        indicator == "HTS_RECENT" ~ "Modality/Age/Sex/RTRI/HIVStatus",
        indicator == "HTS_TST" ~ "Modality/Age/Sex/Result",
        TRUE ~ first(standardizeddisaggregate))
    ) %>%
    ungroup() %>%
    distinct(indicator, disagg)

  df_disaggs2 <- df_disaggs %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = disagg, values_from = value)


  # Partner x indicator summary ----
  df_partners <- df_sites %>%
    filter(indicator %in% c("HTS_TST", "HTS_RECENT", "HTS_RECENT_D"),
           standardizeddisaggregate %in% c("Modality/Age/Sex/Result",
                                           "Modality/Age/Sex/RTRI/HIVStatus",
                                           "Age/Sex/HIVIndication")) %>%
    mutate(indicator = paste0(indicator, "_",
                              str_to_upper(sitetype), "_",
                              str_to_upper(str_replace_all(modality, " ", "_")))) %>%
    bind_rows(df_sites, .) %>%
    filter(fundingagency == "USAID",
           standardizeddisaggregate %in% c("Total Numerator",
                                           "Total Denominator",
                                           "Modality/Age/Sex/Result",
                                           "Modality/Age/Sex/RTRI/HIVStatus",
                                           "Age/Sex/HIVIndication")) %>%
    group_by(fiscal_year, indicator, partner) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    reshape_msd() %>%
    select(-period_type) %>%
    filter(period %in% hist_pds)

  # Partner RoI
  df_partners_roi <- df_partners %>%
    group_by(partner, indicator) %>%
    mutate(partner_data_hist = length(which(value > 0)) * 3,
           partner_ttl = sum(value, na.rm = TRUE),
           partner_avg = mean(value[value > 0], na.rm = T),
           partner_roi = partner_ttl / partner_avg) %>%
    ungroup() %>%
    filter(partner_ttl > 0) %>%
    group_by(indicator) %>%
    mutate(agency_roi = mean(partner_roi, na.rm = T)) %>%
    pivot_wider(names_from = period, values_from = value) %>%
    relocate(starts_with(c("partner_", "agency_")), .after = last_col())

  # Agency RoI
  df_agency_roi <- df_partners_roi %>%
    select(-starts_with("partner_")) %>%
    group_by(indicator) %>%
    summarise(across(starts_with("FY"), sum, na.rm = T)) %>%
    ungroup() %>%
    pivot_longer(cols = starts_with("FY"), names_to = "period", values_to = "value") %>%
    group_by(indicator) %>%
    mutate(agency_data_hist = length(which(value > 0)) * 3,
           agency_ttl = sum(value, na.rm = TRUE),
           agency_avg = mean(value[value > 0], na.rm = T),
           agency_roi = agency_ttl / agency_avg) %>%
    ungroup() %>%
    pivot_wider(names_from = period, values_from = value) %>%
    relocate(starts_with("agency_"), .after = last_col())

  # Site x Indicator x Sex x Age summary ----

  # Results Trend
  df_results <- df_disaggs %>%
    filter(str_detect(indicator, "TX_.*")) %>%
    pmap_dfr(~calc_increases(df_msd = df_sites, ind = .x, disag = .y))

  # Add assumptions to results
  df_results <- df_results %>%
    add_assumptions(assums = dfs_assums)

  # Calculate Increases
  df_results <- df_results %>%
    update_increases(df_roi = df_agency_roi)

  # Write outputs
  write_csv(df_results,
            file = "./Dataout/Simulation - FalicilityTargetProjecton.csv",
            na = "")






















