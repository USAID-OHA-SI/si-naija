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
  library(gisr)
  library(janitor)
  library(gt)


# GLOBAL ----

# DIR - Global ----

  dir_merdata <- glamr::si_path("path_msd")
  dir_datapack <- "../../PEPFAR/COUNTRIES/Nigeria/DataPack"
  dir_dp22 <- paste0(dir_datapack, "/DP-Nigeria-Version")
  dir_assums <- paste0(dir_dp22, "/assumptions")
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

  file_assums <- return_latest(
    folderpath = dir_assums,
    pattern = "^FY22 - Targets Param.*.xlsx$")

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
  curr_fys <- df_msd %>%
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

#' @title Update Modalities
#'
#'
update_modalities <- function(df_msd,
                              sep = "-",
                              full = FALSE) {

  df_msd <- df_msd %>%
    mutate(
      indicator = case_when(
        str_detect(modality, "Mod$") ~ paste0(indicator, "_COM"),
        TRUE ~ paste0(indicator, "_FAC")
      ),
      modality = case_when(
        str_detect(modality, "Mod$") ~ str_remove(modality, "Mod$"),
        str_detect(modality, "^Other*") ~ str_replace(modality, "^Other*", "Other "),
        modality == "Inpat" ~ "Inpatient",
        modality == "TBClinic" ~ "TB Clinic",
        TRUE ~ modality
      ),
      modality = str_replace_all(modality, " ", sep)
    )

  if (full == TRUE) {
    df_msd <- df_msd %>%
      mutate(indicator = paste0(indicator, "_", modality))
  }

  return(df_msd)
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
calc_roi <- function(df_msd) {

}

#' @title Add Assumptions
#'
#'
add_assumptions <- function(df_results, assums) {

  df_results %>%
    left_join(assums$hvol,
              by = c("psnu", "orgunituid", "sitename")) %>%
    left_join(assums$gf,
              by = c("psnu" = "state", "sitename" = "orgunit")) %>%
    left_join(assums$states, by = c("psnu" = "state"))
}


#' @title Calculate Results Increases
#'
#'
calc_increases <- function(df_msd, periods = NULL) {

  # Summarize and calculate increase
  df_msd <- df_msd %>%
    group_by(fiscal_year, psnuuid, psnu, orgunituid, sitename, partner, indicator, ageasentered, sex) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    reshape_msd() %>%
    select(-period_type) %>%
    rename(age = ageasentered)

  # Filter datasets
  if (!is.null(periods)) {
    df_msd <- df_msd %>%
      filter(period %in% periods)
  }

  # Get latest
  last_fy <- df_msd %>%
    distinct(period) %>%
    arrange(period) %>%
    pull() %>%
    last() %>%
    str_sub(1, 4)

  # Calculate increases
  df_msd %>%
    group_by(psnuuid, psnu, orgunituid, sitename, partner, indicator, age, sex) %>%
    arrange(period) %>%
    mutate(
      record = case_when(is.na(value) | value > 0 ~ 1, TRUE ~ 0),
      value_hist = length(value[record == 1]),
      value_hist = case_when(
        indicator %in% c("CXCA_SCRN", "GEND_GBV") ~ value_hist * 6,
        TRUE ~ value_hist * 3
      ),
      value_start = first(value[record == 1]),
      value_end = case_when(
        indicator %in% gophr::snapshot_ind ~ last(value[record == 1]), # TODO: how about missing values at the bigin/end?
        TRUE ~ sum(value, na.rm = TRUE)
      ),
      value_fy = case_when(
        indicator %in% gophr::snapshot_ind ~ last(value[record == 1]),
        TRUE ~ sum(value[str_detect(period, last_fy)], na.rm = TRUE)
      ),
      h_increase = value_end - value_start,
      m_increase = h_increase / value_hist # TODO: Exclude NA / Zero values?
    ) %>%
    ungroup() %>%
    select(-record) %>%
    pivot_wider(id_cols = -c(period, value), names_from = period, values_from = value) %>%
    relocate(c(starts_with("value_"), ends_with("increase")), .after = last_col())
}

#' @title Update Results Increases
#'
#'
update_increases <- function(df_results,
                             df_roi,
                             df_targets = NULL,
                             indicator = "TX_CURR") {

  prev_target = 0

  if (!is.null(df_targets)) {
    prev_target <- df_targets %>%
    filter(indicator == indicator) %>%
    pull(value)
  }

  # # of Assumptions
  len <- df_results %>%
    select(starts_with("assum")) %>%
    colnames() %>%
    length()

  df_results %>%
    rowwise() %>%
    mutate(
      len_assum = len,
      assum_count = sum(c_across(starts_with("assum")) == "Y"),
      assum_count = ifelse(is.na(assum_count) | !is.finite(assum_count), 0, assum_count),
      ind_roi = first(df_roi$agency_roi[indicator == indicator]),
      add_increase = ind_roi / len_assum * assum_count,
      add_increase = ifelse(is.na(add_increase) | !is.finite(add_increase), 0, add_increase),
      total_m_increase = (1 + add_increase) * m_increase,
      total_a_increase = round(total_m_increase * 12, 0),
      total_result = value_fy,
      est_target = total_result + total_a_increase
    ) %>%
    ungroup() %>%
    group_by(indicator) %>%
    mutate(
      total_est_target = sum(est_target, na.rm = TRUE),
      prop_est_target = est_target / total_est_target,
      new_target = case_when(
        prev_target == 0 | is.null(prev_target) | is.na(prev_target) ~ est_target,
        TRUE ~ prop_est_target * prev_target
      )) %>%
    ungroup()
}

#' @title Estimate Targets
#'
#'
estimate_targets <- function(df_msd,
                             roi,
                             lst_assums,
                             targets = NULL,
                             periods = NULL,
                             indicator = "TX_CURR") {

  df_targets <- df_msd %>%
    filter(str_detect(indicator, {{indicator}})) %>%
    calc_increases(periods = periods)

  # Add assumptions to results
  df_targets <- df_targets %>%
    add_assumptions(assums = lst_assums) %>%
    mutate(across(starts_with("assum"), ~ if_else(is.na(.), "N", .)))

  # Calculate Increases
  df_targets <- df_targets %>%
    update_increases(df_roi = roi,
                     df_targets = targets)

  return(df_targets)
}


#' @title Summarize Targets Estimations
#'
#'
summarise_targets <- function(df_targets, ...) {
  df_targets %>%
    group_by(...) %>%
    summarise(across(c(starts_with("value"),
                       ends_with("increase"),
                       total_result,
                       ends_with("target")), sum, na.rm = T),
              .groups = "drop") %>%
    ungroup() %>%
    select(-c(value_hist, add_increase, new_target)) %>%
    mutate(total_est_target = sum(est_target, na.rm = TRUE),
           prop_est_target = round(est_target / total_est_target * 100, 2))
}

# DATA ----

  # MSD - PSNU x IM ----
  df_psnu <- file_msd_psnu %>% read_msd()

  df_psnu %>% glimpse()

  df_psnu_targets <- df_psnu %>%
    filter(fiscal_year == 2021,
           fundingagency != "Dedup",
           primepartner != "TBD",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, fundingagency, operatingunit,
             psnu, psnuuid, mech_code, mech_name, primepartner,
             indicator) %>%
    summarise(across(targets, sum, na.rm = TRUE)) %>%
    ungroup()


  # MSD - Site x IM ----
  df_raw <- file_msd_sites %>% read_msd()

  # Identify history period ----
  curr_pd <- df_raw %>% identifypd()
  curr_fy <- df_raw %>% identifypd(pd_type = "year")

  len_pds = 6

  hist_pds <- df_raw %>% identify_pds(pd_end = curr_pd, len = len_pds)
  hist_yrs <- df_raw %>% identify_pds(pd_end = curr_pd, len = len_pds, pds_type = "year")

  # Identify list of reference indicators ----
  ref_ind_list <- c("CXCA_SCRN",  "GEND_GBV",
                    "HTS_RECENT", "HTS_SELF",    "HTS_TST",
                    "PMTCT_ART",  "PMTCT_STAT",
                    "PrEP_CURR",  "PrEP_NEW",
                    "TB_ART",     "TB_PREV",    "TB_PREV_D",
                    "TB_STAT",    "TB_STAT_D",
                    "TX_CURR",    "TX_NEW",
                    "TX_PVLS",    "TX_PVLS_D",
                    "TX_TB",      "TX_TB_D")

  # Site Results Data ----
  df_sites <- df_raw %>%
    filter(fundingagency == agency,
           fiscal_year %in% hist_yrs,
           primepartner != "TBD") %>%
    select(-c(pre_rgnlztn_hq_mech_code, prime_partner_duns,
              award_number, source_name, cumulative)) %>%
    clean_agency() %>%
    clean_mechs() %>%
    clean_partners() %>%
    partners_label() %>%
    clean_indicator() %>%
    filter(indicator %in% ref_ind_list)

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
        indicator == "HTS_RECENT_D" ~ "Age/Sex/HIVIndication",
        indicator == "HTS_TST" ~ "Modality/Age/Sex/Result",
        TRUE ~ first(standardizeddisaggregate))
    ) %>%
    ungroup() %>%
    distinct(indicator, disagg)

  # Extract Indicators and corresponding disagg for age/sex
  df_sites <- df_disaggs %>%
    rename(ind = indicator) %>%
    pmap_dfr(function(ind, disagg) {
      df_sites %>%
        filter(indicator == ind,
               standardizeddisaggregate == disagg)
    })

  # Verify
  df_sites %>%
    distinct(indicator, standardizeddisaggregate) %>%
    mutate(msd = TRUE) %>%
    full_join(df_disaggs,
              by = c("indicator",
                     "standardizeddisaggregate" = "disagg"))

  # HTS TST or RECENT ----

  # Checkk HTS Modalities
  df_sites %>%
    filter(indicator %in% c("HTS_TST", "HTS_RECENT")) %>%
    distinct(indicator, modality, sitetype) %>%
    arrange(sitetype, indicator) %>%
    prinf()

  # Unpack modalities
  df_sites <- df_sites %>%
    filter(indicator %in% c("HTS_TST", "HTS_RECENT")) %>%
    update_modalities(full = TRUE) %>%
    bind_rows(df_sites, .) %>%
    filter(!indicator %in% c("HTS_TST", "HTS_RECENT"))

  df_sites %>% distinct(indicator) %>% pull()

  # Assumptions ----
  df_assum_level <- tibble(
      assum_id = 1,
      assum_name = c("High volume site",
                     "Site in high PLHIV burden state",
                     "Previously a Global Fund site",
                     "Site in a surge state",
                     "Site in a saturated state",
                     "More funds for state")
    ) %>%
    mutate(
      assum_level = case_when(
        str_detect(assum_name, "state") ~ "state",
        TRUE ~ "site"
      ),
      assum_id = row_number()
    )

  ## Parameters
  df_assum_params <- file_assums %>%
    read_excel(sheet = "Parameters")

  df_assum_params %>%
    select(-Note) %>%
    gt::gt()

  ## A: Read Assumptions

  ## A1: High Volume
  df_hvol <- NULL

  ## A3: High Volume
  df_gf <- file_assums %>%
    read_excel(sheet = "Site Level Assumptions") %>%
    select(state, orgunit, assum_gf_site)

  df_gf %>% head(8) %>% gt::gt()

  ## A2,4-6: High Volume
  df_assum_states <- file_assums %>%
    read_excel(sheet = "State Level Assumptions")

  df_assum_states %>% gt::gt()

  ## A: Combine all assumptions
  dfs_assums <- list(
    "hvol"       = tibble(),
    "gf"         = df_gf,
    "states"     = df_assum_states
  )

  # Update Assumption #1
  df_hvol <- df_sites %>%
    filter(indicator == "TX_CURR") %>%
    select(fiscal_year, psnu, orgunituid, sitename, indicator, starts_with("qtr")) %>%
    reshape_msd() %>%
    filter(period == first(hist_pds)) %>%
    group_by(psnu, orgunituid, sitename) %>%
    summarise(volume = sum(value, na.rm = T), .groups = "drop") %>%
    mutate(assum_high_vol = case_when(
      volume >= 1000 ~ "Y",
      TRUE ~ "N"
    ))

  df_hvol %>%
    filter(str_detect(psnu, "_Mil", negate = T)) %>%
    select(-orgunituid) %>%
    arrange(desc(volume)) %>%
    head(10) %>%
    gt::gt()

  dfs_assums$hvol <- df_hvol %>% select(-volume)


  # Partner x indicator summary ----

  df_partners <- df_sites %>%
    group_by(fiscal_year, indicator, partner) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    reshape_msd() %>%
    select(-period_type) %>%
    filter(period %in% hist_pds)

  # Partner RoI
  df_partners_roi <- df_partners %>%
    group_by(partner, indicator) %>%
    mutate(
      partner_rcd = case_when(value > 0 ~ 1, TRUE ~ 0),
      partner_hist = length(value[partner_rcd == 1]),
      partner_hist = case_when(
        indicator %in% c("CXCA_SCRN", "GEND_GBV") ~ partner_hist * 6,
        TRUE ~ partner_hist * 3
      ),
      partner_start = first(value[partner_rcd == 1]),
      partner_end = case_when(
        indicator %in% gophr::snapshot_ind ~ last(value[partner_rcd == 1]),
        TRUE ~ sum(value, na.rm = TRUE)
      ),
      partner_h_inc = partner_end - partner_start,
      partner_m_inc = round(partner_h_inc / partner_hist, 0),
      partner_roi = partner_h_inc / partner_hist / partner_start
    ) %>%
    ungroup() %>%
    select(-partner_rcd) %>%
    group_by(indicator) %>%
    mutate(agency_roi = mean(partner_roi, na.rm = T)) %>%
    ungroup() %>%
    pivot_wider(id_cols = -c(period, value),
                names_from = period, values_from = value) %>%
    relocate(starts_with(c("partner_", "agency_")), .after = last_col())

  # Agency RoI
  df_agency_roi <- df_partners %>%
    group_by(period, indicator) %>%
    summarise(across(value, sum, na.rm = T), .groups = "drop") %>%
    ungroup() %>%
    group_by(indicator) %>%
    mutate(
      agency_rcd = case_when(value > 0 ~ 1, TRUE ~ 0),
      agency_hist = length(value[agency_rcd == 1]),
      agency_hist = case_when(
        indicator %in% c("CXCA_SCRN", "GEND_GBV") ~ agency_hist * 6,
        TRUE ~ agency_hist * 3
      ),
      agency_start = first(value[agency_rcd == 1]),
      agency_end = case_when(
        indicator %in% gophr::snapshot_ind ~ last(value[agency_rcd == 1]),
        TRUE ~ sum(value, na.rm = TRUE)
      ),
      agency_h_inc = agency_end - agency_start,
      agency_m_inc = round(agency_h_inc / agency_hist, 0),
      agency_roi = agency_h_inc / agency_hist / agency_start
    ) %>%
    ungroup() %>%
    pivot_wider(id_cols = -c(period, value),
                names_from = period, values_from = value) %>%
    relocate(starts_with("agency_"), .after = last_col())

  # Site x Indicator x Sex x Age summary ----

# Targets ----
  df_est <- estimate_targets(df_msd = df_sites,
                             roi = df_agency_roi,
                             lst_assums = dfs_assums,
                             targets = NULL,
                             periods = hist_pds,
                             indicator = "TX_CURR")

  df_est %>% glimpse()

  df_est %>% head(10)

  # Summary Tables ----
  df_est %>%
    summarise_targets(psnu, partner, indicator) %>%
    filter(str_detect(partner, "FHI 360")) %>%
    arrange(desc(h_increase)) %>%
    gt::gt()

  df_est %>%
    summarise_targets(psnu, indicator) %>%
    arrange(desc(h_increase)) %>%
    gt::gt()

  df_est %>%
    summarise_targets(partner, indicator) %>%
    arrange(desc(h_increase)) %>%
    gt::gt()

  # Validation Tables ----
  df_est %>%
    filter(indicator == "TX_CURR") %>%
    select(psnu, sitename, indicator, age, sex, starts_with("FY")) %>%
    head(5) %>%
    gt::gt()

  df_est %>%
    select(psnu, sitename, indicator, age, sex,
           starts_with("FY"), value_hist, value_start, value_end,
           h_increase, m_increase) %>%
    head(5) %>%
    gt::gt()

  df_est %>%
    filter(indicator == "TX_CURR") %>%
    select(psnu, sitename, indicator, age, sex,
           starts_with("FY"), value_hist, value_start, value_end,
           h_increase, m_increase) %>%
    head(5) %>%
    gt::gt()

  df_est %>%
    filter(indicator == "TX_CURR") %>%
    select(psnu, sitename, indicator, age, sex,
           value_hist, value_start, value_end,
           h_increase, m_increase, starts_with("assum"), -assum_count) %>%
    head(5) %>%
    gt::gt()

  df_est %>%
    select(indicator, age, sex, h_increase, m_increase,
           len_assum, assum_count, ind_roi, ends_with("increase")) %>%
    #select(-global_increase) %>%
    head(10) %>%
    gt::gt()

  df_est %>%
    filter(indicator == "TX_CURR") %>%
    select(indicator, age, sex,
           ends_with("increase"),
           total_result,
           ends_with("target")) %>%
    head(10) %>%
    gt::gt()


  # Write outputs ----
  write_csv(df_est,
            file = "./Dataout/Simulation - FalicilityTargetProjecton.csv",
            na = "")






















