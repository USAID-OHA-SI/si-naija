##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Utility functions
##  LICENCE: MIT
##  DATE:    2021-07-12
##  UPDATED: 2021-10-07
##

## Libraries ----
  library(tidyverse)
  library(gophr)
  library(glamr)
  library(glitr)
  library(scales)
  library(extrafont)

## FUNCTIONS ----

#' @title Open Windows folders / files
#'
#'
open_path <- function(path) {
  utils::browseURL(path)
}

#' @title Link list items together as a string
#'
connect_list <- function(items, connector = "-") {
  items[!is.na(items)] %>%
    paste(collapse = connector)
}

#' @title Datim Login
#' @description This is wrap around datimutil::loginToDATIM()
#'
datim_session <- function() {

  #secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
  #datimutils::loginToDATIM()

  # OR
  datimutils::loginToDATIM(username = datim_user(),
                           password = datim_pwd(),
                           base_url = "https://www.datim.org/")
}


#' @title Extract MSD Indicators
#'
#'
msd_indicators <- function(.df_msd, fy) {
  .df_msd %>%
    filter(fiscal_year == fy) %>%
    select(indicator, numeratordenom, indicatortype,
           disaggregate, standardizeddisaggregate,
           #ageasentered, sex,
           statushiv, statustb, statuscx,
           statustx = hiv_treatment_status,
           otherdisaggregate, otherdisaggregate_sub, modality, source_name) %>%
    distinct() %>%
    #arrange(indicator, disaggregate, ageasentered, sex) %>%
    mutate(fiscal_year = fy) %>%
    relocate(fiscal_year, .before = 1) %>%
    arrange(indicator, disaggregate) %>%
    distinct()
}

#' @title Extract MSD Mechanisms
#'
#'
msd_mechanisms <- function(.df_msd, fy, ...) {
  .df_msd %>%
    filter(fiscal_year %in% fy, fundingagency != "Dedup") %>%
    distinct(fiscal_year, fundingagency, mech_code, mech_name, primepartner, ...) %>%
    select(fiscal_year, fundingagency, mech_code, mech_name, primepartner, ...) %>%
    arrange(fundingagency, mech_code) %>%
    update_mechs() %>%
    clean_mechs() %>%
    filter(str_detect(mech_name, "Placeholder", negate = TRUE)) %>%
    clean_partners() %>%
    #mutate(fiscal_year = fy) %>%
    relocate(fiscal_year, .before = 1)
}


#' @title Update IM Transitions
#'
#'
msd_im_transition <- function(.df_msd, .df_mechs, ...) {

  args <- unlist(list(...))

  .df_msd %>%
  left_join(.df_mechs, c("mech_code", args)) %>%
    mutate(mech_code = case_when(
      mech_code_new != mech_code ~ mech_code_new,
      mech_code == "17747" ~ "70255",
      TRUE ~ mech_code
    )) %>%
    select(-mech_code_new)
}


#' @title Map MSD to DP Indicators
#'
msd_to_dp_indicators <- function(.df_msd, .df_de_coc) {
  .df_msd %>%
    left_join(.df_de_coc,
              by = c("indicator", "numeratordenom",
                     "indicatortype" = "indicator_type",
                     "disaggregate", "ageasentered", "sex")) %>%
    filter(!is.na(indicator_code)) %>%
    mutate(
      keypop = case_when(
        str_detect(indicator_code, "KP_.*.T|.*.KP.T") ~ otherdisaggregate,
        TRUE ~ NA_character_
      ),
      ageasentered = case_when(
        indicator_code == "PMTCT_EID.N.2.T" ~ NA_character_,
        indicator_code %in% c("GEND_GBV.PE.T", "GEND_GBV.S.T") ~ NA_character_,
        TRUE ~ ageasentered
      ),
      sex = case_when(
        indicator_code %in% c("GEND_GBV.PE.T", "GEND_GBV.S.T") ~ NA_character_,
        TRUE ~ sex
      ))
}

#' @title Read DP
#'
#'
dp_read <- function(filename,
                    sheet = "PSNUxIM",
                    header = 14,
                    repair = "unique") {
  readxl::read_excel(
    path = filename,
    sheet = sheet,
    range = readxl::cell_limits(c(header, 1), c(NA, NA)),
    col_types = "text",
    .name_repair = repair
  )
}

#' @title DP Write data to file
#'
#'
dp_write <- function(.df_out, file_in, file_out) {

  # Update files
  wb_dp <- loadWorkbook(file = file_in)

  #file_out <- str_replace(file_in, ".xlsx", paste0(curr_date(), ".xlsx"))

  writeData(
    wb = wb_dp,
    sheet = "PSNUxIM",
    x = .df_out,
    startCol = "I",
    startRow = 15,
    colNames = FALSE
  )

  saveWorkbook(
    wb = wb_dp,
    file = file_out,
    overwrite = TRUE
  )
}

#' @title DP Clean Raw data
#'
#'
dp_clean <- function(.df_raw) {

  .df_raw <- .df_raw %>%
    select(-starts_with("...")) %>%
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{3}$", "_value")) %>%
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{1,2}$", "_share"))

  .df_raw <- .df_raw %>%
    mutate(snu = str_extract(PSNU, ".*(?=\\[#)"),
           snu = str_trim(snu, side = "both")) %>%
    relocate(snu, .before = 1) %>%
    clean_names()

  return(.df_raw)
}

#' @title DP Reshape
#'
#'
dp_reshape <- function(.df_clean) {

  .df_clean <- df_dp_psnu_im %>%
    select(PSNU:Rollup, ends_with("dsd_share")) %>%
    pivot_longer(cols = ends_with("share"),
                 names_to = "mech_code",
                 values_to = "shares")

  df_dp_psnu_im <- df_dp_psnu_im %>%
    mutate(mech_code = str_remove(mech_code, "_dsd_share"))
}

#' @title Map DP Indicators
#'
#'
dp_indicators <- function(fy = 2023, support = "DSD") {

  datapackr::cop22_map_DataPack_DATIM_DEs_COCs %>%
    janitor::clean_names() %>%
    filter(fy == fy,
           dataset == "mer",
           support_type == support,
           targets_results == "targets") %>%
    select(indicator_code, indicator = technical_area,
           numeratordenom = numerator_denominator,
           indicator_type = support_type, disaggregate = disagg_type,
           ageasentered = valid_ages_name, sex = valid_sexes_name,
           keypop = valid_kps_name, statushiv = resultstatus,
           modality = hts_modality) %>%
    distinct() %>%
    mutate(
      numeratordenom = case_when(
        numeratordenom == "Numerator" ~ "N",
        numeratordenom == "Denominator" ~ "D",
        TRUE ~ NA_character_
      ),
      disaggregate = case_when(
        str_detect(indicator_code, ".*.SNS.T$|.*.SNS.*T$") ~ "SNS/Age/Sex/Result",
        str_detect(indicator_code, ".*.SNSCom.T$|.*.SNSCom.*T$") ~ "SNSMod/Age/Sex/Result",
        indicator_code == "OVC_HIVSTAT.T" ~ "Total Numerator",
        indicator_code == "CXCA_SCRN.T" ~ "Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
        indicator_code == "GEND_GBV.PE.T" ~ "Age/Sex/ViolenceType",
        indicator_code == "GEND_GBV.S.T" ~ "Age/Sex/ViolenceType",
        indicator_code == "HTS_SELF.T" ~ "Age/Sex/HIVSelfTest",
        indicator_code == "HTS_SELF.KP.T" ~ "KeyPop/HIVSelfTest",
        str_detect(indicator_code, "HTS_.*.KP.T") ~ "KeyPop/HIVStatus",
        str_detect(indicator_code, "HTS_INDEX_COM*.T") ~ "IndexMod/Age/Sex/Result",
        str_detect(indicator_code, "HTS_INDEX_FAC*.T") ~ "Index/Age/Sex/Result",
        #HTS_TST & RECENT
        str_detect(indicator_code, "TX_PVLS.*.KP.T") ~ "KeyPop/Indication/HIVStatus",
        str_detect(indicator_code, "TX_TB.D.*.T") ~ "Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus",
        str_detect(indicator_code, "PrEP_NEW.KP.T") ~ "KeyPopAbr",
        str_detect(indicator_code, "TB_ART.*.T") ~ "Age/Sex/NewExistingArt/HIVStatus",
        str_detect(indicator_code, "TB_ART.*.T|TB_PREV.*.T") ~ "Age/Sex/NewExistingArt/HIVStatus",
        str_detect(indicator_code, "TB_STAT.D.T") ~ "Age/Sex",
        str_detect(indicator_code, "TB_STAT.N.T") ~ "Age/Sex/NewExistingArt/HIVStatus",
        TRUE ~ disaggregate
      ),
      modality = case_when(
        str_detect(indicator_code, ".*.SNS.T$|.*.SNS.*T$") ~ "SNS",
        str_detect(indicator_code, ".*.SNSCom.T$|.*.SNSCom.*T$") ~ "SNSMod",
        str_detect(modality, "^Facility.*") ~ str_remove(modality, "^Facility - "),
        str_detect(modality, "^Community*") ~ paste0(str_remove(modality, "^Community - "), "Mod"),
        TRUE ~ modality
      ),
      modality = str_remove(modality, " Services")
    )
}


#' @title Map DP Indicators
#'
#'
dp_map_indicators <- function(.dp_raw) {
  .dp_raw %>%
    select(indicator_code, Age, Sex, KeyPop) %>%
    distinct() %>%
    rename_with(.fn = tolower) %>%
    mutate(indicator_target = indicator_code) %>%
    clean_indicators() %>%
    mutate(
      modality = case_when(
        str_detect(indicator_target, "HTS_RECENT.*.T") ~
          str_extract(indicator_target, "(?<=HTS_RECENT\\.).*(?=\\.T)"),
        TRUE ~ modality
      ),
      modality = str_replace(modality, "Com", "Mod"),
      modality = str_replace(modality, "Fac", ""),
      modality = na_if(modality, "KP"),
      modality = recode(modality,
                        "EW" = "Emergency Ward",
                        "Maln" = "Malnutrition",
                        "Other" = "OtherPITC",
                        "Peds" = "Pediatric",
                        "PMTCT_STAT" = "PMTCT ANC",
                        "PostANC1" = "Post ANC1",
                        "STI" = "STI Clinic",
                        "TB" = "TBClinic")
    ) %>%
    select(indicator_target, indicator, numeratordenom,
           standardizeddisaggregate, statushiv,
           modality, otherdisaggregate) %>%
    distinct() %>%
    mutate(
      indicator = case_when(
        indicator == "PREP_NEW" ~ "PrEP_NEW",
        indicator == "PREP_CURR" ~ "PrEP_CURR",
        indicator == "PREP_CT" ~ "PrEP_CT",
        TRUE ~ indicator
      ),
      modality = case_when(
        str_detect(indicator_target, "HTS_INDEX_COM.*.T") ~ "IndexMod",
        str_detect(indicator_target, "HTS_INDEX_FAC.*.T") ~ "Index",
        TRUE ~ modality
      ),
      otherdisaggregate = case_when(
        #indicator_target == "CXCA_SCRN.T" ~ "Cervical Cancer Screened - First Time, Cervical Cancer - Positive",
        indicator_target == "GEND_GBV.PE.T" ~ "Physical and/or Emotional Violence",
        indicator_target == "GEND_GBV.S.T" ~ "Sexual Violence Post-Rape Care",
        indicator_target == "OVC_SERV.Active.T" ~ "Active",
        indicator_target == "OVC_SERV.Grad.T" ~ "Graduated",
        indicator_target == "OVC_SERV.Prev.T" ~ NA_character_,
        str_detect(indicator_target, "HTS_INDEX_.*[.]New[.].*.T") ~ "Newly Identified",
        str_detect(indicator_target, "HTS_TST[.].*[.]T") ~ "Newly Identified",
        str_detect(indicator_target, "PMTCT_STAT.N.New.*[.]T") ~ "Newly Identified",
        str_detect(indicator_target, "PMTCT_STAT.N.Known.*[.]T") ~ "Known at Entry",
        str_detect(indicator_target, "TX_TB.D.Already.*.T") ~ paste0("TB Screen - ", statushiv, ", Life-Long ART, Already"),
        str_detect(indicator_target, "TX_TB.D.New.*.T") ~ paste0("TB Screen - ", statushiv, ", Life-Long ART, New"),
        str_detect(indicator_target, "TB_STAT.N.New.*[.]T") ~ "Newly Identified",
        str_detect(indicator_target, "TB_STAT.N.Known.*[.]T") ~ "Known at Entry",
        TRUE ~ otherdisaggregate
      ),
      statushiv = case_when(
        indicator_target == "OVC_HIVSTAT.T" ~ "Unknown",
        str_detect(indicator_target, "CXCA_SCRN|TX_PVLS|TX_NEW|TX_CURR|TX_TB|TB_ART|TB_PREV|HTS_RECENT|PMTCT_ART") ~ "Positive",
        TRUE ~ statushiv
      ),
      standardizeddisaggregate = case_when(
        indicator_target == "CXCA_SCRN.T" ~ "Age/Sex/HIVStatus/ScreenResult/ScreenVisitType",
        indicator_target == "TB_STAT.D.T" ~ "Age/Sex",
        indicator_target == "HTS_SELF.T" ~ "Age/Sex/HIVSelfTest",
        indicator_target == "HTS_SELF.KP.T" ~ "KeyPop/HIVSelfTest",
        str_detect(indicator_target, "HTS_INDEX_.*.T") ~ "4:Age/Sex/Result",
        indicator_target == "HTS_RECENT.KP.T" ~ "KeyPop/RTRI/HIVStatus",
        str_detect(indicator_target, "HTS_RECENT.") ~ "Modality/Age/Sex/RTRI/HIVStatus",
        str_detect(indicator_target, "GEND_GBV.*.T") ~ "Age/Sex/ViolenceType",
        str_detect(indicator_target, "HTS_TST.*.KP.T") ~ "KeyPop/HIVStatus",
        str_detect(indicator_target, "TX_PVLS.*.KP.T") ~ "KeyPop/Indication/HIVStatus",
        str_detect(indicator_target, "TX_TB.D.*.T") ~ "Age Aggregated/Sex/TBScreen/NewExistingART/HIVStatus",
        str_detect(indicator_target, "PrEP_NEW.KP.T") ~ "KeyPopAbr",
        str_detect(indicator_target, "TB_ART.*.T") ~ "Age/Sex/NewExistingArt/HIVStatus",
        str_detect(indicator_target, "TB_ART.*.T|TB_PREV.*.T") ~ "Age/Sex/NewExistingArt/HIVStatus",
        str_detect(indicator_target, "TB_STAT.D.T") ~ "Age/Sex",
        str_detect(indicator_target, "TB_STAT.N.T") ~ "Age/Sex/NewExistingArt/HIVStatus",
        TRUE ~ standardizeddisaggregate
      )
    ) %>%
    filter(
      indicator != "HTS_TST_POS",
      !(indicator == "HTS_TST" & str_detect(indicator_target, "HTS_TST", negate = T))
    )
}

#' @title Extract DP Target Values
#'
#'
dp_extract_values <- function(.df_cop) {
  .df_cop %>%
    select(psnu:rollup, -ends_with("share")) %>%
    rename_with(.cols = ends_with("value"),
                .fn = ~str_remove(., "_value")) %>%
  pivot_longer(cols = ends_with("dsd"),
               names_to = "attribute",
               values_to = "value") %>%
    mutate(attribute = str_extract(attribute, "\\d+"))
}

#' @title Extract DP Target Shares
#'
#'
dp_extract_shares <- function(.df_cop) {
  .df_cop %>%
    select(psnu:rollup, ends_with("share")) %>%
    rename_with(.cols = ends_with("share"),
                .fn = ~str_remove(., "_share")) %>%
    pivot_longer(cols = ends_with("dsd"),
                 names_to = "attribute",
                 values_to = "share") %>%
    mutate(attribute = str_extract(attribute, "\\d+"))
}

#' @title Extract DP Target Shares/Values
#'
#'
dp_extract_data <- function(.df_cop) {
  .df_cop %>%
    select(PSNU:Rollup, ends_with(c("share", "value"))) %>%
    pivot_longer(cols = ends_with(c("share", "value")),
                 names_to = "attribute",
                 values_to = "aalue") %>%
    separate(attribute, into = c("attribute", "support_type", "value_type"))
}

#' @title Get Orgunit Prioritization
#'
#'
orgunit_prioritization <- function(.data,
                                   org_type = "snu") {
  # Org Prioritization column
  col_prio <- case_when(
      str_detect(org_type, "1$") ~ str_remove(org_type, "1$"),
      TRUE ~ org_type
    ) %>%
    paste0("prioritization")

  col_uid <- paste0(org_type, "uid")

  cols <- c(col_uid, org_type, col_prio)

  # Check if cols exist
  if(!any(cols %in% names(.data))) {
    print(paste(cols, collapse = ", "))
    stop("INVALID INPUT - At least one of these columns does not exist in the data")
  }

  # Select applicable org columns and extract distinct rows
  .data %>%
    select(all_of(cols)) %>%
    distinct() %>%
    filter(across(.cols = all_of(org_type),
                  .fns = ~ (!is.na(.x) & !str_detect(.x, "Data reported above"))))
}

#' @title Summarize Indicators Values
#'
#' @param df
#' @param inds
#' @param disags
#' @param sum_vars
#' @param ...
#'
sum_indicator <- function(df,
                          inds = 'TX_CURR',
                          disags = 'Total Numerator',
                          sum_vars = 'cumulative',
                          ...) {

  # df %>%
  #   filter(indicator %in% inds,
  #          standardizeddisaggregate %in% disags) %>%
  #   group_by(...) %>%
  #   summarise(across(all_of(values), sum, na.rm = TRUE)) %>%
  #   ungroup()

  df %>%
    filter(indicator %in% inds,
           standardizeddisaggregate %in% disags) %>%
    sum_group(sum_vars = sum_vars, keep_group = FALSE, ...)
}

#' @title Sum Group Values
#'
#' @param df
#' @param sum_vars
#' @param ...
#'
sum_group <- function(df,
                      sum_vars = 'value',
                      keep_group = FALSE,
                      ...) {

  # Mutate
  if (keep_group == TRUE) {

    sum_rst <- paste0(sum_vars, "_ttl")

    df <- df %>%
      group_by(...) %>%
      mutate({{sum_rst}} := sum(!!sym(sum_vars), na.rm = TRUE)) %>%
      #mutate_at(.vars = vars(all_of(sum_rst)),.funs = ~ sum(!!sym(sum_vars), na.rm = TRUE)) %>%
      ungroup()

    return(df)
  }

  # Summarise
  df %>%
    group_by(...) %>%
    summarise(across(all_of(sum_vars), sum, na.rm = TRUE), .groups = "drop")
}


#' @title Categorize age groups
#'
#'
group_agesex <- function(.data) {

  .data %>%
    mutate(
      category = case_when(
        age == "<15" ~ "Children",
        age == "15+" & sex == "Female" ~ "Adult Female",
        age == "15+" & sex == "Male" ~ "Adult Male",
        TRUE ~ NA_character_
      )) %>%
    relocate(category, .before = age)
}

#' @title Ellipsis
#'
test_ellipsis <- function(...) {
  args <- list(...)
  print(class(args))
  print(length(args))
  print(args %>% unlist)
  print(c(...))
}

#' @title Convert Ellipsis to Vector
#'
#' @param ... Additional and varing number of function parmeters
#'
ellipsis_args <- function(...) {

  #args <- list(...)

  args <- as.character(substitute(...()))

  return(args)
}


#' @title Unpack TX_ML Disaggs
#'
unpack_tx_ml <- function(.data,
                         sum_vars = 'cumulative',
                         unpack_iit = TRUE,
                         ...) {

  # Ellipsis parameters
  cols_grp <- ellipsis_args(...)

  # Check cols availability
  cols_req <- c("indicator",
                "standardizeddisaggregate",
                "otherdisaggregate")

  if (all(cols_req %ni% names(.data))) {
    print(cols_req)
    stop("Missing required columns from the dataset")
  }

  if ("indicator" %ni% cols_grp) {
    print("Adding `indicator` in group variables")

    cols_grp <- c(cols_grp, "indicator")
  }

  # Indicators
  ind <- "TX_ML"
  disag <- "Age/Sex/ARTNoContactReason/HIVStatus"

  if (ind %ni% .data $indicator | disag %ni% .data$standardizeddisaggregate) {
    print(paste0(ind, " :: ", disag))
    stop("Missing required values from the dataset's indicator or disaggs")
  }

  # Filter and Summarize TX_ML data
  df_tx_ml <- .data %>%
    filter(indicator == ind, standardizeddisaggregate == disag) %>%
    mutate(indicator = otherdisaggregate,
           indicator = str_remove(indicator, "No Contact Outcome - "),
           indicator = str_replace(indicator, "Interruption in Treatment", "IIT"),
           indicator = str_replace(indicator, " Treatment", ""),
           indicator = case_when(
             indicator == 'Transferred Out' ~ 'TX_ML_XFRED_OUT',
             indicator == 'IIT <3 Months' ~ 'TX_ML_IIT_LT3M',
             indicator == 'IIT 3+ Months' ~ 'TX_ML_IIT_3MPLUS',
             indicator == 'Refused Stopped' ~ 'TX_ML_REF_STOPPED',
             indicator == 'Died' ~ 'TX_ML_DIED',
             TRUE ~ 'TX_ML_OTHER'
           )) %>%
    #group_by(...) %>%
    group_by_at(vars(all_of(cols_grp))) %>%
    summarise(across(all_of(sum_vars), sum, na.rm = TRUE), .groups = "drop")

  # Group IIT <3m & 3m+ into 1 category
  if (!unpack_iit) {

    cols <- setdiff(names(df_tx_ml), c(sum_vars, "indicator"))

    print(cols)

    df_tx_ml <- df_tx_ml %>%
      filter(str_detect(indicator, "IIT")) %>%
      group_by_at(all_of(cols)) %>%
      summarise(across(all_of(sum_vars), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(indicator = "TX_ML_IIT") %>%
      bind_rows(df_tx_ml, .) %>%
      filter(str_detect(indicator, "TX_ML_IIT_.*", negate = TRUE))
  }

  return(df_tx_ml)
}


#' @title Summarize TX_CURR with No Contacts
#'
#'
tx_nocontact <- function(.data,
                         rep_pd = "FY21Q3",
                         unpack = "ml",
                         ...) {

  # ... arguments
  cols_grp <- ellipsis_args(...)

  if (!"indicator" %in% cols_grp) {
    cols_grp <- c(cols_grp, "indicator")
  }

  cols_tx_grp <- c(cols_grp, "standardizeddisaggregate", "otherdisaggregate")

  cols_tx_ml <- c(cols_grp, "value")


  ## Indicators
  inds <- c(
    'TX_CURR',
    'TX_NEW',
    'TX_RTT',
    'TX_ML',
    'TX_NET_NEW')

  ind_levels1 <- c(
    'TX_CURR_LAG1',
    'TX_NEW',
    'TX_RTT',
    'TX_CURR_ADJ',
    'TX_ML',
    'TX_CURR',
    'TX_NET_NEW')

  ind_levels2 <- c(
    'TX_CURR_LAG1',
    'TX_NEW',
    'TX_RTT',
    'TX_CURR_ADJ',
    'TX_ML_IIT',
    'TX_ML_XFRED_OUT',
    'TX_ML_REF_STOPPED',
    'TX_ML_DIED',
    'TX_CURR',
    'TX_NET_NEW')

  ind_levels3 <- c(
    'TX_CURR_LAG1',
    'TX_NEW',
    'TX_RTT',
    'TX_CURR_ADJ',
    'TX_ML_IIT_LT3M',
    'TX_ML_IIT_3MPLUS',
    'TX_ML_XFRED_OUT',
    'TX_ML_REF_STOPPED',
    'TX_ML_DIED',
    'TX_CURR',
    'TX_NET_NEW')

  # Periods: curr + prev
  hist_pds <- .data %>% identify_pds(pd_end = rep_pd, len = 2)

  curr_pd <- hist_pds[1]
  prev_pd <- hist_pds[2]

  # TX_CURR, NET_NEW, RTT & TX_ML
  df_tx <- NULL

  # Filter based on unpacking method
  if (is.null(unpack) | is.na(unpack)) {
    df_tx <- .data %>%
      filter(indicator %in% inds &
               standardizeddisaggregate == 'Total Numerator')
  } else {
    df_tx <- .data %>%
      filter(indicator %in% inds[inds != "TX_ML"] &
               standardizeddisaggregate == "Age/Sex/HIVStatus" |
             indicator == "TX_ML" &
               standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus")
  }

  # Initial Summary
  df_tx <- df_tx %>%
    reshape_msd() %>%
    filter(period == curr_pd | (period == prev_pd & indicator == "TX_CURR")) %>%
    mutate(
      indicator = case_when(
        indicator == "TX_CURR" & period == prev_pd ~ "TX_CURR_LAG1",
        TRUE ~ indicator)) %>%
    select(-c(period, period_type)) %>%
    group_by_at(all_of(cols_tx_grp)) %>%
    summarise(across(all_of("value"), sum, na.rm = TRUE), .groups = "drop")

  # Unpack ML
  if (is.null(unpack) | is.na(unpack)) {

    ind_levels <- ind_levels1

    df_tx <- df_tx %>%
      group_by_at(all_of(cols_grp)) %>%
      summarise(across(all_of("value"), sum, na.rm = TRUE), .groups = "drop")

  } else if (unpack == "ml") {

    ind_levels <- ind_levels2

    df_tx_ml <- df_tx %>%
      unpack_tx_ml(sum_vars = "value", unpack_iit = F, ...)

    df_tx <- df_tx %>%
      filter(indicator != "TX_ML") %>%
      select(all_of(cols_tx_ml)) %>%
      bind_rows(df_tx_ml)

  } else if (unpack == "iit") {

    ind_levels <- ind_levels3

    df_tx_ml <- df_tx %>%
      unpack_tx_ml(sum_vars = "value", unpack_iit = T, ...)

    df_tx <- df_tx %>%
      filter(indicator != "TX_ML") %>%
      select(all_of(cols_tx_ml)) %>%
      bind_rows(df_tx_ml)

  } else {
    stop("PARAM - Invalid `unpack` option")
  }

  # Labels => for scale_x_manual()
  ind_labels <- case_when(
    ind_levels == "TX_CURR" ~ curr_pd,
    ind_levels == "TX_CURR_LAG1" ~ prev_pd,
    TRUE ~ NA_character_
  )

  # summarize and adjust
  df_tx <- df_tx %>%
    filter(indicator %in% ind_levels[1:3]) %>%
    group_by_at(vars(-c(indicator, value))) %>%
    summarise(across(value, sum, na.rm = T), .groups = "drop") %>%
    mutate(indicator = "TX_CURR_ADJ") %>%
    bind_rows(df_tx, .) %>%
    mutate(indicator = factor(indicator, levels = ind_levels, ordered = T)) #%>%
    # group_by_at(vars(-c(indicator, value))) %>%
    # mutate(
    #   label_value = case_when(
    #     str_detect(indicator, "TX_ML") ~ -1 * value,
    #     TRUE ~ value
    #   ),
    #   label_changes = case_when(
    #     str_detect(indicator, "NEW|RTT") ~ str_remove(indicator, "TX_"),
    #     indicator == "TX_ML" ~ str_remove(indicator, "TX_"),
    #     str_detect(indicator, "XFR") ~ "XFR-OUT",
    #     str_detect(indicator, "REF") ~ "R-STOP",
    #     str_detect(indicator, "REF") ~ "R-STOP",
    #     str_detect(indicator, "ITT_LT3M") ~ "ITT <3mo",
    #     str_detect(indicator, "ITT_3MPLUS") ~ "ITT 3mo+",
    #     str_detect(indicator, "ADJ") ~ "ADJ",
    #     str_detect(indicator, "TX_ML_.*") ~ str_remove(indicator, "TX_ML_"),
    #     TRUE ~ ""
    #   ),
    #   label_stages = case_when(
    #     indicator == "TX_CURR" ~ curr_pd,
    #     indicator == "TX_CURR_LAG1" ~ prev_pd,
    #     TRUE ~ ""
    #   ),
    #   label = case_when(
    #     !is.na(label_changes) ~ paste0(label_changes, "\n", comma(value)),
    #     TRUE ~ comma(value)
    #   ))

  return(df_tx)
}

#' @title Prep TX_ML data for Plot
#'
tx_ml_colors <- function(.data) {

  cols <- .data %>%
    select(!(indicator:last_col())) %>%
    names()

  .data %>%
    group_by_at(vars(-c(indicator, value))) %>%
    mutate(
      label_value = case_when(
        str_detect(indicator, "TX_ML") ~ -1 * value,
        TRUE ~ value
      ),
      label_changes = case_when(
        str_detect(indicator, "NEW|RTT") ~ str_remove(indicator, "TX_"),
        indicator == "TX_ML" ~ str_remove(indicator, "TX_"),
        #indicator == "TX_ML_IIT" ~ "ITT",
        #indicator == "TX_ML_IIT_LT3M" ~ "ITT <3mo",
        #indicator == "TX_ML_IIT_3MPLUS" ~ "ITT 3mo+",
        str_detect(indicator, "XFR") ~ "XFR-OUT",
        str_detect(indicator, "REF") ~ "R-STOP",
        str_detect(indicator, "IIT_LT3M") ~ "ITT <3mo",
        str_detect(indicator, "IIT_3MPLUS") ~ "ITT 3mo+",
        str_detect(indicator, "ADJ") ~ "ADJ",
        str_detect(indicator, "TX_ML_.*") ~ str_remove(indicator, "TX_ML_"),
        TRUE ~ ""
      ),
      label_stages = case_when(
        indicator == "TX_CURR" ~ curr_pd,
        indicator == "TX_CURR_LAG1" ~ prev_pd,
        TRUE ~ ""
      ),
      label = case_when(
        !is.na(label_changes) ~ paste0(label_changes, "\n", comma(value)),
        TRUE ~ comma(value)
      )) %>%
    ungroup() %>%
    group_by_at(all_of(cols)) %>%
    arrange(indicator) %>%
    mutate(
      ymin = case_when(
        indicator == "TX_CURR_ADJ" ~ value,
        str_detect(indicator, "TX_CURR") ~ 0,
        indicator == "TX_NEW" ~ lag(value, 1),
        indicator == "TX_RTT" ~ lag(cumsum(value), 1),
        str_detect(indicator, "TX_ML") ~ value[indicator == "TX_CURR_ADJ"] - cumsum(ifelse(str_detect(indicator, "TX_ML"), value, 0)),
        indicator == "TX_NET_NEW" ~ value[indicator == "TX_CURR_LAG1"],
        TRUE ~ value
      ),
      ymax = case_when(
        indicator == "TX_CURR_ADJ" ~ value,
        TRUE ~ ymin + value
      )) %>%
    ungroup() %>%
    mutate(
      color = case_when(
        indicator %in% c('TX_CURR_LAG1', 'TX_CURR') ~ scooter,
        indicator %in% c("TX_NEW", "TX_RTT") ~ genoa,
        indicator == "TX_CURR_ADJ" ~ grey30k,
        indicator == "TX_NET_NEW" ~ genoa_light,
        TRUE ~ burnt_sienna # for all the TX_ML
      ))
}


#' @title Plot TX_ML data
#'
tx_ml_bars <- function(.data, lsize = 6, ...) {

  # Bar width
  w <- .96/2

  # Split data
  df_viz <- .data %>%
    filter(indicator != "TX_NET_NEW")

  df_nn <- .data %>%
    filter(indicator == "TX_NET_NEW")

  # limits
  n_ind <- df_viz %>%
    distinct(indicator) %>%
    pull() %>%
    length()

  xlim_labels <- df_viz %>%
    distinct(label_stages) %>%
    filter(label_stages != "") %>%
    pull(label_stages) %>%
    paste0("TX_CURR\n(", ., ")")

  xlim_labels <- xlim_labels %>%
    append(x = .,
           values = rep("", times = n_ind - length(xlim_labels)),
           after = 1)

  # Plot bar chart
  viz <- ggplot(data = df_viz) +
    geom_rect(aes(xmin = as.integer(indicator) - w,
                  xmax = as.integer(indicator) + w,
                  ymin = 0,
                  ymax = ymax,
                  fill = trolley_grey_light)) +
    geom_rect(aes(xmin = as.integer(indicator) - w,
                  xmax = as.integer(indicator) + w,
                  ymin = ymin,
                  ymax = ymax,
                  fill = color)) +
    geom_text(aes(x = as.integer(indicator),
                  y = ymax,
                  label = comma(label_value)),
              size = lsize,
              color = usaid_black,
              vjust = -1) +
    geom_text(aes(x = as.integer(indicator),
                  y = ymin,
                  label = label_changes),
              size = lsize,
              color = usaid_black,
              vjust = 1.4) +
    geom_segment(data = df_nn,
                 aes(x = as.integer(indicator) - 1 - w,
                     xend = as.integer(indicator) - 1 + w,
                     y = ymin,
                     yend = ymin),
                 size = 1,
                 color = grey10k) +
    geom_curve(data = df_nn,
               aes(x = as.integer(indicator) - 1 - (w/2),
                   xend = as.integer(indicator) - 1 - (w/2),
                   y = ymin - value,
                   yend = ymin + (value / 2)),
               arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
               curvature = -0.5,
               #angle = 90,
               size = 1,
               color = grey10k) +
    geom_text(data = df_nn,
              aes(x = as.integer(indicator) - 1,
                  y = ymin - value,
                  label = paste0("NN\n", comma(value))),
              size = lsize,
              color = grey10k) +
    scale_fill_identity() +
    scale_x_discrete(limits = xlim_labels) +
    facet_wrap(vars(...)) +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = lsize * .pt, color = usaid_black))

  return(viz)
}


#' @title Clean modalities
#'
#'
clean_modalities <- function(.data, colname = "modality") {

  .data %>%
    mutate(across(all_of(colname), ~ case_when(
      . == "Inpat" ~ "Inpatient",
      . == "HomeMod" ~ "Home-Based (Community)",
      . == "Index" ~ "Index (Facility)",
      . == "IndexMod" ~ "Index (Community)",
      . == "MobileMod" ~ "Mobile (Community)",
      . == "TBClinic" ~ "TB Clinic",
      . == "OtherPITC" ~ "Other PITC",
      . == "VCTMod" ~ "VCT (Community)",
      . == "OtherMod" ~ "Other Community",
      . == "Emergency Ward" ~ "Emergency",
      TRUE ~ .
    )))
}

#' @title Clean ARV Dispsensing
#'
#'
clean_arv_dispense <- function(.data,
                               colname = "otherdisaggregate",
                               add_3plus = FALSE) {

  .data <- .data %>%
    dplyr::mutate(across(all_of(colname), ~ str_remove(., "ARV Dispensing Quantity - "))) %>%
    dplyr::mutate(across(all_of(colname), ~ case_when(
      . == "Less than 3 months" ~ "<3",
      . == "3 to 5 months" ~ "3-5",
      . == "6 or more months" ~ "6+",
      is.na(.) ~ "tn",
      TRUE ~ .
    )))

  if (add_3plus) {

    val_cols <- c(paste0("qtr", 1:4), "cummulative", "value")

    val_cols <- .data %>%
      names() %>%
      intersect(val_cols)

    grp_cols <- .data %>%
      names() %>%
      setdiff(c(val_cols, colname))

    print(grp_cols)

    .data <- .data %>%
      filter(across(all_of(colname), ~ . %in% c("3-5", "6+"))) %>%
      group_by_at(.vars = all_of(grp_cols)) %>%
      summarise(across(all_of(val_cols), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(!!colname := "3+") %>%
      bind_rows(.data, .)

  }

  return(.data)
}

#' @title Clean Index
#'
#'
facility_type <- function(.data, colname = "disaggregate") {
  .data %>%
    mutate(
      community_facility = case_when(
        str_detect(disaggregate, "IndexMod/") ~ "Community",
        str_detect(disaggregate, "Index/") ~ "Facility",
        TRUE ~ NA_character_
      )
    )
}


#' @title Update Mechs
#'
#' @description Update details of TBD Mechs
#'
#'
update_mechs <- function(.data) {
  .data %>%
    mutate(
      primepartner = case_when(
        mech_code == 160521 ~ 'Achieving Health Nigeria Initiative (AHINi)',
        mech_code == 160522 ~ 'Georgetown Global Health Nigeria (GGHN)',
        mech_code == 160523 ~ 'Health Systems Consult Limited (HSCL)',
        mech_code == 160524 ~ 'Center for Clinical Care and Clinical Research (CCCRN)',
        mech_code == 160525 ~ 'Heartland Alliance Ltd GTE (HALG)',
        mech_code == 160527 ~ 'AKS & CRS',
        TRUE ~ primepartner
      ),
      mech_name = case_when(
        mech_code == 160521 ~ 'ACE Cluster 1',
        mech_code == 160522 ~ 'ACE Cluster 2',
        mech_code == 160523 ~ 'ACE Cluster 3',
        mech_code == 160524 ~ 'ACE Cluster 4',
        mech_code == 160525 ~ 'ACE Cluster 6',
        mech_code == 160527 ~ 'ACE Cluster 5',
        TRUE ~ mech_name
      )
    )
}

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
        mech_name == "Youth Power Action" ~ "YPA",
        # FY22 - New Awards
        mech_name == "ACE Cluster 1" ~ "ACE 1",
        mech_name == "ACE Cluster 2" ~ "ACE 2",
        mech_name == "ACE Cluster 3" ~ "ACE 3",
        mech_name == "ACE Cluster 4" ~ "ACE 4",
        mech_name == "ACE Cluster 5" ~ "ACE 5",
        mech_name == "ACE Cluster 6" ~ "ACE 6",
        # CDC
        mech_name == "Partnering Effectively to end AIDS through Results and Learning (PEARL)_2097" ~ "PEARL",
        mech_name == "Global Action towards HIV Epidemic Control in Subnational units in Nigeria (4GATES PROJECT)_2100" ~ "4GATES",
        mech_name == "ACTION to Control HIV Epidemic through Evidence (ACHIEVE)_2099" ~ "ACHIEVE",
        mech_name == "Improving Comprehensive AIDS Response Enhanced for Sustainability (iCARES)_2098" ~ "iCARES",
        # DOD
        mech_name == "Henry Jackson Foundation" ~ "HJF",
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
        primepartner == "HEARTLAND ALLIANCE" ~ "HEARTLAND",
        primepartner == "HEARTLAND ALLIANCE LTD-GTE" ~ "HEARTLAND",
        primepartner == "Heartland Alliance International, LLC" ~ "HEARTLAND",
        primepartner == "CENTER FOR CLINICAL CARE AND CLINICAL RESEARCH LTD GTE" ~ "C4C3R",
        primepartner == "ASSOCIATION FOR REPRODUCTIVE  AND FAMILY HEALTH" ~ "ARFH",
        primepartner == "PRO-HEALTH INTERNATIONAL" ~ "ProHI",
        primepartner == "Management Sciences For Health, Inc." ~ "MHS",
        primepartner == "APIN PUBLIC HEALTH INITIATIVE S LTD/GTE" ~ "APIN",
        primepartner == "CATHOLIC CARITAS FOUNDATION O F NIGERIA" ~ "CARITAS",
        # FY22 - New Partners
        primepartner == 'Achieving Health Nigeria Initiative (AHINi)' ~ 'AHINi',
        primepartner == 'Georgetown Global Health Nigeria (GGHN)' ~ 'GGHN',
        primepartner == 'Health Systems Consult Limited (HSCL)' ~ 'HSCL',
        primepartner == 'Center for Clinical Care and Clinical Research (CCCRN)' ~ 'CCCRN',
        primepartner == 'Heartland Alliance Ltd GTE (HALG)' ~ 'HALG',
        primepartner == 'AKS & CRS' ~ 'AKS & CRS',
        # CDC
        primepartner == "APIN PUBLIC HEALTH INITIATIVES LTD/GTE" ~ "APHI",
        primepartner == "INSTITUTE OF HUMAN VIROLOGY" ~ "IHVN",
        primepartner == "CATHOLIC CARITAS FOUNDATION OF NIGERIA" ~ "CCFN",
        primepartner == "CENTRE FOR INTEGRATED HEALTH PROGRAMS" ~ "CIHP",
        # DOD
        primepartner == "Henry M. Jackson Foundation For The Advancement Of Military Medicine, Inc., The" ~ "HJF",
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


#' @title Extract Mechanisms Details
#' @description
#' @param .df_msd MSD Dataset
#'
extract_mechs <- function(.df_msd) {
  .df_msd %>%
    filter(fundingagency != "Dedup", is.na(cumulative)) %>%
    select(fiscal_year, fundingagency, psnu, mech_code, mech_name, primepartner) %>%
    distinct() %>%
    clean_agency() %>%
    update_mechs() %>%
    partners_label()
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


#' @title Get lag of current reporting period
#'
#'
lag_pd <- function(pd, n = 2, type = "full") {

  if (type != "full")
    stop("INVALID INPUT - Period type not currently supported")

  # Fiscal year
  fy <- pd %>%
    str_sub(3, 4) %>%
    as.integer()

  # Rererence FY & Quaters
  valid_qtrs <- c(1:4)
  lookup_qtrs <- rep(valid_qtrs, times = n)
  lookup_fys <- rep(c(fy-1, fy), times = 1, each = length(valid_qtrs))

  #
  curr_qtr <- pd %>%
    str_sub(-1) %>%
    as.integer()

  idx_qtr <- match(curr_qtr, valid_qtrs) + length(valid_qtrs)

  lag_qtr <- lookup_qtrs[idx_qtr - n]
  lag_fy <- lookup_fys[idx_qtr - n]

  pd <- paste0("FY", lag_fy, "Q", lag_qtr)

  return(pd)
}

#' @title States Prioritization
#'
#'
get_prioritization <- function(df_nat,
                               fy = 2021,
                               country = "Nigeria") {

  df_nat %>%
    filter(fiscal_year == fy,
           countryname == country,
           snuprioritization != "Missing") %>%
    select(psnuuid, psnu, snuprioritization) %>%
    distinct() %>%
    mutate(
      flag = case_when(
        psnu %in% c("Akwa Abom", "Rivers", "Delta", "Lagos", "Enugu", "Imo") ~ "Red",
        psnu %in% c("Gombe", "Nasarawa", "Benue") ~ "Green",
        psnu %in% c("_Military Nigeria") ~ NA_character_,
        TRUE ~ "Yellow"
      ))
}

#' @title States Prioritization
#'
#'
set_prioritization <- function(df_nat,
                               fy = NULL,
                               cntry = NULL) {

  # Fiscal year
  if (!is.null(fy))
    df_nat <- df_nat %>% filter(fiscal_year == fy)

  # Country
  if(!is.null(cntry))
    df_nat <- df_nat %>% filter(countryname == cntry)

  # Update prioritization
  df_nat %>%
    mutate(
      snupriority = case_when(
        psnu %in% c("Akwa Abom", "Rivers", "Delta", "Lagos", "Enugu", "Imo") ~ "Red",
        psnu %in% c("Gombe", "Nasarawa", "Benue") ~ "Green",
        psnu %in% c("_Military Nigeria") ~ NA_character_,
        TRUE ~ "Yellow"
      ))
}


#' @title Round parts to total to 100%
#'
equal_parts <- function(x, percent = T) {

  df_x <- tibble(value = x)

  if (percent != TRUE | all(x < 1) == TRUE) {
    df_x <- df_x %>%
      mutate(value = value * 100)
  }

  df_x <- df_x %>%
    mutate(flr = floor(value),
           ext = value - flr,
           rnd = round(value, 0),
           idx = row_number())

  t <- df_x %>% pull(rnd) %>% sum()
  r <- 0
  ad <- 1

  if (t < 100) {
    r <- 100 - t
    ad <- 1
  }
  else if (t > 100) {
    ad <- -1
  }
  else if (t == 100) {
    ad <- 0
  }

  df_x <- df_x %>%
    arrange(desc(ext)) %>%
    mutate(
      rnd = case_when(
        row_number() <= r ~ rnd + ad,
        TRUE ~ rnd
      )
    ) %>%
    arrange(idx)

  return(df_x$rnd)
}

#equal_parts(c(0.01044178, 0.56472255, 0.42483567))
#equal_parts(c(1.044178, 56.472255, 42.483567))


#' @title Get formatted current date
#'
#'
curr_date <- function(fmt = "%Y-%m-%d") {
  d <- base::Sys.Date()

  if (!is.null(fmt)) {
    d <- base::format(d, fmt)
  }

  return(d)
}

#' @title Get n% of a number
#'
#' @param n  Number
#' @param p Proportion, default 1
#' @param ...  Other valid options of round
#'
get_proportion <- function(n, p = 1, ...) {
  base::sapply(n, function(x) {
    round((x * (p / 100)), ...)
  })
}


#' @title Locator map
#'
#' @param aoi
#'
locator_map <- function(spdf, terr,
                        country = "Nigeria",
                        aoi = NULL,
                        lbl_all = TRUE,
                        lbl_size = 3) {
  # Notification
  print(aoi)

  # Extract admin 0 and 1 for basemap
  admin0 <- spdf %>%
    filter(operatingunit == country,
           label == "country")

  admin1 <- spdf %>%
    filter(operatingunit == country,
           label == "snu1")

  # Identify oai
  locator <- spdf %>%
    filter(operatingunit == country,
           label == "prioritization",
           name == aoi)

  # Target layer for labels
  if ( lbl_all == TRUE) {
    lbl_layer <- admin1
  } else {
    lbl_layer <- locator
  }

  # Produce basemap
  basemap <- terrain_map(countries = admin0,
                         adm0 = admin0,
                         adm1 = admin1,
                         mask = TRUE,
                         terr = terr)

  # locator map
  map <- basemap +
    geom_sf(data = locator,
            fill = glitr::burnt_sienna,
            alpha = .8) +
    geom_sf_text(data = lbl_layer,
                 aes(label = name),
                 size = lbl_size,
                 color = usaid_darkgrey)

  return(map)
}