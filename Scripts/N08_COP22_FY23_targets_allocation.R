##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: COP22 / FY23 Targets Allocation
##  LICENCE: MIT
##  DATE:    2022-03-17
##  UPDATED: 2022-03-23

## Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glamr)
  library(tameDP)
  library(janitor)
  library(glue)
  library(gt)
  library(gtExtras)

  library(datapackr)
  library(datimutils)
  library(datimvalidation)

  source("./Scripts/N00_Utilities.R")

## GLOBALS ----

  # Dirs ----

  dir_merdata <- si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_cop21 <- "../../PEPFAR/COUNTRIES/Nigeria/COPs/COP21"
  dir_cop22 <- "../../PEPFAR/COUNTRIES/Nigeria/COPs/COP22"

  dir_cop21 %>% open_path()

  # Files ----

  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY20.*_N")

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*_N")

  file_opu_dp <- dir_cop21 %>%
    return_latest("^OPU Data Pack_.*_\\d{8} _rev.xlsx$")

  file_cop21_dp <- dir_cop21 %>%
    return_latest("COP21 Data Pack .*.xlsx$")

  file_cop22_dp <- dir_cop22 %>%
    return_latest("Nigeria_datapack.*.xlsx$", recursive = T)

  # Params ----

  cntry <- "Nigeria"

  ou_uid <- get_ouuid(cntry)

  agency <- "USAID"

  curr_pd <- file_psnu_im %>% identify_pd()

  curr_fy <- curr_pd %>%
    str_sub(1, 4) %>%
    str_replace("FY", "20") %>%
    as.numeric()

  # indicators
  inds <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D")

  inds_cascade <- c("HTS_TST", "HTS_TST_POS", "PMTCT_HEI_POS",
                    "TX_NEW", "TX_CURR",
                    "TX_PVLS", "TX_PVLS_D")

  disaggs_cascade <- c(
    "Modality/Age/Sex/Result",
    "KeyPop/Result",
    "Age/Sex/HIVStatus",
    "KeyPop/HIVStatus"
  )

  # Age bands
  cop_age_labels <- list(peds = "<15",
                         adolescent = "15-19",
                         young = "20-24",
                         adult = "25+")

  cop_age_values <- list(peds = c("<01", "01-04", "05-09", "10-14"),
                         adolescent = c("15-19"),
                         young = c("20-24"),
                         adult = c("25-24", "25-29", "30-34",
                                   "35-39", "40-44", "45-49",
                                   "50-54", "55-59", "60+"))


# FUNCTION ----

  # Datim Login

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

  # Read PSNUxIM Data
  read_dp()

  # Clean DP Indicatoros

  # Map R to T Indicators

  # Summarize Results to Match Targets

  # IM Indicators by Age/Sex Proportion

# Datim Authentication ----

  datim_session()

# Load Data ----

  # MSD - PSNU x IM ----

  df_psnu <- file_psnu_im %>% read_msd()

  df_msd_indicators <- df_psnu %>%
    filter(str_detect(standardizeddisaggregate, "^Total.*tor$", negate = TRUE),
           str_detect(standardizeddisaggregate, "Age/Sex")) %>%
    select(indicator, numeratordenom, indicatortype,
           disaggregate, standardizeddisaggregate,
           ageasentered, sex,
           statushiv, statustb, statuscx,
           statustx = hiv_treatment_status,
           otherdisaggregate, modality, source_name) %>%
    distinct()

  # MSD - Sites x IM ----

  df_sites <- file_site_im %>% read_msd()

  df_sites %>% glimpse()
  df_sites %>% distinct(fundingagency)
  df_sites %>% distinct(psnu) %>% prinf()

  # FY22 Targets
  df_obs_cascade1a <- df_psnu %>%
    filter(fiscal_year == curr_fy) %>%
    clean_indicator() %>%
    filter(indicator %in% inds_cascade,
           standardizeddisaggregate == "Total Numerator") %>%
    mutate(
      indicator = case_when(
        indicator == "PMTCT_HEI_POS" ~ "HTS_TST_POS",
        TRUE ~ indicator
      )) %>%
    group_by(psnu, indicator) %>%
    summarise(across(targets, sum, na.rm = TRUE), .groups = "drop") %>%
    rename(targets_currfy = targets)

  # FY21 Targets & Results
  df_obs_cascade1b <- df_psnu %>%
    filter(fiscal_year == prev_fy) %>%
    clean_indicator() %>%
    filter(indicator %in% inds_cascade,
           standardizeddisaggregate == "Total Numerator") %>%
    mutate(
      indicator = case_when(
        indicator == "PMTCT_HEI_POS" ~ "HTS_TST_POS",
        TRUE ~ indicator
      )) %>%
    group_by(psnu, indicator) %>%
    summarise(across(c(targets, cumulative, starts_with("qtr")),
                     sum, na.rm = TRUE), .groups = "drop") %>%
    rename(target_prevfy = targets)

  # FY21 T&R + FY22 T
  df_obs_cascade1c <- df_psnu %>%
    filter(fiscal_year %in% c(prev_fy, curr_fy)) %>%
    clean_indicator() %>%
    filter(
      indicator %in% inds_cascade,
      #standardizeddisaggregate == "Total Numerator"
      standardizeddisaggregate %in% disaggs_cascade,
      !is.na(ageasentered)
    ) %>%
    mutate(
      indicator = case_when(
        indicator == "PMTCT_HEI_POS" ~ "HTS_TST_POS",
        TRUE ~ indicator
      )) %>%
    group_by(psnu, indicator, ageasentered, sex) %>%
    summarise(
      observed = sum(cumulative[fiscal_year == prev_fy], na.rm = T),
      planned = sum(targets[fiscal_year == curr_fy], na.rm = T),
      .groups = "drop"
    )

  df_obs_cascade1c <- df_obs_cascade1c %>%
    group_by(psnu, indicator) %>%
    summarise(across(all_of(c("observed", "planned")), sum, na.rm = T), .groups = "drop") %>%
    mutate(ageasentered = NA_character_, sex = NA_character_) %>%
    bind_rows(df_obs_cascade1c, .) %>%
    pivot_wider(names_from = indicator,
                names_sep = "_",
                values_from = c(observed, planned)) %>%
    arrange(psnu, ageasentered, sex)

  # PMTCT
  df_obs_pmtct <- df_psnu %>%
    filter(fiscal_year %in% c(prev_fy, curr_fy)) %>%
    clean_indicator() %>%
    filter(indicator %in% inds_cascade,
           standardizeddisaggregate %in% disaggs_cascade) %>%
    group_by(psnu, indicator, ageasentered, sex) %>%
    summarise(across(c(targets, cumulative, starts_with("qtr")),
                     sum, na.rm = TRUE), .groups = "drop")

  # COP Data ----
  cop_year <- curr_fy

  # Datim Reference Datasets ----

  cop_mechs <- datapackr::getMechanismView()

  cop_datapack <- datapackr::cop22_data_pack_schema

  cop_coc <- datapackr::getValidCategoryOptions()

  cop_de_coc <- datapackr::cop22_map_DataPack_DATIM_DEs_COCs %>% clean_names()

  df_cop_de_coc <- cop_de_coc %>%
    filter(targets_results == "targets",
           period == paste0(curr_fy, "Oct"),
           dataset == "mer",
           support_type == "DSD") %>%
    select(indicator_code,
           indicator = technical_area,
           disagg_type,
           categoryoptioncomboname,
           age = valid_ages_name,
           sex = valid_sexes_name,
           key_pop = valid_kps_name)

  cop_indicators <- df_cop_de_coc %>%
    distinct(indicator) %>%
    arrange(indicator) %>%
    pull()

  cop_cascade_indicators <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS")

  # PSNU => Error - "Obubra General Hospital" is not a PSNU
  cop_psnus <- datapackr::cop22_valid_PSNUs %>% filter(ou == cntry)

  # Data Pack ----

  #file_cop22_dp %>% tameDP::tame_dp()

  df_cop22 <- file_cop22_dp %>%
    read_dp() %>%
    select(-starts_with("...")) %>% #glimpse()
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{3}$", "_value")) %>%
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{1,2}$", "_share"))

  df_cop22 %>% glimpse()

  # COP IM Targets
  df_cop22_values <- df_cop22 %>%
    select(-ends_with("share")) %>%
    rename_with(.cols = ends_with("value"),
                .fn = ~str_remove(., "_value")) %>%
    pivot_longer(cols = ends_with("DSD"),
                 names_to = "Attribute",
                 values_to = "Value")

  # COP IM Allocations
  df_cop22_shares <- df_cop22 %>%
    select(PSNU:Rollup, ends_with("share")) %>%
    rename_with(.cols = ends_with("share"),
                .fn = ~str_remove(., "_share")) %>%
    pivot_longer(cols = ends_with("DSD"),
                 names_to = "Attribute",
                 values_to = "Share")

  # COP21 IM Allocations
  df_cop22_data <- df_cop22 %>% dp_extract_data()

  # COP21 Indicator / Disaggs ----

  df_cop_indicators0 <- df_cop22 %>%
    mutate(indicator = str_extract(indicator_code, "[^\\.]+"),
           tech_area = str_extract(indicator, "[^\\_]+")) %>%
    distinct(tech_area, indicator) %>%
    arrange(tech_area, indicator)

  df_cop_indicators1 <- df_cop22 %>%
    mutate(indicator = str_extract(indicator_code, "[^\\.]+"),
           tech_area = str_extract(indicator, "[^\\_]+")) %>%
    select(indicator_code, tech_area, indicator, Age, Sex, KeyPop) %>%
    distinct()

  df_cop_indicators2 <- df_cop22 %>%
    mutate(indicator = str_extract(indicator_code, "[^\\.]+"),
           tech_area = str_extract(indicator, "[^\\_]+")) %>%
    select(indicator_code, tech_area, indicator, Age, Sex, KeyPop) %>%
    clean_names() %>%
    left_join(df_cop_de_coc,
              by = c("indicator_code", "indicator", "age", "sex", "key_pop"))

  df_cop_indicators2 %>% glimpse()


# MUNGING ----

  df_psnu %>% glimpse()

  df_psnu %>% distinct(fundingagency)

  # USAID/Military Sites => These are community work reported above psnu
  df_sites %>%
    filter(fundingagency == agency,
           str_detect(psnu, "_Mil")) %>%
    distinct(fundingagency, psnu, community, sitename, facility, sitetype) %>%
    transpose() %>%
    unlist() %>%
    as_tibble(.name_repair = "unique")

  df_usaid_mil <- df_sites %>% filter(fundingagency == agency, str_detect(psnu, "_Mil"))

  df_usaid_mil %>%
    distinct(indicator) %>%
    prinf()

  # Mechanisms
  df_mechs <- df_sites %>%
    filter(fundingagency != "Dedup",
           !(fundingagency == agency & str_detect(psnu, "_Mil"))) %>%
    select(fiscal_year, fundingagency, psnu, mech_code, mech_name, primepartner) %>%
    distinct() %>%
    update_mechs() %>%
    partners_label()

  df_im_cov <- df_psnu %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup",
           !(fundingagency == agency & str_detect(psnu, "_Mil"))) %>%
    distinct(fundingagency, psnu, mech_code) %>%
    clean_agency() %>%
    arrange(psnu) %>%
    group_by(psnu) %>%
    mutate(n_mech = n_distinct(mech_code)) %>%
    ungroup() %>%
    pivot_wider(names_from = mech_code,
                values_from = fundingagency,
                names_sort = TRUE)

  df_sites %>% glimpse()

  df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup",
           !(fundingagency == agency & str_detect(psnu, "_Mil")),
           community != "Data reported above Community Level") %>%
    distinct(psnu, community, sitename)


  df_im_cov <- df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup",
           !(fundingagency == agency & str_detect(psnu, "_Mil"))
           #,community != "Data reported above Community Level"
           ) %>%
    distinct(fundingagency, psnu, community, sitename, mech_code) %>%
    clean_agency() %>%
    arrange(psnu) %>%
    group_by(fundingagency, psnu, mech_code) %>%
    summarise(im_comms = n_distinct(community),
              im_sites = n_distinct(sitename)) %>%
    ungroup() %>%
    group_by(fundingagency, psnu) %>%
    mutate(share = im_sites / n_distinct(sitename) * 100) %>%
    ungroup() %>%
    select(-c(community, sitename, im_sites)) %>%
    pivot_wider(names_from = mech_code,
                values_from = im_share,
                names_sort = TRUE)


# VIZ ----

  im_closing <- c("81857",
                  "81857",
                  "18655",
                  "81856",
                  "18655",
                  "100222")

  im_incoming <- c("160521",
                   "160522",
                   "160524",
                   "160525",
                   "160523",
                   "160527")

  # USAID Mechs - list of States
  df_mechs %>%
    filter(fundingagency == agency) %>%
    gt()

  # IM Coverage ----
  plot_ims_cov <-
    df_mechs %>%
    filter(fundingagency == agency,
           primepartner != "TBD",
           fiscal_year == curr_fy) %>%
    mutate(
      mech_code = case_when(
        mech_code %in% im_closing ~ paste0(mech_code, "*"),
        mech_code %in% im_incoming ~ paste0(mech_code, "**"),
        TRUE ~ mech_code
    )) %>%
    select(-c(primepartner, mech_name)) %>%
    arrange(desc(fundingagency), partner, psnu) %>%
    group_by(mech_code, partner) %>%
    summarise(psnus = paste0(psnu, collapse = ", "), .groups = "drop") %>%
    arrange(partner) %>%
    mutate(id = row_number()) %>%
    relocate(id, .before = 1) %>%
    gt() %>%
    cols_label(id = "",
               mech_code = "MECH. Code",
               partner = "IP & MECH. NAME",
               psnus = "STATES") %>%
    tab_header(title = md("**USAID/NIGERIA - IMPLEMENTING MECHANISMS**")) %>%
    fmt_missing(columns = everything(), missing_text = "....") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(side = c("top", "bottom"), color = grey70k, weight = px(2))
      ),
      locations = cells_column_labels(
        columns = everything()
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_fill(color = grey10k),
        cell_borders(side = "right", color = grey70k, weight = px(2))
      ),
      locations = cells_body(
        columns = "mech_code",
        rows = everything()
      )
    ) %>%
    tab_style(
      style = cell_text(color = usaid_red),
      locations = cells_body(
        columns = mech_code,
        rows = gt::matches("\\d*$")
      )
    ) %>%
    tab_source_note(source_note = "*  Mechanisms closing out") %>%
    tab_source_note(source_note = "** Mechanisms starting this year")

  gtsave(data = plot_ims_cov,
         filename = paste0(dir_graphics, "/Nigeria - Mechanisms State Coverage.png"),
         zoom = .6)

  gtsave(data = plot_ims_cov,
         filename = paste0(dir_graphics, "/Nigeria - Mechanisms State Coverage.pdf"),
         zoom = .6)

  ## Mechs - State COVERAGE
  plot_psnu_cov <-
    df_mechs %>%
      filter(fundingagency == agency,
             primepartner != "TBD",
             fiscal_year == curr_fy) %>%
      mutate(
        mech_code = case_when(
          mech_code %in% im_closing ~ paste0("<b>", mech_code, "*</b>"),
          mech_code %in% im_incoming ~ paste0("<b>", mech_code, "**</b>"),
          TRUE ~ paste0("<b>", mech_code, "</b>")
        ),
        partner = paste0(mech_code, " - ", partner)) %>%
      select(-c(primepartner, mech_name)) %>%
      arrange(desc(fundingagency), partner, psnu) %>%
      group_by(psnu) %>%
      summarise(partners = paste0(partner, collapse = ";  "), .groups = "drop") %>%
      arrange(psnu) %>%
      mutate(id = row_number()) %>%
      relocate(id, .before = 1) %>%
      gt() %>%
      cols_label(id = "",
                 psnu = "STATE",
                 partners = "MECHANISMS") %>%
      tab_header(title = md("**USAID/NIGERIA - IMPLEMENTING MECHANISMS**")) %>%
      fmt_missing(columns = everything(), missing_text = "....") %>%
      fmt_markdown(columns = partners, rows = everything()) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_borders(side = c("top", "bottom"), color = grey70k, weight = px(2))
        ),
        locations = cells_column_labels(
          columns = everything()
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = grey10k),
          cell_borders(side = "right", color = grey70k, weight = px(2))
        ),
        locations = cells_body(
          columns = psnu,
          rows = everything()
        )
      ) %>%
      tab_source_note(source_note = "*  Mechanisms closing out") %>%
      tab_source_note(source_note = "** Mechanisms starting this year")

  gtsave(data = plot_psnu_cov,
         filename = paste0(dir_graphics, "/Nigeria - States Mechanisms Coverage.png"),
         zoom = .7)

  gtsave(data = plot_psnu_cov,
         filename = paste0(dir_graphics, "/Nigeria - States Mechanisms Coverage.pdf"),
         zoom = .7)


  # IM List ----
  plot_ims <- df_mechs %>%
    filter(primepartner != "TBD") %>%
    mutate(partner = paste0(mech_code, " - ", partner)) %>%
    select(-c(primepartner, mech_name)) %>%
    arrange(desc(fundingagency), psnu, partner) %>%
    pivot_wider(names_from = fiscal_year,
                names_sort = TRUE,
                values_from = partner) %>%
    mutate(
      mech_code = case_when(
        mech_code %in% im_closing ~ paste0("<b>", mech_code, "*</b>"),
        mech_code %in% im_incoming ~ paste0("<b>", mech_code, "**</b>"),
        TRUE ~ paste0("<b>", mech_code, "</b>")
      )
    ) %>%
    group_by(fundingagency, psnu) %>%
    gt() %>%
    cols_label(mech_code = "MECHANISM") %>%
    tab_header(title = md("**USAID/NIGERIA - LIST of MECHANISMS**")) %>%
    fmt_missing(columns = everything(), missing_text = "....") %>%
    fmt_markdown(columns = mech_code, rows = everything()) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(side = c("top", "bottom"), color = grey70k, weight = px(2))
      ),
      locations = cells_column_labels(
        columns = everything()
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(side = "right", color = grey70k, weight = px(2))
      ),
      locations = cells_body(
        columns = "mech_code",
        rows = everything()
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = grey10k),
        cell_text(style = "italic", weight = "bold")
      ),
      locations = cells_body(
        columns = c(fundingagency, mech_code),
        rows = everything()
      )
    ) %>%
    tab_source_note(source_note = "*  Mechanisms closing out") %>%
    tab_source_note(source_note = "** Mechanisms starting this year")

  gtsave(data = plot_ims,
         filename = paste0(dir_graphics, "/Nigeria - List of All Mechanisms.png"),
         zoom = .7)

  gtsave(data = plot_ims,
         filename = paste0(dir_graphics, "/Nigeria - List of All Mechanisms.pdf"),
         zoom = .7)






