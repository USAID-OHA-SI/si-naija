##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: COP22 / FY23 Targets Allocation
##  LICENCE: MIT
##  DATE:    2022-04-11
##  UPDATED: 2022-04-12

## Libraries ----

  library(tidyverse)
  library(readxl)
  library(openxlsx)
  library(gophr)
  library(glamr)
  library(tameDP)
  library(janitor)
  library(glue)
  library(gt)
  library(gtExtras)

  source("./Scripts/N00_Utilities.R")

## GLOBALS ----

  # Dirs ----

  dir_merdata <- si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_cop22 <- "../../PEPFAR/COUNTRIES/Nigeria/COPs/COP22"

  #dir_cop22 %>% open_path()

  # Files ----

  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY20.*_N")

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*_N")

  file_subnat <- dir_merdata %>%
    glamr::return_latest(pattern = "NAT_SUBNAT_FY15")

  file_cop22_mechs <- dir_cop22 %>%
    file.path("Data") %>%
    return_latest("FY22 Mechanisms flags.xlsx")

  file_cop22_dp <- dir_cop22 %>%
    return_latest("Nigeria_datapack_\\d{8}.*v7.xlsx$")

  # Params ----

  cntry <- "Nigeria"

  ou_uid <- get_ouuid(cntry)

  ou_uid <- datapackr::cop22_valid_PSNUs %>%
    filter(ou == cntry) %>%
    pull(country_uid) %>%
    first()

  agency <- "USAID"

  curr_pd <- file_psnu_im %>% identify_pd()

  curr_fy <- curr_pd %>%
    str_sub(1, 4) %>%
    str_replace("FY", "20") %>%
    as.numeric()

  # Previous Fiscal Year
  prev_fy <- curr_fy - 1

  # Current COP
  cop_year <- curr_fy
  cop_fy <- curr_fy + 1

  # KeyPop mech
  kp_mechs <- c(81860, 81861)

  cop_ages_order <- list(
    default = c("<15", "15-19", "20-24", "25+", NA, "Total"),
    simple = c("<15", "15-19", "20-24", "25+", NA, "KeyPop", "Total"),
    ovc = c("<18", "18+", NA, "KeyPop", "Total"),
    all = c("<15", "15-19", "<18", "18+", "20-24", "25+", NA, "KeyPop", "Total"))

  # Age bands labels
  cop_age_labels <- list(peds = "<15",
                         ovc = "<18",
                         adolescent = "15-19",
                         young = "20-24",
                         adult = "25+")

  # Age bands groups
  cop_age_values <- list(peds = c("<01", "01-04", "05-09", "10-14"),
                         ovc = c("<01", "01-04", "05-09", "10-14", "15-17"),
                         adolescent = c("15-19"),
                         young = c("20-24"),
                         adult = c("25-24", "25-29", "30-34",
                                   "35-39", "40-44", "45-49",
                                   "50-54", "55-59", "50+", "60-64", "65+"))

# DATA

  # DP POPs
  # DP Targets
  df_plhiv <- file_cop22_dp %>%
    tame_dp(type = "PLHIV")

  # DP Targets
  df_targets <- file_cop22_dp %>%
    tame_dp(type = "PSNUxIM",
            map_names = T)

  df_targets <- df_targets %>%
    filter(mech_code != "00000")

  df_targets <- df_targets %>%
    clean_agency()

  # Extract Mechanisms
  df_dp_mechs <- df_targets %>%
    select(fundingagency, mech_code, mech_name, primepartner) %>%
    distinct() %>%
    update_mechs() %>%
    clean_mechs() %>%
    clean_partners()

  # Indicators
  dp_inds <- df_targets %>%
    distinct(indicator) %>%
    arrange(indicator) %>%
    pull()

  dp_ages <- df_targets %>%
    distinct(ageasentered) %>%
    arrange(ageasentered) %>%
    pull()

  dp_ages_sex_check <- df_targets %>%
    mutate(keypop = ifelse(str_detect(standardizeddisaggregate, "KeyPop"),
                           otherdisaggregate, NA_character_)) %>%
    distinct(indicator, standardizeddisaggregate, ageasentered, sex, keypop) %>%
    group_by(indicator, standardizeddisaggregate) %>%
    summarise(n_age = n_distinct(ageasentered, na.rm = T),
              n_sex = n_distinct(sex, na.rm = T),
              n_keypop = n_distinct(keypop, na.rm = T), .groups = "drop")


# MUNGE

  ## CASCADE ----

  # OU Targets
  df_targets_ou <- df_targets %>%
    clean_indicator() %>%
    filter(str_detect(indicator, "HTS_.*|TX_C.*|TX_N.*|TX_P.*")) %>%
    mutate(
      agecoarse = case_when(
        ageasentered %in% cop_age_values$peds ~ cop_age_labels$peds,
        ageasentered %in% cop_age_values$adolescent ~ cop_age_labels$adolescent,
        ageasentered %in% cop_age_values$young ~ cop_age_labels$young,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        is.na(ageasentered) & str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
        TRUE ~ ageasentered
      ),
      indicator = factor(
        indicator,
        levels = c("HTS_SELF", "HTS_INDEX", "HTS_TST", "HTS_TST_POS",
                   "HTS_RECENT", "TX_CURR", "TX_NEW", "TX_PVLS", "TX_PVLS_D"),
        ordered = T)) %>%
    group_by(indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)


  # Agency Targets
  df_targets_agency <- df_targets %>%
    clean_indicator() %>%
    filter(str_detect(indicator, "HTS_.*|TX_C.*|TX_N.*|TX_P.*")) %>%
    mutate(
      agecoarse = case_when(
        ageasentered %in% cop_age_values$peds ~ cop_age_labels$peds,
        ageasentered %in% cop_age_values$adolescent ~ cop_age_labels$adolescent,
        ageasentered %in% cop_age_values$young ~ cop_age_labels$young,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        is.na(ageasentered) & str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
        TRUE ~ ageasentered
      ),
      indicator = factor(
        indicator,
        levels = c("HTS_SELF", "HTS_INDEX", "HTS_TST", "HTS_TST_POS",
                   "HTS_RECENT", "TX_CURR", "TX_NEW", "TX_PVLS", "TX_PVLS_D"),
        ordered = T)) %>%
    group_by(fundingagency, indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)

  # PSNU x IM
  df_targets_psnu <- df_targets %>%
    clean_indicator() %>%
    filter(fundingagency == "USAID",
           str_detect(indicator, "HTS_.*|TX_C.*|TX_N.*|TX_P.*")) %>%
    mutate(
      agecoarse = case_when(
        ageasentered %in% cop_age_values$peds ~ cop_age_labels$peds,
        ageasentered %in% cop_age_values$adolescent ~ cop_age_labels$adolescent,
        ageasentered %in% cop_age_values$young ~ cop_age_labels$young,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        is.na(ageasentered) & str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
        TRUE ~ ageasentered
      ),
      indicator = factor(
        indicator,
        levels = c("HTS_SELF", "HTS_INDEX", "HTS_TST", "HTS_TST_POS",
                   "HTS_RECENT", "TX_CURR", "TX_NEW", "TX_PVLS", "TX_PVLS_D"),
        ordered = T)) %>%
    update_mechs() %>%
    clean_mechs() %>%
    group_by(psnu, mech_code, mech_name, indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)


  ## PMTCT ----

  # OU Targets
  df_targets_pmtct_ou <- df_targets %>%
    clean_indicator() %>%
    filter(str_detect(indicator, "PMTCT_.*")) %>%
    mutate(
      agecoarse = case_when(
        str_detect(ageasentered, "Mon") ~ NA_character_,
        ageasentered %in% cop_age_values$peds ~ cop_age_labels$peds,
        ageasentered %in% cop_age_values$adolescent ~ cop_age_labels$adolescent,
        ageasentered %in% cop_age_values$young ~ cop_age_labels$young,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        is.na(ageasentered) & str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
        TRUE ~ ageasentered
      ),
      indicator = case_when(
        !is.na(otherdisaggregate) ~ paste0(indicator, "-", otherdisaggregate),
        TRUE ~ indicator
      )) %>%
    filter(str_detect(indicator, ".*NewNeg", negate = T)) %>%
    group_by(indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)

  # Agency Targets
  df_targets_pmtct_agency <- df_targets %>%
    clean_indicator() %>%
    filter(str_detect(indicator, "PMTCT_.*")) %>%
    mutate(
      agecoarse = case_when(
        str_detect(ageasentered, "Mon") ~ NA_character_,
        ageasentered %in% cop_age_values$peds ~ cop_age_labels$peds,
        ageasentered %in% cop_age_values$adolescent ~ cop_age_labels$adolescent,
        ageasentered %in% cop_age_values$young ~ cop_age_labels$young,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        is.na(ageasentered) & str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
        TRUE ~ ageasentered
      ),
      indicator = case_when(
        !is.na(otherdisaggregate) ~ paste0(indicator, "-", otherdisaggregate),
        TRUE ~ indicator
      )) %>%
    filter(str_detect(indicator, ".*NewNeg", negate = T)) %>%
    group_by(fundingagency, indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)

  # psnu Targets
  df_targets_pmtct_psnu <- df_targets %>%
    clean_indicator() %>%
    filter(fundingagency == "USAID",
           str_detect(indicator, "PMTCT_.*")) %>%
    mutate(
      agecoarse = case_when(
        str_detect(ageasentered, "Mon") ~ NA_character_,
        ageasentered %in% cop_age_values$peds ~ cop_age_labels$peds,
        ageasentered %in% cop_age_values$adolescent ~ cop_age_labels$adolescent,
        ageasentered %in% cop_age_values$young ~ cop_age_labels$young,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        is.na(ageasentered) & str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
        TRUE ~ ageasentered
      ),
      indicator = case_when(
        !is.na(otherdisaggregate) ~ paste0(indicator, "-", otherdisaggregate),
        TRUE ~ indicator
      )) %>%
    filter(str_detect(indicator, ".*NewNeg", negate = T)) %>%
    update_mechs() %>%
    clean_mechs() %>%
    group_by(psnu, mech_code, mech_name, indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)


  ## GENDER & OVC ----

  # OU Targets
  df_targets_ovc_ou <- df_targets %>%
    clean_indicator() %>%
    filter(str_detect(indicator, "CXCA_.*|GEND.*|OVC.*")) %>%
    mutate(
      indicator = case_when(
        !is.na(otherdisaggregate) ~ paste0(indicator, "-", otherdisaggregate),
        TRUE ~ indicator
      ),
      agecoarse = case_when(
        str_detect(indicator, "OVC") & ageasentered %in% cop_age_values$ovc ~ cop_age_labels$ovc,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        TRUE ~ ageasentered
      )) %>%
    group_by(indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)

  # Agency Targets
  df_targets_ovc_agency <- df_targets %>%
    clean_indicator() %>%
    filter(str_detect(indicator, "CXCA_.*|GEND.*|OVC.*")) %>% #distinct(ageasentered)
    mutate(
      indicator = case_when(
        !is.na(otherdisaggregate) ~ paste0(indicator, "-", otherdisaggregate),
        TRUE ~ indicator
      ),
      agecoarse = case_when(
        str_detect(indicator, "OVC") & ageasentered %in% cop_age_values$ovc ~ cop_age_labels$ovc,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        TRUE ~ ageasentered
      )) %>%
    group_by(fundingagency, indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)

  # PSNU Targets
  df_targets_ovc_psnu <- df_targets %>%
    clean_indicator() %>%
    filter(
      fundingagency == "USAID",
      str_detect(indicator, "CXCA_.*|GEND.*|OVC.*")) %>% #distinct(ageasentered)
    mutate(
      indicator = case_when(
        !is.na(otherdisaggregate) ~ paste0(indicator, "-", otherdisaggregate),
        TRUE ~ indicator
      ),
      agecoarse = case_when(
        str_detect(indicator, "OVC") & ageasentered %in% cop_age_values$ovc ~ cop_age_labels$ovc,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        TRUE ~ ageasentered
      )) %>%
    update_mechs() %>%
    clean_mechs() %>%
    group_by(psnu, mech_code, mech_name, indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)

  ## TX_TB & PrEP ----

  ## TB ----

  # OU TB
  df_targets_tb_ou <- df_targets %>%
    clean_indicator() %>%
    filter(str_detect(indicator, "^TB_.*")) %>%
    mutate(
      indicator = case_when(
        !is.na(otherdisaggregate) ~ paste0(indicator, "-", otherdisaggregate),
        TRUE ~ indicator
      ),
      agecoarse = case_when(
        ageasentered %in% cop_age_values$peds ~ cop_age_labels$peds,
        ageasentered %in% cop_age_values$adolescent ~ cop_age_labels$adolescent,
        ageasentered %in% cop_age_values$young ~ cop_age_labels$young,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        is.na(ageasentered) & str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
        TRUE ~ ageasentered
      )) %>%
    filter(str_detect(indicator, "-NewNeg", negate = T)) %>%
    group_by(indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)

  # Agency
  df_targets_tb_agency <- df_targets %>%
    clean_indicator() %>%
    filter(str_detect(indicator, "^TB_.*")) %>%
    mutate(
      indicator = case_when(
        !is.na(otherdisaggregate) ~ paste0(indicator, "-", otherdisaggregate),
        TRUE ~ indicator
      ),
      agecoarse = case_when(
        ageasentered %in% cop_age_values$peds ~ cop_age_labels$peds,
        ageasentered %in% cop_age_values$adolescent ~ cop_age_labels$adolescent,
        ageasentered %in% cop_age_values$young ~ cop_age_labels$young,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        is.na(ageasentered) & str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
        TRUE ~ ageasentered
      )) %>%
    filter(str_detect(indicator, "-NewNeg", negate = T)) %>%
    group_by(fundingagency, indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)

  # PSNU x IM

  df_targets_tb_psnu <- df_targets %>%
    clean_indicator() %>%
    filter(fundingagency == "USAID",
           str_detect(indicator, "^TB_.*")) %>%
    mutate(
      indicator = case_when(
        !is.na(otherdisaggregate) ~ paste0(indicator, "-", otherdisaggregate),
        TRUE ~ indicator
      ),
      agecoarse = case_when(
        ageasentered %in% cop_age_values$peds ~ cop_age_labels$peds,
        ageasentered %in% cop_age_values$adolescent ~ cop_age_labels$adolescent,
        ageasentered %in% cop_age_values$young ~ cop_age_labels$young,
        ageasentered %in% cop_age_values$adult ~ cop_age_labels$adult,
        is.na(ageasentered) & str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
        TRUE ~ ageasentered
      )) %>%
    filter(str_detect(indicator, "-NewNeg", negate = T)) %>%
    update_mechs() %>%
    clean_mechs() %>%
    group_by(psnu, mech_code, mech_name, indicator, agecoarse) %>%
    summarise(targets = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = targets)



  # VIZ ----

  # HTS & TX ----

    # OU level ----
    df_targets_ou %>%
      filter(agecoarse != "KeyPop") %>%
      group_by() %>%
      summarise(across(-agecoarse, sum, na.rm = T), .groups = "drop") %>%
      mutate(agecoarse = "Total") %>%
      bind_rows(df_targets_ou, .) %>%
      gt() %>%
      tab_header(title = "FY23 OU TARGETS - HTS & TREATMENT") %>%
      cols_label(agecoarse = "AGE") %>%
      fmt_missing(columns = everything(), missing_text = "--") %>%
      fmt_number(columns = -agecoarse, decimals = 0) %>%
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_body(columns = agecoarse, rows = everything())
      ) %>%
      tab_style(
        style = list(cell_text(weight = "bold"), cell_fill(color = grey10k)),
        locations = cells_body(columns = everything(), rows = agecoarse == "Total")
      ) %>%
      tab_options(
        heading.title.font.weight = "bold",
        column_labels.font.weight = "bold"
      ) %>%
      gtsave(filename = file.path(dir_graphics, "Nigeria - FY23 OU Targets HTS and TX.png"))


  # Agency level ----
  df_targets_agency %>%
    distinct(fundingagency) %>%
    pull() %>%
    walk(function(.x) {

      df_targets_agency %>%
        filter(agecoarse %ni% "KeyPop") %>%
        group_by(fundingagency) %>%
        summarise(across(-agecoarse, sum, na.rm = T), .groups = "drop") %>%
        mutate(agecoarse = "Total") %>%
        bind_rows(df_targets_agency, .) %>%
        filter(fundingagency == .x) %>%
        group_by(fundingagency) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", .x, " TARGETS - HTS & TREATMENT")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = starts_with(c("HTS", "TX")), decimals = 0) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey10k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "Total")
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold",
          summary_row.background.color = grey10k
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ", .x,
                                           " Targets HTS and TX.png")))

    })


  # psnu level ----
  df_targets_psnu %>%
    distinct(psnu) %>%
    pull() %>%
    walk(function(.x) {

      df_targets_psnu_x <- df_targets_psnu %>%
        filter(agecoarse %ni% "KeyPop") %>%
        group_by(psnu, mech_code, mech_name) %>%
        summarise(across(-agecoarse, sum, na.rm = T), .groups = "drop") %>%
        mutate(agecoarse = "Total") %>%
        bind_rows(df_targets_psnu, .) %>%
        filter(psnu == .x)

      df_targets_psnu_x <- df_targets_psnu_x %>%
        filter(agecoarse %ni% "KeyPop") %>%
        group_by(psnu) %>%
        summarise(across(-c(mech_code, mech_name, agecoarse), sum, na.rm = T), .groups = "drop") %>%
        mutate(agecoarse = "G. Total", mech_code = NA, mech_name = NA) %>%
        bind_rows(df_targets_psnu_x, .) %>%
        mutate(mech_name = case_when(
          is.na(mech_name) ~ "",
          TRUE ~ paste0(mech_name, " (", mech_code, ")")
        )) %>%
        select(-psnu, -mech_code)

      df_targets_psnu_x %>%
        group_by(mech_name) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", .x, " TARGETS - HTS & TREATMENT")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = -agecoarse, decimals = 0) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey10k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "Total")
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey20k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "G. Total")
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold"
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ", .x,
                                           " Targets HTS and TX.png")))

    })

  # IM level
  df_targets_psnu %>%
    distinct(mech_code, mech_name) %>%
    #filter(row_number() == 1) %>%
    pwalk(function(mech_code, mech_name) {

      code = mech_code
      name = mech_name

      df_targets_psnu %>%
        filter(mech_code == code) %>%
        mutate(mech_name = paste0(mech_name, " (", mech_code, ")")) %>%
        select(-mech_code) %>%
        group_by(mech_name, psnu) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", name, " TARGETS - HTS & TREATMENT")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = -agecoarse, decimals = 0) %>%
        summary_rows(
          groups = TRUE,
          columns = -agecoarse,
          missing_text = "--",
          fns = list("Total" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        grand_summary_rows(
          columns = -agecoarse,
          missing_text = "--",
          fns = list("G. Total" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey10k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "Total")
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey20k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "G. Total")
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold",
          summary_row.background.color = grey10k,
          grand_summary_row.background.color = grey20k
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ",
                                           name, " - ", code, "-",
                                           " Targets HTS and TX.png")))

    })


  # PMTCT ----

  # Agency level ----
  df_targets_pmtct_ou %>%
    filter(agecoarse %ni% "KeyPop") %>%
    group_by() %>%
    summarise(across(-agecoarse, sum, na.rm = T), .groups = "drop") %>%
    mutate(agecoarse = "Total") %>%
    bind_rows(df_targets_pmtct_ou, .) %>%
    gt() %>%
    tab_header(title = "FY23 OU TARGETS - PMTCT") %>%
    cols_label(agecoarse = "AGE") %>%
    fmt_missing(columns = everything(), missing_text = "--") %>%
    fmt_number(columns = starts_with("PMTCT"), decimals = 0) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = agecoarse, rows = everything())
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold"), cell_fill(color = grey10k)),
      locations = cells_body(columns = everything(), rows = agecoarse == "Total")
    ) %>%
    tab_options(
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold"
    ) %>%
    gtsave(filename = file.path(dir_graphics, "Nigeria - FY23 OU Targets PMTCT.png"))


  # Agency level ----
  df_targets_pmtct_agency %>%
    distinct(fundingagency) %>%
    pull() %>%
    first() %>%
    walk(function(.x) {

      df_targets_pmtct_agency %>%
        filter(agecoarse %ni% "KeyPop") %>%
        group_by(fundingagency) %>%
        summarise(across(-agecoarse, sum, na.rm = T), .groups = "drop") %>%
        mutate(agecoarse = "Total") %>%
        bind_rows(df_targets_pmtct_agency, .) %>%
        filter(fundingagency == .x) %>%
        group_by(fundingagency) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", .x, " TARGETS - PMTCT")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = starts_with("PMTCT"), decimals = 0) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey10k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "Total")
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold"
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ", .x,
                                           " Targets PMTCT.png")))

    })


  # psnu level ----
  df_targets_pmtct_psnu %>%
    distinct(psnu) %>%
    pull() %>%
    walk(function(.x) {

      df_targets_pmtct_psnu_x <- df_targets_pmtct_psnu %>%
        filter(agecoarse %ni% "KeyPop") %>%
        group_by(psnu, mech_code, mech_name) %>%
        arrange(agecoarse) %>%
        summarise(across(starts_with("PMTCT"), sum, na.rm = T), .groups = "drop") %>%
        mutate(agecoarse = "Total") %>%
        bind_rows(df_targets_pmtct_psnu, .) %>%
        filter(psnu == .x)

      df_targets_pmtct_psnu_x <- df_targets_pmtct_psnu_x %>%
        filter(agecoarse %ni% "KeyPop") %>%
        group_by(psnu) %>%
        summarise(across(starts_with("PMTCT"), sum, na.rm = T), .groups = "drop") %>%
        mutate(agecoarse = "G. Total", mech_code = NA, mech_name = NA) %>%
        bind_rows(df_targets_pmtct_psnu_x, .) %>%
        mutate(mech_name = case_when(
          is.na(mech_name) ~ "",
          TRUE ~ paste0(mech_name, " (", mech_code, ")")
        )) %>%
        select(-psnu, -mech_code)

      df_targets_pmtct_psnu_x %>%
        mutate(
          agecoarse = if_else(is.na(agecoarse), "<2m", agecoarse),
          agecoarse = factor(agecoarse,
                                  levels = c("<2m", "<15", "15-19", "20-24", "25+",
                                             "Total", "G. Total"),
                                  exclude = NA,
                                  ordered = T)) %>%
        group_by(mech_name) %>%
        arrange(agecoarse) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", .x, " TARGETS - PMTCT")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = starts_with("PMTCT"), decimals = 0) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey10k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "Total")
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey20k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "G. Total")
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold"
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ", .x,
                                           " Targets PMTCT.png")))

    })

  # IM level ----
  df_targets_pmtct_psnu %>%
    distinct(mech_code, mech_name) %>%
    #filter(row_number() == 1) %>%
    pwalk(function(mech_code, mech_name) {

      code = mech_code
      name = mech_name

      df_targets_pmtct_psnu %>%
        filter(mech_code == code) %>%
        mutate(
          mech_name = paste0(mech_name, " (", mech_code, ")"),
          agecoarse = if_else(is.na(agecoarse), "<2m", agecoarse),
          agecoarse = factor(agecoarse,
                             levels = c("<2m", "<15", "15-19", "20-24", "25+",
                                        "Total", "G. Total"),
                             exclude = NA,
                             ordered = T)) %>%
        select(-mech_code) %>%
        group_by(mech_name, psnu) %>%
        arrange(agecoarse) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", name, " TARGETS - PMTCT")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = starts_with("PMTCT"), decimals = 0) %>%
        summary_rows(
          groups = TRUE,
          columns = -agecoarse,
          missing_text = "--",
          fns = list("Total" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        grand_summary_rows(
          columns = -agecoarse,
          missing_text = "--",
          fns = list("G. Total" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey10k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "Total")
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold"), cell_fill(color = grey20k)),
          locations = cells_body(columns = everything(), rows = agecoarse == "G. Total")
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold",
          summary_row.background.color = grey10k,
          grand_summary_row.background.color = grey20k
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ",
                                           name, " - ", code, "-",
                                           " Targets PMTCT.png")))

    })


  # OVC ----

  # OU level ----
  df_targets_ovc_ou %>%
    gt() %>%
    tab_header(title = "FY23 OU TARGETS - CXCA, GBV & OVC") %>%
    cols_label(agecoarse = "AGE") %>%
    fmt_missing(columns = everything(), missing_text = "--") %>%
    fmt_number(columns = -agecoarse, decimals = 0) %>%
    summary_rows(
      groups = NULL,
      columns = -agecoarse,
      missing_text = "--",
      fns = list("T" = ~sum(., na.rm = T)),
      formatter = fmt_number,
      decimals = 0
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = agecoarse, rows = everything())
    ) %>%
    tab_options(
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold"
    ) %>%
    gtsave(filename = file.path(dir_graphics, "Nigeria - FY23 OU Targets CXCA, GBV and OVC.png"))


  # Agency level ----
  df_targets_ovc_agency %>%
    distinct(fundingagency) %>%
    pull() %>%
    walk(function(.x) {

      df_targets_ovc_agency %>%
        filter(fundingagency == .x) %>%
        group_by(fundingagency) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", .x, " TARGETS - CXCA, GBV and OVC")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = -agecoarse, decimals = 0) %>%
        summary_rows(
          groups = NULL,
          columns = -agecoarse,
          missing_text = "--",
          fns = list("T" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold"
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ", .x,
                                           " Targets CXCA, GBV and OVC.png")))

    })


  # psnu level ----
  df_targets_ovc_psnu %>%
    distinct(psnu) %>%
    pull() %>%
    walk(function(.x) {

      df_targets_ovc_psnu %>%
        filter(psnu == .x) %>%
        mutate(mech_name = paste0(mech_name, " (", mech_code, ")")) %>%
        select(-psnu, -mech_code) %>%
        group_by(mech_name) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", .x, " TARGETS - CXCA, GBV and OVC")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = -agecoarse, decimals = 0) %>%
        summary_rows(
          groups = TRUE,
          columns = -agecoarse,
          missing_text = "--",
          fns = list("T" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        grand_summary_rows(
          columns = -agecoarse,
          missing_text = "--",
          fns = list("GT" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold"
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ", .x,
                                           " Targets CXCA, GBV and OVC.png")))

    })


  # IM level ----
  df_targets_ovc_psnu %>%
    distinct(mech_code, mech_name) %>%
    #filter(row_number() == 1) %>%
    pwalk(function(mech_code, mech_name) {

      code = mech_code
      name = mech_name

      df_targets_ovc_psnu %>%
        filter(mech_code == code) %>%
        mutate(
          mech_name = paste0(mech_name, " (", mech_code, ")"),
          agecoarse = if_else(is.na(agecoarse), "N/A", agecoarse),
          agecoarse = factor(agecoarse,
                             levels = c("N/A", "<18", "18+", "25+",
                                        "Total", "G. Total"),
                             exclude = NA,
                             ordered = T)
          ) %>%
        select(-mech_code) %>%
        group_by(mech_name, psnu) %>%
        arrange(agecoarse) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", name, " TARGETS - CXCA, GBV and OVC")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = -agecoarse, decimals = 0) %>%
        summary_rows(
          groups = TRUE,
          columns = -agecoarse,
          missing_text = "--",
          fns = list("T" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        grand_summary_rows(
          columns = -agecoarse,
          missing_text = "--",
          fns = list("GT" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold",
          summary_row.background.color = grey10k,
          grand_summary_row.background.color = grey20k
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ",
                                           name, " - ", code, "-",
                                           " Targets CXCA, GBV and OVC.png")))

    })

  # TB ----

  # OU level ----
  df_targets_tb_ou %>%
    gt() %>%
    tab_header(title = "FY23 OU TARGETS - TB") %>%
    cols_label(agecoarse = "AGE") %>%
    fmt_missing(columns = everything(), missing_text = "--") %>%
    fmt_number(columns = starts_with("TB"), decimals = 0) %>%
    summary_rows(
      groups = NULL,
      columns = starts_with("TB"),
      missing_text = "--",
      fns = list("T" = ~sum(., na.rm = T)),
      formatter = fmt_number,
      decimals = 0
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = agecoarse, rows = everything())
    ) %>%
    tab_options(
      heading.title.font.weight = "bold",
      column_labels.font.weight = "bold"
    ) %>%
    gtsave(filename = file.path(dir_graphics, "Nigeria - FY23 OU Targets TB.png"))


  # Agency level ----
  df_targets_tb_agency %>%
    distinct(fundingagency) %>%
    pull() %>%
    walk(function(.x) {

      df_targets_tb_agency %>%
        filter(fundingagency == .x) %>%
        group_by(fundingagency) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", .x, " TARGETS - TB")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = starts_with("TB"), decimals = 0) %>%
        summary_rows(
          groups = NULL,
          columns = starts_with("TB"),
          missing_text = "--",
          fns = list("T" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold"
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ", .x,
                                           " Targets TB.png")))

    })


  # psnu level ----
  df_targets_tb_psnu %>%
    distinct(psnu) %>%
    pull() %>%
    walk(function(.x) {

      df_targets_tb_psnu %>%
        filter(psnu == .x) %>%
        mutate(mech_name = paste0(mech_name, " (", mech_code, ")")) %>%
        select(-psnu, -mech_code) %>%
        group_by(mech_name) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", .x, " TARGETS - PMTCT")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = starts_with("TB"), decimals = 0) %>%
        summary_rows(
          groups = TRUE,
          columns = starts_with("TB"),
          missing_text = "--",
          fns = list("T" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        grand_summary_rows(
          columns = starts_with("TB"),
          missing_text = "--",
          fns = list("GT" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold"
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ", .x,
                                           " Targets TB.png")))

    })


  # IM level ----
  df_targets_tb_psnu %>%
    distinct(mech_code, mech_name) %>%
    #filter(row_number() == 1) %>%
    pwalk(function(mech_code, mech_name) {

      code = mech_code
      name = mech_name

      df_targets_tb_psnu %>%
        filter(mech_code == code) %>%
        mutate(
          mech_name = paste0(mech_name, " (", mech_code, ")"),
          agecoarse = if_else(is.na(agecoarse), "N/A", agecoarse),
          agecoarse = factor(agecoarse,
                             levels = c("<15", "15+", "15-19", "20-24", "25+",
                                        "Total", "G. Total"),
                             exclude = NA,
                             ordered = T)
        ) %>%
        select(-mech_code) %>%
        group_by(mech_name, psnu) %>%
        arrange(agecoarse) %>%
        gt() %>%
        tab_header(title = paste0("FY23 ", name, " TARGETS - TB")) %>%
        cols_label(agecoarse = "AGE") %>%
        fmt_missing(columns = everything(), missing_text = "--") %>%
        fmt_number(columns = starts_with("TB"), decimals = 0) %>%
        summary_rows(
          groups = TRUE,
          columns = starts_with("TB"),
          missing_text = "--",
          fns = list("T" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        grand_summary_rows(
          columns = starts_with("TB"),
          missing_text = "--",
          fns = list("GT" = ~sum(., na.rm = T)),
          formatter = fmt_number,
          decimals = 0
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = agecoarse, rows = everything())
        ) %>%
        tab_options(
          heading.title.font.weight = "bold",
          column_labels.font.weight = "bold",
          summary_row.background.color = grey10k,
          grand_summary_row.background.color = grey20k
        ) %>%
        gtsave(filename = file.path(dir_graphics,
                                    paste0("Nigeria - FY23 ",
                                           name, " - ", code, "-",
                                           " Targets TB.png")))

    })
