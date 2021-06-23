##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: COP20 OPU Approval
##  LICENCE: MIT
##  DATE:    2021-06-21


# PACKAGES -------------------------------------------------

  library(tidyverse)
  library(ggrepel)
  library(readxl)
  library(tameDP)
  library(glitr)
  library(scales)
  library(glamr)
  library(gisr)
  library(janitor)
  library(extrafont)
  library(ICPIutilities)
  library(gt)

# GLOBAL --------------------------------------------------

  # Load configs
  source("./Scripts/N00_Config.R")

  # Country name
  country <- "Nigeria"

  # file - COP20 OPU
  file_cop20_targets <- return_latest(
    folderpath = dir_data,
    pattern = "Updated OPU Data Pack_Nigeria_20210609_Final_v9.xlsx$")

  # Latest MSD PSNU x IM File
  file_msd_nat <- return_latest(
    folderpath = dir_merdata,
    pattern = "MER_S.*_NAT_SUBNAT_FY15-21_\\d{8}_v.*.zip")

  # Latest MSD PSNU x IM File
  file_msd_psnu <- return_latest(
    folderpath = dir_merdata,
    pattern = "MER_S.*_PSNU_IM_.*_\\d{8}_v.*_N.*.zip")

  # Latest MSD Site x IM File
  file_msd_sites <- return_latest(
    folderpath = dir_merdata,
    pattern = "MER_S.*_Site_IM_.*_\\d{8}_v.*_N.*.zip")

# FUNCTIONS ------------------------------------------------

#' OPU Target Change
plot_change <- function(df,
                        agency = "USAID",
                        indicator = "HTS_TST",
                        save = FALSE) {

  # Params
  agency <- {{agency}}
  ind <- {{indicator}}

  # Filter & Plot
  plot <- df %>%
    filter(indicator == ind) %>%
    ggplot() +
    geom_segment(
      aes(x = 1, xend = 2,
          y = val, yend = updated_val,
          color = increased_val),
      size = 1,
      show.legend = F) +
    geom_vline(xintercept = 1,
               linetype = "dashed",
               color = grey50k) +
    geom_vline(xintercept = 2,
               linetype = "dashed",
               color = grey50k) +
    geom_text(aes(x = 1, y = 1.1 * max(val, updated_val)),
              label = "Current Target",
              size = 4, hjust = 1.1) +
    geom_text(aes(x = 2, y = 1.1 * max(val, updated_val)),
              label = "*Amanded Target",
              size = 4, hjust = -0.1) +
    geom_text_repel(
      aes(x = 1, y = val,
          label = paste0(psnu, " (", comma(val), ")")),
      size = 3, hjust = 1.1) +
    geom_text(
      aes(x = 2, y = updated_val,
          label = paste0(psnu, " (", comma(updated_val), ")")),
      size = 3, hjust = -0.1) +
    scale_color_manual(
      labels = c("Decrease", "Increase"),
      values = c("Decrease" = usaid_red, "Increase" = usaid_blue)) +
    xlim(.5, 2.5) +
    labs(title = paste0(agency, "/NIGERIA - FY21 ", ind, " TARGETS"),
         subtitle = "* OPU for COP20 has not yet been approved",
         caption = paste0("Produced by OHA/SIEI on ",
                          format(Sys.Date(), "%Y%m%d"),
                          ", Source: ", agency,
                          "/NIGERIA"),
         x = "", y = "") +
    si_style_void() +
    theme(text = element_text(family = "Source Sans Pro"),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5),
          plot.caption = element_text(hjust = .5))

  if (save == TRUE) {
    print(plot)

    si_save(
      filename = paste0("./Graphics/NIGERIA_FY21_OPU_", agency,
                        "_", indicator,
                        "_Targets_",
                        format(Sys.Date(), "%Y%m%d"),
                        ".png"),
      dpi = 320)
  }

  return(plot)
}


# DATA ----------------------------------------------------

  # MSD Data

  # MSD NAT

  df_nat <- file_msd_nat %>% read_msd()

  df_plhiv <- df_nat %>%
    filter(operatingunit == country,
           fiscal_year %in% c(2020, 2021),
           indicator == 'PLHIV',
           standardizeddisaggregate == 'Total Numerator') %>%
    select(fiscal_year, psnu, psnuuid, snuprioritization, indicator, targets)

  # MSD PSNU ----

  df_msd <- file_msd_psnu %>% read_msd()

  df_msd %>% glimpse()

  df_msd %>% distinct(indicator, standardizeddisaggregate)

  df_tx_curr <- df_msd %>%
    filter(operatingunit == country,
           fiscal_year %in% c(2020, 2021),
           indicator == 'TX_CURR',
           standardizeddisaggregate == 'Total Numerator') %>%
    group_by(fiscal_year, psnu, psnuuid, indicator) %>%
    summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>%
    ungroup()


  # OPU Data ----

  #df_opu <- tame_dp(file_cop20_targets)

  df_opu <- import_dp(filepath = file_cop20_targets)

  df_opu %>% glimpse()

  #df_dp2 <- reshape_dp(df_dp)

  key_cols <- c("psnu", "indicator_code", "age", "sex", "keypop")

  mechs <- df_opu %>%
    select(matches("^(1|2|3|4|5|6|7|8|9).")) %>%
    names()

  df_dp <- df_opu %>%
    select(key_cols, mechs) %>%
    tidyr::pivot_longer(-key_cols,
                        names_to = c("mech_code", "indicatortype"),
                        names_sep = "_") %>%
    dplyr::mutate(indicatortype = toupper(indicatortype)) %>%
    dplyr::filter_at(vars(value), any_vars(!is.na(.))) %>%
    mutate(across(value, as.numeric)) %>%
    dplyr::mutate(psnu = str_remove_all(psnu, " \\[#(Country|SNU|DREAMS|Military)]")) %>%
    tidyr::separate(col = psnu,
                    into = c("psnu", "psnuuid", NA),
                    sep = " \\[|]")

  #df_dp%>% clean_indicators()
  df_dp <- df_dp %>%
    separate(indicator_code,
             c("indicator", "numeratordenom", "disaggregate", NA, "otherdisaggregate"),
             sep = "\\.", fill = "right") %>%
    mutate(statushiv = case_when(
        otherdisaggregate %in% c("NewPos", "KnownPos", "Positive") ~ "Positive",
        otherdisaggregate %in% c("NewNeg", "Negative")             ~ "Negative",
        otherdisaggregate == "Unknown"                             ~ "Unknown"
      ),
      otherdisaggregate = ifelse(str_detect(indicator, "STAT") &
                                 otherdisaggregate %in% c("NewPos", "Positive",
                                                          "NewNeg", "Negative",
                                                          "Unknown"),
                                 as.character(NA),
                                 otherdisaggregate)) %>%
    mutate(disaggregate = str_replace_all(disaggregate, "_", "/"),
           disaggregate = ifelse(disaggregate == "total",
                                 "Total Numerator",
                                 disaggregate))

  #convert external modalities
  #df_dp <- df_dp %>% convert_mods()

  df_mods <- df_dp %>%
    mutate(modality = case_when(str_detect(indicator, "HTS_(TST|RECENT).") ~
                                str_remove(indicator, "HTS_(TST|RECENT)_")),
           modality = ifelse(modality == "PMTCTPostANC1", "Post ANC1", modality),
           indicator = case_when(str_detect(indicator, "HTS_TST.") ~ "HTS_TST",
                                 str_detect(indicator, "HTS_RECENT.") ~ "HTS_RECENT",
                                 TRUE ~ indicator))

  #create index modalities & rename HTS
  df_index <- df_mods %>%
    filter(indicator %in% c("HTS_INDEX_COM", "HTS_INDEX_FAC")) %>%
    mutate(modality = case_when(
      indicator == "HTS_INDEX_COM" ~ "IndexMod",
      indicator == "HTS_INDEX_FAC" ~ "Index"),
      indicator = "HTS_TST")

  #filter to indicators which feed into HTS_TST
  df_exmod <- df_mods %>%
    dplyr::filter(indicator %in% c("PMTCT_STAT", "TB_STAT", "VMMC_CIRC"),
                  statushiv %in% c("Negative", "Positive"),
                  otherdisaggregate %in% c("NewNeg", "NewPos", NA))

  #convert -> map modality & change rest to match HTS_TST
  df_exmod <- df_exmod %>%
    mutate(modality = case_when(
        indicator == "VMMC_CIRC"  ~ "VMMC",
        indicator == "TB_STAT"    ~ "TBClinic",
        indicator == "PMTCT_STAT" ~ "PMTCT ANC"
      ),
      indicator = "HTS_TST",
      disaggregate = "Age/Sex/Result",
      otherdisaggregate = as.character(NA))

  #binding onto main data frame
  df_dp <- dplyr::bind_rows(df_mods, df_index, df_exmod)

  #add HTS_TST_POS as an indicator
  df_dp <- df_dp %>%
    filter(indicator == "HTS_TST" & statushiv == "Positive") %>%
    mutate(indicator = "HTS_TST_POS") %>%
    bind_rows(df_dp, .)

  #rename keypop
  df_dp <- df_dp %>% rename(population = keypop)

  #move keypop to otherdisagg
  df_dp <- df_dp %>%
    mutate(fiscal_year = 2021,
           agecoarse = case_when(
             age %in% c("<2 Months","2-12 Months", "<01", "<1",
                        "01-04","05-09", "<10", "10-14", "<15") ~ "<15",
             age %in% c("15-17", "15-19", "15+", "18+", "20-24",
                        "25-29", "30-34", "35-39", "40-44", "45-49", "50+") ~ "15+",
             TRUE ~ age
           )) %>%
    relocate(agecoarse, .after = age) %>%
    select(fiscal_year, everything())

  #Fill in the mechs
  df_dp <- df_dp %>%
    get_names(map_names = TRUE)


## TARGETS ----

  df_dp %>%
    distinct(age) %>%
    arrange(age) %>%
    pull()

  df_dp %>%
    distinct(agecoarse)

  # State level Summary
  df_dp %>%
    group_by(psnu, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(indicator == 'TX_CURR',
           psnu %in% c("Delta", "Enugu", "Gombe", "Lagos")) %>%
    rename(targets = value) %>%
    gt()

  # Indicators / age coarse
  df_dp %>%
    filter(agecoarse %in% c("<15", "15+")) %>%
    distinct(indicator) %>%
    arrange(indicator) %>%
    pull()

  # Priotization table
  df_dp %>%
    filter(agecoarse %in% c("<15", "15+")) %>%
    mutate(indicator = if_else(str_detect(indicator, "_COM$|_FAC$"),
                               str_remove(indicator, "_COM$|_FAC$"),
                               indicator)) %>%
    left_join(df_plhiv %>%
                filter(fiscal_year == 2021) %>%
                distinct(psnuuid, snuprioritization),
              by = "psnuuid") %>%
    group_by(indicator, agecoarse, snuprioritization) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(snuprioritization = ifelse(is.na(snuprioritization), "No Prioritization", snuprioritization)) %>%
    group_by(indicator, agecoarse) %>%
    mutate(Total = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = snuprioritization, values_from = value) %>%
    relocate(Total, .after = last_col()) %>%
    group_by(indicator) %>%
    gt() %>%
    fmt_number(columns = 3:7, decimals = 0, rows = everything()) %>%
    summary_rows(
      groups = TRUE,
      columns = 3:7,
      fns = list(total = ~sum(.)),
      formatter = fmt_number,
      decimals = 0
    ) %>%
    tab_header(title = "Nigeria - Proposed COP20 Prioritization") %>%
    gtsave("Images/Nigeria_Proposed_COP20_Prioritization.png")

  # HTS_INDEX to TX_PVLS
  df_dp %>%
    mutate(indicator = if_else(str_detect(indicator, "_COM$|_FAC$"),
                               str_remove(indicator, "_COM$|_FAC$"),
                               indicator)) %>%
    filter(indicator %in% c("HTS_INDEX", "HTS_TST", "HTS_TST_POS",
                            "TX_NEW", "TX_CURR", "TX_PVLS"),
           agecoarse %in% c("<15", "15+")) %>%
    group_by(fundingagency, primepartner, indicator, agecoarse) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    group_by(fundingagency, primepartner) %>%
    gt() %>%
    fmt_number(columns = 4:9, decimals = 0, rows = everything()) %>%
    summary_rows(
      groups = TRUE,
      columns = 4:9,
      fns = list(total = ~sum(., na.rm = T)),
      formatter = fmt_number,
      decimals = 0
    ) %>%
    tab_header(title = "Nigeria - Proposed COP20 for HTS_INDEX to TX_PHVS") %>%
    gtsave("Images/Nigeria_Proposed_COP20_for HTS_INDEX to TX_PHVS.png")


  # CXCA_SCRN to PMTCT
  df_dp %>%
    mutate(
      agecoarse = case_when(
        indicator %in% c("CXCA_SCRN", "PMTCT_EID") ~ "Total",
        indicator == "OVC_SERV" & age %in% c("<01", "01-04", "05-09", "10-14", "15-17") ~ "<18",
        indicator == "OVC_SERV" & age == "18+" ~ "18+",
        TRUE ~ agecoarse
      )
    ) %>%
    filter(indicator %in% c("CXCA_SCRN", "OVC_SERV", "OVC_HIVSTAT",
                            "PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_ART", "PMTCT_EID"),
           agecoarse %in% c("<15", "15+", "<18", "18+", "Total")) %>%
    group_by(fundingagency, primepartner, indicator, agecoarse) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    group_by(fundingagency, primepartner) %>% #view()
    gt() %>%
    fmt_number(columns = 4:8, decimals = 0) %>%
    summary_rows(
      groups = TRUE,
      columns = 4:7,
      fns = list(total = ~sum(., na.rm = T)),
      formatter = fmt_number,
      decimals = 0
    ) %>%
    tab_header(title = "Nigeria - Proposed COP20 for CXCA_SCRN to PMTCT") %>%
    gtsave("Images/Nigeria_Proposed_COP20_for CXCA_SCRN to PMTCT.png")




