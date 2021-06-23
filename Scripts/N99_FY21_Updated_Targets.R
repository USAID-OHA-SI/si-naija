##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: Update TX Targets
##  LICENCE: MIT
##  DATE:    2021-02-05
##  UPDATED: 2021-06-21


# PACKAGES -------------------------------------------------

library(tidyverse)
library(ggrepel)
library(readxl)
library(glitr)
library(scales)
library(glamr)
library(gisr)
library(janitor)
library(extrafont)
library(ICPIutilities)

# GLOBAL --------------------------------------------------

  # Load configs
  source("./Scripts/N00_Config.R")

  # Country name
  country <- "Nigeria"

  # file - COP19 OPU
  file_fy20_targets <- return_latest(
      folderpath = dir_data,
      pattern = "^Site Tool_Nig.*_\\d{14}_F.*.xlsx$")

  # file - COP20 OPU
  file_fy21_usaid_targets <- return_latest(
    folderpath = dir_data,
    pattern = "FY21 DATIM.*Nov 2020.xlsx$")

  file_fy21_cdc_targets <- return_latest(
    folderpath = dir_data,
    pattern = "Nigeria COP20.*.CDC.*.xlsx$"
  )

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

  # MSD PSNU ----

  df_msd <- file_msd_psnu %>%
    read_msd() %>%
    reshape_msd(clean = TRUE)

  df_msd %>% glimpse()

  df_msd %>% distinct(period)

  # MSD PSNU Targets
  df_msd_psnu_trgts <- df_msd %>%
    clean_agency() %>%
    filter(operatingunit == country,
           fundingagency %in% c("USAID", "CDC"),
           period %in% c("FY20", "FY21"),
           period_type  == "targets",
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(fundingagency, snu1, psnu, indicator, period) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = period, values_from = value)

  df_msd_psnu_trgts %>% glimpse()

  # Check HTS
  df_msd %>%
    clean_agency() %>%
    filter(operatingunit == country,
           fundingagency %in% c("USAID", "CDC"),
           period %in% c("FY20", "FY21"),
           period_type %in% c("targets","cumulative"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(fundingagency, snu1, psnu, indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = period_type, values_from = val) %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW")) %>%
    mutate(achive = cumulative / targets * 100) %>%
    View(title = "State Results")

  df_msd_fy21_targets <- df_msd %>%
    clean_agency() %>%
    filter(operatingunit == country,
           fundingagency %in% c("USAID"),
           period %in% c("FY21"),
           period_type %in% c("targets"),
           #standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(fundingagency, psnu, indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW"))

  # Check HTS
  df_msd %>%
    clean_agency() %>%
    filter(operatingunit == country,
           fundingagency %in% c("USAID", "CDC"),
           period %in% c("FY20", "FY21"),
           period_type %in% c("targets","cumulative"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(fundingagency, indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = period_type, values_from = val) %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW")) %>%
    mutate(achive = cumulative / targets * 100) %>%
    View(title = "Country Results")

  # MSD IM Targets
  df_msd_im_trgts <- df_msd %>%
    filter(operatingunit == country,
           fundingagency %in% c("USAID", "CDC"),
           #indicator %in% c("TX_CURR", "TX_NEW"),
           period %in% c("FY20", "FY21"),
           period_type == "targets",
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(fundingagency, mech_code, mech_name,
             primepartner, indicator, period) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = period, values_from = val)

  df_msd_im_trgts %>% glimpse()


  # MSD Sites ----

  df_msd_sites <- file_msd_sites %>%
    read_msd() %>%
    reshape_msd(clean = TRUE)

  df_msd_sites %>% glimpse()

  df_msd_sites <- df_msd_sites %>%
    filter(str_to_lower(fundingagency) != "dedup")

  df_msd_sites %>% glimpse()

  # IMs
  df_ims <- df_msd_sites %>%
    clean_agency() %>%
    mutate(
      mech_name = case_when(
        # USAID
        mech_name == "Meeting Targets and Maintaining Epidemic Control (EpiC)" ~ "EpiC",
        mech_name == "STRENGHTENING INTERGRATED DELIVERY OF HIV/AIDS SERVICES(SIDHAS)" ~ "SIDHAS",
        mech_name == "SHARP Task Order 1" ~ "SHARP TO1",
        mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 2" ~ "SHARP TO2",
        mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 3" ~ "SHARP TO3",
        mech_name == "Reaching Impact, Saturation and Epidemic Control (RISE)" ~ "RISE",
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
      ),
      primepartner = case_when(
        # USAID
        primepartner == "Family Health International" ~ "FHI360",
        primepartner == "Chemonics International, Inc." ~ "Chemonics",
        primepartner == "JHPIEGO CORPORATION" ~ "JHPIEGO",
        primepartner == "SOCIETY FOR FAMILY HEALTH" ~ "SFH",
        primepartner == "HEARTLAND ALLIANCE LTD-GTE" ~ "HAN",
        primepartner == "CENTER FOR CLINICAL CARE AND CLINICAL RESEARCH LTD GTE" ~ "C4C3R",
        primepartner == "ASSOCIATION FOR REPRODUCTIVE AND FAMILY HEALTH" ~ "A4RnFH",
        primepartner == "PRO-HEALTH INTERNATIONAL" ~ "ProHI",
        # CDC
        primepartner == "APIN PUBLIC HEALTH INITIATIVES LTD/GTE" ~ "APHI",
        primepartner == "INSTITUTE OF HUMAN VIROLOGY" ~ "IHVN",
        primepartner == "CATHOLIC CARITAS FOUNDATION OF NIGERIA" ~ "CCFN",
        primepartner == "CENTRE FOR INTEGRATED HEALTH PROGRAMS" ~ "CIHP",
        TRUE ~ primepartner
      ),
      mech_label = paste0(primepartner, " ", mech_name)
    ) %>%
    filter(period_type == "targets",
           period %in% c("FY20", "FY21"),
           standardizeddisaggregate == "Total Numerator",
           str_to_lower(fundingagency) != "dedup") %>%
    group_by(period, fundingagency, mech_code, mech_name,
             primepartner, mech_label, indicator) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(period, fundingagency, primepartner)

  df_ims %>% prinf()
  df_ims %>% view()

  df_ims %>%
    filter(
      period == "FY21",
      indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")) %>%
    group_by(period, fundingagency, mech_code, mech_name,
             primepartner, mech_label, indicator) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = "indicator",
                values_from = "val") %>% view()




  # FY20 Updated targets ----

  df_targets <- file_fy20_targets %>%
    read_excel(sheet = "TX", skip = 4) %>%
    clean_names() %>%
    rename(tx_curr_n = tx_curr_n_age_sex_hiv_status_20t,
           tx_new_n = tx_new_n_age_sex_hiv_status_20t,
           tx_pvls_n = tx_pvls_n_age_sex_indication_hiv_status_20t_routine,
           tx_pvls_d = tx_pvls_d_age_sex_indication_hiv_status_20t_routine)

  df_targets %>% glimpse()

  df_trgts <- df_targets %>%
    separate(site, into = c("psnu", "facility"), sep = " > ", remove = FALSE) %>%
    mutate(site_type = str_extract(site, " (?<=\\[#).*(?=\\] )"),
           facility = if_else(is.na(facility), psnu, facility),
           psnu = if_else(str_detect(psnu, "_Mil"), "_Military Nigeria", psnu)) %>%
    separate(facility, into = c("facility", NA, "orgunituid"), sep = " \\[") %>%
    mutate_at(vars(facility, orgunituid), str_remove, pattern = "\\]") %>%
    mutate(mechanism = str_replace(mechanism, " - ", " -- ")) %>%
    separate(mechanism, into = c("mech_code", "partner"), sep = " -- ", remove = F) %>%
    mutate(mechanism = str_replace(mechanism, " -- ", " - ")) %>%
    relocate(site_type, .after = site) %>%
    pivot_longer(cols = starts_with("tx_"),
                 names_to = "indicator",
                 values_to = "val") %>%
    mutate(denominatornumerator = str_sub(indicator, -1),
           denominatornumerator = str_to_upper(denominatornumerator)) %>%
    mutate(indicator = str_sub(indicator, 1, -3),
           indicator = str_to_upper(indicator),
           period_type = "cop19") %>%
    relocate(val, .after = last_col())


  df_trgts %>% glimpse()

  View(df_trgts)

  # PSNU targets
  df_psnu_trgts <- df_trgts %>%
    filter(status == "Active") %>%
    group_by(psnu, indicator, period_type, denominatornumerator) %>%
    summarise_at(vars(val), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           denominatornumerator == "N") %>%
    select(-denominatornumerator) %>%
    bind_rows(df_msd_psnu_trgts) %>%
    mutate(indicator = paste0(indicator, "_", period_type),
           indicator = str_to_lower(indicator)) %>%
    select(-period_type) %>%
    pivot_wider(names_from = indicator,
                values_from = val) %>%
    mutate(
      tx_curr_ach1 = round(tx_curr_cumulative / tx_curr_targets * 100),
      tx_curr_ach2 = round(tx_curr_cumulative / tx_curr_cop19 * 100),
      tx_new_ach1 = round(tx_new_cumulative / tx_new_targets * 100),
      tx_new_ach2 = round(tx_new_cumulative / tx_new_cop19 * 100)
    ) %>%
    relocate(tx_curr_cumulative, tx_curr_targets, tx_curr_cop19,
             tx_curr_ach1, tx_curr_ach2,
             tx_new_cumulative, tx_new_targets, tx_new_cop19,
             tx_new_ach1, tx_new_ach2,
             .after = psnu)

  df_psnu_trgts %>% glimpse()

  df_psnu_trgts %>% prinf()

  #df_psnu_trgts %>% View()


  # FY21 USAID Targets ----

  file_fy21_usaid_targets %>%
    read_excel(sheet = 3, skip = 4) %>%
    view(title = "FY21 - Targets")

  df_fy21_usaid_targets <- file_fy21_targets %>%
    read_excel(sheet = 3, skip = 4)

  df_fy21_usaid_targets <- df_fy21_usaid_targets %>%
    pivot_longer(cols = -c(State, Mech, `Partner Name`),
                 names_to = "indicator",
                 values_to = "val") %>%
    clean_names() %>%
    rename(primepartner = partner_name)

  df_fy21_usaid_targets %>% glimpse()

  df_fy21_usaid_targets %>% view()

  df_fy21_usaid_targets %>%
    distinct(indicator) %>%
    prinf()

  df_fy21_usaid_targets <- df_fy21_usaid_targets %>%
    mutate(
      indicator = case_when(
        str_detect(indicator, " HTS_TST$") ~
          paste0("HTS_TST (", str_remove(indicator, "  HTS_TST"), ")"),
        str_detect(indicator, " HTS_TST_POS$") ~
          paste0("HTS_TST_POS (", str_remove(indicator, "  HTS_TST_POS"), ")"),
        indicator == "HTS_POS" ~ "HTS_TST_POS",
        TRUE ~ indicator
      ),
      disaggregate = if_else(str_detect(indicator, "\\("),
                             extract_text(indicator, "()"),
                             NA_character_),
      modality = if_else(!str_detect(disaggregate, ", "),
                         disaggregate,
                         NA_character_),
      disaggregate = if_else(str_detect(disaggregate, ", "),
                             disaggregate,
                             NA_character_)
    ) %>%
    rowwise() %>%
    mutate(
      mech_code = if_else(str_detect(mech, "NGA"),
                          str_split(mech, " - ") %>% unlist() %>% nth(2),
                          "TBD"),
      mech_name = if_else(str_detect(mech, "NGA"),
                          str_split(mech, " - ") %>% unlist() %>% last(),
                          mech)
    ) %>%
    ungroup() %>%
    separate(disaggregate,
             into = c("numeratordenom",
                      "indicatortype",
                      "standardizeddisaggregate"),
             sep = ", ") %>%
    separate(modality,
             into = c("site_type", "modality"),
             sep = " - ") %>%
    relocate(val, .after = last_col()) %>%
    relocate(mech_code, mech_name, .after = mech)

  df_fy21_usaid_targets %>%
    view(title = "FY21 - Long Clean")

  df_fy21_usaid_targets <- df_fy21_usaid_targets %>%
    mutate(indicator = str_to_upper(indicator),
           val = as.integer(round(val))) %>%
    filter(indicator %in% c("HTS_TST", "HTS_POS", "HTS_TST_POS", "TX_NEW", "TX_CURR")) %>%
    group_by(state, indicator) %>%
    summarise(val = sum(val, na.rm = T)) %>%
    ungroup() %>%
    rename(updated_val = val) %>%
    left_join(df_msd_fy21_targets, by = c("state" = "psnu", "indicator" = "indicator")) %>%
    select(psnu = state, indicator, val, updated_val) %>%
    mutate(changed_val = updated_val - val,
           increased_val = if_else(val < updated_val,
                                   "Increase", "Decrease"))


  ## Slopes

  df_fy21_usaid_targets %>%
    distinct(indicator) %>%
    pull() %>%
    #nth(2) %>%
    #first() %>%
    map(.x, .f = ~plot_change(
      df = df_fy21_usaid_targets,
      agency = "USAID",
      indicator = .x,
      save = F
    ))

  # FY21 CDC Targets ----

  file_fy21_cdc_targets %>%
    read_excel(sheet = 2, skip = 2) %>%
    view(title = "CDC/FY21 - Targets")

  df_fy21_cdc_targets <- file_fy21_cdc_targets %>%
    read_excel(sheet = 2, skip = 2) %>%
    clean_names() %>%
    select(indicator:final_adjusted_target) %>%
    rename_with(~str_remove(., "_\\d{1}$")) %>%
    rename(mech_code = im,
           mech_name = im_name,
           primepartner = partner_name,
           val = current_target,
           updated_val = final_adjusted_target) %>%
    filter(!is.na(psnu)) %>%
    fill(indicator) %>%
    mutate_at(vars(ends_with("val")), as.integer) %>%
    mutate(mech_label = paste0(primepartner, " - ", mech_name),
           changed_val = updated_val - val,
           increased_val = if_else(val < updated_val,
                                   "Increase", "Decrease")) %>%
    relocate(psnu, mech_code, mech_name, primepartner, mech_label,
             .before = indicator)

  df_fy21_cdc_targets %>% glimpse()
  df_fy21_cdc_targets %>% View()


  ## Slopes

  df_fy21_cdc_targets %>%
    distinct(indicator) %>%
    pull() %>%
    map(.x, .f = ~plot_change(
      df = df_fy21_cdc_targets,
      agency = "CDC",
      indicator = .x,
      save = TRUE
    ))


