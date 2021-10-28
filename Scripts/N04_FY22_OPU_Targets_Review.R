##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: COP22 Prep
##  LICENCE: MIT
##  DATE:    2021-10-22
##  UPDATED: 2021-10-25


# PACKAGES ----

library(tidyverse)
library(readxl)
library(gophr)
library(glitr)
library(glamr)
library(gisr)
library(janitor)
library(gt)

# GLOBAL ----

  # DIR - Global ----

    dir_merdata <- glamr::si_path("path_msd")
    dir_targets <- "../../PEPFAR/COUNTRIES/Nigeria/DataPack"

  # DIR - Project ----

    dir_data <- "Data"
    dir_dataout <- "Dataout"
    dir_gis <- "GIS"
    dir_graphics <- "Graphics"

  # Files ----

    file_msd_psnu <- return_latest(
      folderpath = dir_merdata,
      pattern = "PSNU_IM.*_N.*"
    )

    file_msd_nat <- return_latest(
      folderpath = dir_merdata,
      pattern = "NAT_SUBNAT.*"
    )

# Country name ----

  # Country name
  cntry <- "Nigeria"

  # Agency
  agency <- "USAID"

# DATA ----

  # Pop and HIV ----
  df_nat <- file_msd_nat %>% read_msd()

  df_nat %>% glimpse()

  df_nat %>% distinct(indicator) %>% prinf()

  df_pops <- df_nat %>%
    filter(operatingunit == "Nigeria",
           indicator %in% c("POP_EST", "PLHIV"),
           standardizeddisaggregate == "Total Numerator") %>%
    select(fiscal_year, psnuuid, psnu, indicator, value = targets) %>%
    arrange(fiscal_year, desc(indicator)) %>%
    pivot_wider(names_from = indicator, values_from = value)


  # Red states ----
  df_states <- df_nat %>%
    filter(operatingunit == "Nigeria",
           snuprioritization != "Missing") %>%
    select(psnuuid, psnu, snuprioritization) %>%
    distinct() %>%
    mutate(
      flag = case_when(
        psnu %in% c("Akwa Ibom", "Rivers", "Delta", "Lagos", "Enugu", "Imo") ~ "Red",
        psnu %in% c("Gombe", "Nasarawa", "Benue") ~ "Green",
        psnu %in% c("Abia", "Taraba", "_Military Nigeria") ~ NA_character_,
        TRUE ~ "Yellow"
      ))

  # Pops & States
  df_cntry <- df_states %>%
    left_join(df_pops %>% filter(fiscal_year == 2022),
              by = c("psnuuid", "psnu")) %>%
    select(-fiscal_year) %>%
    arrange(flag, desc(PLHIV), psnu)

  # PSNU TX
  df_psnu <- file_msd_psnu %>% read_msd()

  df_psnu %<>% clean_agency()
  df_psnu %>% distinct(fiscal_year)
  df_psnu %>% distinct(fundingagency)

  curr_fy <- df_psnu %>% identifypd(pd_type = "year")

  #inds <- c("TX_CURR", "TX_NEW", "TX_NET_NEW")
  inds <- c("TX_CURR", "TX_NEW")

  # Extract history ----
  df_targets <- df_psnu %>%
    filter(fiscal_year %in% 2020:2022,
           fundingagency != "DEDUP",
           indicator %in% inds,
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, psnuuid, psnu, fundingagency, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    pivot_longer(cols = c(starts_with("qtr"), cumulative, targets),
                 names_to = "period", values_to = "value") %>%
    mutate(
      period = case_when(
        period %in% c("targets", "cumulative") ~ paste0("FY", str_sub(fiscal_year, 3,4), str_to_sentence(period)),
        TRUE ~ paste0("FY", str_sub(fiscal_year, 3,4), "Q", str_sub(period, 4))
      )
    ) %>%
    filter(
      fiscal_year == 2022 & period == "FY22Targets" |
      str_detect(period, "FY20|FY21")
    ) %>%
    select(-fiscal_year) %>%
    pivot_wider(names_from = period, values_from = value) %>%
    filter(!is.na(FY22Targets))

  # Add states context ----
  df_targets <- df_cntry %>%
    left_join(df_targets, by = c("psnuuid", "psnu")) %>%
    relocate(fundingagency, .after = psnu)

  # Merge Lagos data ----
  df_targets <- df_targets %>%
    filter(psnu == "Lagos") %>%
    group_by(psnuuid, psnu, snuprioritization, flag, POP_EST, PLHIV, indicator) %>%
    summarise(across(starts_with("FY"), sum, na.rm = T), .groups = "drop") %>%
    ungroup() %>%
    mutate(fundingagency = "USAID & CDC") %>%
    relocate(fundingagency, .after = psnu) %>%
    bind_rows(df_targets) %>%
    filter(!(psnu == "Lagos" & fundingagency %in% c("USAID", "CDC"))) %>%
    mutate(FY21Q4 = NA_integer_) %>%
    arrange(flag, desc(PLHIV), psnu) %>%
    filter(!is.na(FY22Targets))


  # Calculate Performance ----
  df_targets <- df_targets %>%
    mutate(
      FY20Achievement = FY20Cumulative / FY20Targets,
      FY21Achievement = FY21Cumulative / FY21Targets
    ) %>%
    relocate(FY20Achievement, .after = FY20Targets) %>%
    relocate(FY21Achievement, .after = FY21Targets) %>%
    pivot_wider(names_from = indicator, values_from = starts_with("FY")) %>%
    select(psnuuid, psnu, fundingagency, snuprioritization, flag, POP_EST, PLHIV,
           ends_with("TX_CURR"), ends_with("TX_NEW"))

# VIZ ----

  df_tx_curr <- df_targets %>%
    select(-ends_with("TX_NEW")) %>%
    select(-matches("FY20Q\\d{1}_TX_CURR")) %>%
    mutate(
      FY21RIncrease_TX_CURR = (FY21Cumulative_TX_CURR - FY20Cumulative_TX_CURR) / FY20Cumulative_TX_CURR,
      FY21PIncrease_TX_CURR = FY21Achievement_TX_CURR - FY20Achievement_TX_CURR,
      FY22TIncrease_TX_CURR = (FY22Targets_TX_CURR - FY21Targets_TX_CURR) / FY21Targets_TX_CURR,
      FY22Coverage_TX_CURR = FY22Targets_TX_CURR / PLHIV
    ) %>%
    relocate(FY21RIncrease_TX_CURR, .after = FY21Cumulative_TX_CURR)


  viz_targets <- df_tx_curr %>%
    mutate(fundingagency = factor(
      fundingagency,
      levels = c("USAID", "USAID & CDC", "CDC", "DOD"),
      ordered = TRUE)) %>%
    arrange(fundingagency) %>%
    group_by(fundingagency) %>%
    gt() %>%
    tab_header(
      title = "NIGERIA - PEPFAR TX_CURR PERFORMANCE & TARGETS",
      subtitle = "FY22 TX_CURR Targets Adjustments based on past/current performance"
    ) %>%
    tab_spanner(
      label = "2020 Performance",
      columns = starts_with("FY20")
    ) %>%
    tab_spanner(
      label = "2021 Performance",
      columns = starts_with("FY21")
    ) %>%
    tab_spanner(
      label = "2022 Targets",
      columns = starts_with("FY22")
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold", transform = "uppercase")),
      locations = list(cells_title(groups = "title"), cells_row_groups())
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold", transform = "uppercase")),
        locations = cells_column_spanners(spanners = tidyselect::everything())
    ) %>%
    tab_style(
      style = list(cell_text(transform = "uppercase")),
      locations = cells_column_labels(columns = tidyselect::everything())
    ) %>%
    cols_hide(
      columns = c("psnuuid", "snuprioritization", "flag", "POP_EST", matches("FY\\d{2}Q\\d{1}"))
    ) %>%
    cols_label(
      psnuuid = "UID",
      psnu = "State",
      fundingagency = "Agency",
      snuprioritization = "SNU Prioritization",
      flag = "Programing Status",
      POP_EST = "Population",
      PLHIV = "PLHIV",
      FY20Cumulative_TX_CURR = "Results",
      FY20Targets_TX_CURR = "Targets",
      FY20Achievement_TX_CURR = "Achv",
      FY21Q1_TX_CURR = "Q1",
      FY21Q2_TX_CURR = "Q2",
      FY21Q3_TX_CURR = "Q3",
      FY21Q4_TX_CURR = "Q4",
      FY21Cumulative_TX_CURR = "Results",
      FY21Targets_TX_CURR = "Targets",
      FY21Achievement_TX_CURR = "Achv",
      FY21PIncrease_TX_CURR = "Change**",
      FY21RIncrease_TX_CURR = "Increase*",
      FY22Targets_TX_CURR = "Targets",
      FY22TIncrease_TX_CURR = "Change***",
      FY22Coverage_TX_CURR = "Coverage"
    ) %>%
    fmt_missing(
      columns = tidyselect::everything(),
      missing_text = "-"
    ) %>%
    fmt_percent(
      columns = matches("Achievement|Coverage|RIncrease|PIncrease|TIncrease"),
      decimal = 0
    ) %>%
    fmt_number(
      columns = matches("PLHIV|Q|Results|Cumulative|Targets"),
      decimal = 0
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = usaid_red),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_body(
        columns = psnu,
        rows = flag == "Red"
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = genoa),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_body(
        columns = psnu,
        rows = flag == "Green"
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = golden_sand),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_body(
        columns = psnu,
        rows = flag == "Yellow"
      )
    ) %>%
    tab_source_note(
      source_note = md("**INCREASE***: % Increase of Cumulative Results from previous fiscal year")
    ) %>%
    tab_source_note(
      source_note = md("**CHANGE****: % Change in performance from previous fiscal year")
    ) %>%
    tab_source_note(
      source_note = md("**CHANGE***: % Change in targets from previous fiscal year")
    ) %>%
    tab_options(
      source_notes.font.size = 8,
      table.font.size = 12,
      data_row.padding = gt::px(5),
      source_notes.padding = gt::px(1))


  viz_targets %>%
    gtsave(filename = paste0(dir_graphics,
                             "/Nigeria_FY22_Targets_Adjustment.png"))


