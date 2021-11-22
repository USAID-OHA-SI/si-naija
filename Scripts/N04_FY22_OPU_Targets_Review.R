##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: COP22 Prep - Review of Historical data
##  LICENCE: MIT
##  DATE:    2021-10-22
##  UPDATED: 2021-11-22

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

    file_cop21_net_new <- return_latest(
      folderpath = dir_targets,
      pattern = "UPDATED_ COP 21 Projection_with "
    )

# Country name ----

  # Country name
  cntry <- "Nigeria"

  # Agency
  agency <- "USAID"

# DATA ----

  # PSNU x IM
  df_psnu <- file_msd_psnu %>% read_msd()

  df_psnu <- df_psnu %>% clean_agency()

  curr_fy <- df_psnu %>% identifypd(pd_type = "year")
  curr_pd <- df_psnu %>% identifypd(pd_type = "full")

  # Pop and HIV ----
  df_nat <- file_msd_nat %>% read_msd()

  # COP21 Extimated NET_NEW
  df_net_new <- file_cop21_net_new %>%
    read_excel(sheet = "Scenario 2a+AT_April estimate-", skip = 1) %>%
    clean_names() %>%
    select(state, FY22_NET_NEW = fy_22_proposed_tx_net_new_target) %>%
    mutate(FY22_NET_NEW = round(FY22_NET_NEW, 0))


# MUNGING ----

  # NAT SUBNAT
  df_pops <- df_nat %>%
    filter(operatingunit == "Nigeria",
           indicator %in% c("POP_EST", "PLHIV"),
           standardizeddisaggregate == "Total Numerator") %>%
    select(fiscal_year, psnuuid, psnu, indicator, value = targets) %>%
    arrange(fiscal_year, desc(indicator)) %>%
    filter(fiscal_year %in% c(curr_fy -1, curr_fy, curr_fy + 1)) %>%
    mutate(period = paste0("FY", str_sub(fiscal_year, 3, 4))) %>%
    select(-fiscal_year) %>%
    relocate(period, .before = 1) %>%
    pivot_wider(names_from = c(period, indicator), values_from = value)


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
    left_join(df_pops, by = c("psnuuid", "psnu")) %>%
    arrange(flag, desc(FY22_PLHIV), psnu)

  # PSNU TX
  inds <- c("TX_CURR", "TX_NEW", "TX_NET_NEW")
  #inds <- c("TX_CURR", "TX_NEW")

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
    pivot_wider(names_from = period, values_from = value)

  # Add states context ----
  df_targets <- df_cntry %>%
    left_join(df_targets, by = c("psnuuid", "psnu")) %>%
    relocate(fundingagency, .after = psnu)

  # Merge Lagos data ----
  df_targets <- df_targets %>%
    filter(psnu == "Lagos") %>%
    group_by_at(.vars = vars(psnuuid, psnu, snuprioritization, flag,
                          ends_with("POP_EST"), ends_with("PLHIV"),
                          indicator)) %>%
    summarise(across(starts_with("FY"), sum, na.rm = T), .groups = "drop") %>%
    ungroup() %>%
    mutate(fundingagency = "USAID & CDC") %>%
    relocate(fundingagency, .after = psnu) %>%
    bind_rows(df_targets) %>%
    filter(!(psnu == "Lagos" & fundingagency %in% c("USAID", "CDC"))) %>%
    #mutate(FY21Q4 = NA_integer_) %>%
    arrange(flag, desc(FY22_PLHIV), psnu)

  # Clean up TX_NEW Targets
  df_targets <- df_targets %>%
    mutate(across(.cols = ends_with("Targets"), ~ ifelse(. == 0, NA, .)))


  # Extract NET NEW Results
  df_targets_tx_net_new <- df_targets %>%
    filter(indicator == "TX_NET_NEW") %>%
    select(-ends_with("Targets")) %>%
    left_join(df_net_new, by = c("psnu" = "state")) %>%
    relocate(FY20_POP_EST, FY20_PLHIV, .before = FY20Q1) %>%
    relocate(FY21_POP_EST, FY21_PLHIV, .before = FY21Q1) %>%
    relocate(FY22_POP_EST, FY22_PLHIV, .after = FY21Cumulative)

  # df_targets_tx_net_new %>%
  #   write_csv(file = paste0(dir_dataout,
  #                           "/Nigeria - OPU FY22 Targets TX_NET_NEW.csv"),
  #             na = "")


  # Calculate TX_CURR Performance ----
  df_targets_tx_curr <- df_targets %>%
    filter(indicator == "TX_CURR") %>%
    mutate(
      FY21Targets = case_when(
        psnu == "Abia" & is.na(FY21Targets) ~ 17588,
        psnu == "Taraba" & is.na(FY21Targets) ~ 45531,
        TRUE ~ FY21Targets
      ),
      FY20Achievement = FY20Cumulative / FY20Targets,
      FY21Achievement = FY21Cumulative / FY21Targets,
      FY20Coverage = FY20Cumulative / FY20_PLHIV,
      FY20PCoverage = FY20Targets / FY20_PLHIV,
      FY21RIncrease = (FY21Cumulative - FY20Cumulative) / FY20Cumulative,
      FY21PIncrease = FY21Achievement - FY20Achievement,
      FY21Coverage = FY21Cumulative / FY21_PLHIV,
      FY21PCoverage = FY21Targets / FY21_PLHIV,
      FY22PCoverage = FY22Targets / FY22_PLHIV,
      FY22TIncrease = (FY22Targets - FY21Targets)
    ) %>%
    relocate(FY20Achievement, .after = FY20Targets) %>%
    relocate(FY20Coverage, FY20PCoverage, .after = FY20Achievement) %>%
    relocate(FY21Achievement, .after = FY21Targets) %>%
    relocate(FY21RIncrease, .after = FY21Cumulative) %>%
    relocate(FY21PIncrease, .after = FY21Achievement) %>%
    relocate(FY21Coverage, FY21PCoverage, .after = FY21PIncrease) %>%
    relocate(FY22PCoverage, .after = FY22Targets) %>%
    relocate(matches("FY20_POP|FY20_PL"), .before = FY20Q1) %>%
    relocate(matches("FY21_POP|FY21_PL"), .before = FY21Q1) %>%
    relocate(matches("FY22_POP|FY22_PL"), .before = FY22Targets)



  df_targets_tx_curr <- df_targets_tx_curr %>%
    left_join(df_net_new, by = c("psnu" = "state")) %>%
    mutate(
      FY22_TX_NEW = round(FY22_NET_NEW + (FY22_NET_NEW * 0.02) + (FY21Targets * 0.02), 0),
      FY22OPUTargets = FY21Targets + FY22_NET_NEW,
      FY22OPUTIncrease = FY22OPUTargets - FY21Targets,
      FY22OPUCoverage = FY22OPUTargets / FY22_PLHIV
    ) %>%
    relocate(FY22_NET_NEW, FY22_TX_NEW, .after = FY22_PLHIV) %>%
    relocate(FY22OPUTargets, .after = FY22Targets) %>%
    relocate(FY22OPUCoverage, .after = FY22PCoverage) %>%
    mutate(fundingagency = factor(
      fundingagency,
      levels = c("USAID", "USAID & CDC", "CDC", "DOD"),
      ordered = TRUE)
    )

  # df_targets_tx_curr %>%
  #   write_csv(file = paste0(dir_dataout,
  #                           "/Nigeria - OPU FY22 Targets TX_CURR.csv"), na = "")


# VIZ ----


  df_targets_tx_curr %>% glimpse()

  df_targets_tx_curr %>%
    arrange(fundingagency) %>%
    group_by(fundingagency) %>%
    gt() %>%
    tab_options(
      source_notes.font.size = 8,
      table.font.size = 12,
      data_row.padding = gt::px(5),
      source_notes.padding = gt::px(1)
    )

  viz_targets <- df_targets_tx_curr %>%
    arrange(fundingagency) %>%
    group_by(fundingagency) %>%
    gt(rowname_col = "psnu") %>%
    tab_header(
      title = "PEPFAR/Nigeria - TX_CURR PERFORMANCE & TARGETS",
      subtitle = "USAID FY22 Targets Adjustments based on past/current performance"
    ) %>%
    tab_spanner(
      label = "2020",
      columns = starts_with("FY20")
    ) %>%
    tab_spanner(
      label = "2021",
      columns = starts_with("FY21")
    ) %>%
    tab_spanner(
      label = "2022",
      columns = starts_with("FY22")
    ) %>%
    tab_options(
      source_notes.font.size = 8,
      table.font.size = 12,
      data_row.padding = gt::px(5),
      source_notes.padding = gt::px(1)
    ) %>%
    cols_hide(
      columns = c("psnuuid", "snuprioritization", "flag", "indicator",
                  ends_with("POP_EST"),
                  matches("FY\\d{2}Q\\d{1}"))
    ) %>%
    cols_label(
      #psnuuid = "UID",
      psnu = "State",
      fundingagency = "Agency",
      snuprioritization = "SNU Prioritization",
      flag = "Programing Status",
      FY20_POP_EST = "Population",
      FY21_POP_EST = "Population",
      FY22_POP_EST = "Population",
      FY20_PLHIV = "PLHIV",
      FY21_PLHIV = "PLHIV",
      FY22_PLHIV = "PLHIV",
      FY21Q1 = "Q1",
      FY21Q2 = "Q2",
      FY21Q3 = "Q3",
      FY21Q4 = "Q4",
      FY20Cumulative = "Results",
      FY21Cumulative = "Results",
      FY20Targets = "Targets",
      FY21Targets = "Targets",
      FY22Targets = "Targets",
      FY22OPUTargets = "OPU Targets*",
      FY20Achievement = "Achv",
      FY21Achievement = "Achv",
      FY21RIncrease = "Increase*",
      FY21PIncrease = "Change**",
      FY22TIncrease = "Change***",
      FY22OPUTIncrease = "OPU.Change***",
      FY22_NET_NEW = "NET NEW",
      FY22_TX_NEW = "TX NEW",
      FY20Coverage = "R.Cov*",
      FY20PCoverage = "P.Cov*",
      FY21Coverage = "R.Cov",
      FY21PCoverage = "P.Cov",
      FY22PCoverage = "P.Cov",
      FY22OPUCoverage = "OPU.Cov*"
    ) %>%
    fmt_missing(
      columns = tidyselect::everything(),
      missing_text = "-"
    ) %>%
    fmt_percent(
      columns = matches("Achievement|Coverage|RIncrease|PIncrease"),
      decimal = 0
    ) %>%
    fmt_number(
      columns = matches("PLHIV|Q|Results|Cumulative|Targets|TIncrease"),
      decimal = 0
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
    )



  viz_targets %>%
    gtsave(filename = paste0(dir_graphics,
                             "/Nigeria_FY22_Targets_Adjustment.png"))


