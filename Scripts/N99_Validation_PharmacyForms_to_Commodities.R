# PROJECT: si-naija
# PURPOSE: Pharmacie form triangulation with ARV Consumptions
# AUTHOR: Baboyma Kagniniwa | USAID/GH - Office of HIV-AIDS
# LICENSE: MIT
# REF. ID: 75d05bc9
# CREATED: 2024-09-10
# UPDATED: 2024-09-10
# NOTES:

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(glamr)
  library(gophr)
  library(glitr)
  library(tidytext)
  library(ggtext)
  library(scales)
  library(systemfonts)
  library(glue)
  library(janitor)
  library(lubridate)
  library(patchwork)

  source("./Scripts/N00_Utilities.R")

# Set paths  ====

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_datim <- glamr::si_path("path_datim")

# Params

  ref_id <- "75d05bc9"
  ou <-  "Nigeria"
  cntry <- ou
  agency <- "USAID"

# FILES ====

  file_ou <- dir_mer %>%
    return_latest(glue("OU_IM_FY22.*.zip$"))

  file_psnu <- dir_mer %>%
    return_latest(glue("PSNU_IM_FY22.*_{ou}.zip$"))

  file_site <- dir_mer %>%
    return_latest(glue("Site_IM_FY22.*_{ou}.zip$"))

  file_clients <- dir_datim %>%
    file.path(glue("COUNTRIES/{ou}/Data/EMRs")) %>%
    file.path("FY24 Q4 LAMISPLUS Extracts/ACE 3/Patient List") %>%
    return_latest("Patient Line List")

  file_pharm <- dir_datim %>%
    file.path(glue("COUNTRIES/{ou}/Data/EMRs")) %>%
    file.path("FY24 Q4 LAMISPLUS Extracts/ACE 3/Pharmacy") %>%
    return_latest("Pharmacy")

  file_lab <- dir_datim %>%
    file.path(glue("COUNTRIES/{ou}/Data/EMRs")) %>%
    file.path("FY24 Q4 LAMISPLUS Extracts/ACE 3/Lab") %>%
    return_latest("Laboratory")

# Options

  meta <- file_site %>% get_metadata()

  curr_pds <- list(
    "FY24Q1" = list("start" = "2023-10-01", "end" = "2023-12-31"),
    "FY24Q2" = list("start" = "2024-01-01", "end" = "2024-03-31"),
    "FY24Q3" = list("start" = "2024-04-01", "end" = "2024-06-30"),
    "FY24Q4" = list("start" = "2024-07-01", "end" = "2024-09-30")
  )

# Functions  ====

  #' @title Determine MMD Type based on refill # of days
  #'
  #' @param refill # of refill days
  #'
  get_mmd_type <- function(refill) {

    mdays <- 30

    mmd <- round(refill/mdays, 0)

    mmd <- map(mmd, ~ifelse(. <= 1, 0, .))
    #mmd <- map(mmd, ~ifelse(. >= 6, 99, .))

    paste0("MMD-", mmd)
  }

# LOAD DATA ====

  ## Site x IM - TX Data

  df_msd_sites <- file_site %>%
    read_psd() %>%
    filter(str_detect(indicator, "TX_"))

  ## ACE 3 - Patient List

  file_clients %>% excel_sheets()

  df_clients <- file_clients %>%
    excel_sheets() %>%
    map(~read_excel(path = file_clients, sheet = .x)) %>%
    bind_rows()

  df_clients %>% glimpse()

  df_clients <- df_clients %>%
    rename_with(~str_remove(.x, "\\(.*\\)")) %>%
    clean_names()

  df_clients %>% distinct(current_status)

  df_clients %>%
    #filter(!is.na(date_current_status)) %>%
    mutate(date_current_status = ymd(date_current_status)) %>%
    pull(date_current_status) %>%
    range(na.rm = T)

  df_clients %>%
    filter(current_status == "ACTIVE") %>%
    mutate(date_current_status = ymd(date_current_status)) %>%
    pull(date_of_last_refill) %>%
    range(na.rm = T)

  ## ACE 3 - Pharmacy refill forms

  file_pharm %>% excel_sheets()

  df_pharm <- file_pharm %>%
    excel_sheets() %>%
    map(~read_excel(path = file_pharm, sheet = .x)) %>%
    bind_rows()

  df_pharm %>% glimpse()

  df_pharm <- df_pharm %>%
    rename_with(~str_remove(.x, "\\(.*\\)")) %>%
    clean_names()

  ## ACE 3 - Laboratory

  file_lab %>% excel_sheets()

  df_lab <- file_lab %>%
    excel_sheets() %>%
    map(~read_excel(path = file_lab, sheet = .x)) %>%
    bind_rows()

  df_lab %>% glimpse()

  df_lab <- df_lab %>%
    rename_with(~str_remove(.x, "\\(.*\\)")) %>%
    clean_names()

  df_lab %>% distinct(test)

# MUNGE ====

  ## Site Patients on TX & ARV Dispensation

  df_msd_sites %>% glimpse()

  df_tx <- df_msd_sites %>%
    filter(
      fiscal_year == meta$curr_fy,
      funding_agency == agency,
      indicator %in% c("TX_CURR", "TX_PVLS"),
      standardizeddisaggregate %in% c(
        "Age/Sex/HIVStatus",
        "Age/Sex/ARVDispense/HIVStatus"
      )) %>% #distinct(indicator, standardizeddisaggregate, otherdisaggregate)
    clean_indicator() %>%
    clean_mechs() %>%
    clean_partners() %>%
    clean_modalities()
    summarise(across(starts_with("qtr|cum"), ~sum(.x, na.rm = T)),
              .by = c(orgunituid, sitename,
                      mech_code, mech_name, prime_partner_name,
                      indicator))

  df_tx %>%
    filter()

  ## Pharmacy

  df_pharm %>% glimpse()

  df_pharm %>%
    distinct(refill_period, mmd_type) %>%
    arrange(refill_period)

  df_pharm_clean <- df_pharm %>%
    rename(
      regimen = regimens,
      visit_date = date_visit
    ) %>%
    mutate(
      refill_period = as.integer(refill_period),
      mmd_type = case_when(
        is.na(mmd_type) & refill_period <= 30 ~ "MMD-0",
        is.na(mmd_type) & refill_period == 60 ~ "MMD-2",
        is.na(mmd_type) & refill_period == 90 ~ "MMD-3",
        is.na(mmd_type) & refill_period == 120 ~ "MMD-4",
        is.na(mmd_type) & refill_period == 150 ~ "MMD-5",
        is.na(mmd_type) & refill_period == 180 ~ "MMD-6",
        is.na(mmd_type) & (refill_period == 0 | refill_period > 180) ~ "MMD-X",
        is.na(mmd_type) & !is.na(refill_period) ~ get_mmd_type(refill_period),
        TRUE ~ mmd_type
      ),
      visit_wday = wday(ymd(visit_date), label = T, abbr = T),
      visit_day = day(ymd(visit_date)),
      visit_week = floor_date(ymd(visit_date), unit = "week", week_start = 1),
      #visit_week = week(ymd(visit_date), unit = "week"),
      #visit_month = floor_date(ymd(visit_date), unit = "month"),
      visit_month = month(ymd(visit_date), label = T),
      visit_year = year(ymd(visit_date)),
      visit_qtr = quarter(ymd(visit_date), fiscal_start = 10)
    ) %>%
    relocate(starts_with("visit_"), .after = hospital_num)


  df_pharm_clean %>% distinct(facility_name, datim_id, hospital_num) %>% nrow()
  df_pharm_clean %>% distinct(facility_name, datim_id) %>% nrow()
  df_pharm_clean %>% distinct(facility_name) %>% nrow()

  df_pharm_clean %>% pull(visit_date) %>% range(na.rm = T)
  df_pharm_clean %>% pull(next_appointment) %>% range(na.rm = T)

  # Filter data for Current Reporting Period only
  df_pharm_curr <- df_pharm_clean %>%
    mutate(visit_date = ymd(visit_date)) %>%
    filter(
      str_detect(datim_id, "\\d{3}", negate = T),
      between(
        visit_date,
        left = ymd(curr_pds[[meta$curr_pd]]$start),
        right = ymd(curr_pds[[meta$curr_pd]]$end)
      ),
      str_detect(str_to_lower(regimen_line), "line$"),
      !is.na(refill_period)
      #,mmd_type != "MMD-X"
    )

  df_pharm_curr %>% pull(visit_date) %>% range()

  df_pharm_curr %>%
    distinct(refill_period, mmd_type) %>%
    arrange(refill_period)

  df_pharm_curr %>%
    distinct(regimen_line, regimen) %>%
    arrange(regimen_line)

  df_pharm_curr %>% distinct(datim_id)
  df_pharm_curr %>% distinct(patient_id)

  ids_with_n_pickups <- df_pharm_curr %>%
    count(patient_id) %>%
    filter(n > 1) %>%
    pull(patient_id)

  df_pharm_curr %>%
    filter(patient_id %in% ids_with_n_pickups)

  df_pharm_pickups <- df_pharm_curr %>%
    summarise(
      pickups = n_distinct(patient_id),
      refills = sum(refill_period, na.rm = T),
      .by = c(visit_year, visit_month, visit_week, visit_date, visit_day)
    )

  df_pharm_site <- df_pharm_curr %>%
    summarise(
      pickups = n_distinct(patient_id),
      refills = sum(refill_period, na.rm = T),
      .by = c(facility_name, visit_year, visit_month, visit_week, visit_day)
    )

  df_pharm_mmd <- df_pharm_curr %>%
    mutate(mmd_type = get_mmd_type(refill_period)) %>%
    summarise(
      pickups = n_distinct(patient_id),
      refills = sum(refill_period, na.rm = T),
      .by = c(mmd_type, visit_year, visit_month, visit_day)
    )

  df_pharm_arv <- df_pharm_curr %>%
    summarise(
      pickups = n_distinct(patient_id),
      refills = sum(refill_period, na.rm = T),
      .by = c(regimen_line, regimen,
              visit_year, visit_month, visit_day)
    )

# VIZ ====

  ## Pickups

  df_pharm_pickups_m <- df_pharm_pickups %>%
    summarise(pickups = sum(pickups, na.rm = T), .by = visit_month)

  df_pharm_pickups <- df_pharm_pickups %>%
    arrange(visit_year, visit_month, visit_day) %>%
    group_by(visit_year, visit_month, visit_week) %>%
    mutate(
      label_date = case_when(
        row_number() == 1 ~ visit_week,
        TRUE ~ NA_Date_
      ),
      label_day = case_when(
        row_number() == 1 ~ visit_day,
        TRUE ~ NA_integer_
      )
    ) %>%
    ungroup()

  w_breaks <- df_pharm_pickups %>%
    filter(visit_month == "Jan") %>%
    pull(label_date)

  w_breaks[which(is.na(w_breaks) == F)]

  d_breaks <- df_pharm_pickups %>%
    filter(visit_month == "Jan") %>%
    pull(label_day)

  d_breaks[which(is.na(d_breaks) == F)]

  ## VIZ - Monthly Pickups

  viz_m_pickups <- df_pharm_pickups_m %>%
    ggplot(aes(visit_month, pickups)) +
    geom_col(fill = genoa, width = .9) +
    geom_hline(yintercept = 0) +
    geom_text(aes(label = comma(pickups)),
              vjust = 1.9, size = 8, color = grey10k, fontface = "bold") +
    labs(x = "", y = "",
         title = glue("{meta$curr_pd} - # PATIENTS PICKING UP ARV FROM PHARMACIES"),
         subtitle = glue("**ACE 3** reported **{comma(sum(df_pharm_pickups_m$pickups))} patients** picking up ARVs")) +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(face = "bold"),
          plot.title = element_markdown(),
          plot.subtitle = element_markdown())

  ## VIZ - Month / Weekly Pickups

  viz_d_pickups <-
  df_pharm_pickups %>%
    ggplot(aes(visit_day, visit_month, fill = pickups)) +
    #geom_vline(xintercept = seq(1, 31, 7), linewidth = 2, color = usaid_darkgrey) +
    geom_tile(color = grey10k) +
    geom_point(data = df_pharm_pickups %>% filter(!is.na(label_date)),
               aes(visit_day, visit_month)) +
    #geom_vline(xintercept = seq(1, 31, 7)) +
    #geom_vline(xintercept = 0) +
    #scale_x_continuous(breaks = seq(1, 31, 1)) +
    scale_x_continuous(breaks = w_breaks) +
    scale_fill_si(palette = "genoas",
                  na.value = "red",
                  breaks = seq(0, max(df_pharm_pickups$pickups), 25),
                  labels = seq(0, max(df_pharm_pickups$pickups), 25)) +
    labs(x = "", y = "", title = "", subtitle = "") +
    coord_equal() +
    si_style_nolines() +
    theme(axis.line.x = element_blank(),
          axis.text.y = element_text(face = "bold"),
          #axis.text.y = element_blank(),
          plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.width = unit(.2, "npc"))

  #viz_d_pickups <-
    df_pharm_pickups %>%
    ggplot(aes(visit_date, visit_month, fill = pickups)) +
    geom_tile(color = grey10k, height = .5) +
    geom_point(data = df_pharm_pickups %>%
                 filter(!is.na(label_date)),
               aes(visit_date, visit_month)) +
    scale_x_continuous(breaks = w_breaks) +
    scale_fill_si(palette = "genoas",
                  na.value = "red",
                  breaks = seq(0, max(df_pharm_pickups$pickups), 25),
                  labels = seq(0, max(df_pharm_pickups$pickups), 25)) +
    labs(x = "", y = "", title = "", subtitle = "") +
    facet_wrap(~visit_month, nrow = 3, scales = "free") +
    si_style_nolines() +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.width = unit(.2, "npc"))

  viz_m_pickups / viz_d_pickups

# EXPORT ====

