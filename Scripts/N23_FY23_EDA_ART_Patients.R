# PURPOSE: SI-NAIJA
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: EDA - ART Initiation and Pickups
# REF ID:  0c15dcfe
# LICENSE: MIT
# DATE:    2023-10-24
# UPDATE:  2023-10-24
# NOTES:   DQA - Dates looks too similar!

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(glamr)
  library(gophr)
  library(glitr)
  library(gisr)
  library(sf)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(janitor)
  library(lubridate)

# LOCALS & SETUP ====

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_cntry <- file.path("../../PEPFAR/COUNTRIES/Nigeria")

  # Files

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21")
  file_site <- si_path() %>% return_latest("Site_IM_FY21")

  file_radet <- dir_cntry %>%
    file.path("DQAs/FY23/Enhanced Interagency DQA/Data") %>%
    return_latest("Radet_.*.csv")

  file_dqa_sites <- dir_data %>%
    file.path("DQA") %>%
    return_latest("list of DQA.*.xlsx")

  get_metadata(file_psnu)

  meta <- metadata

  # Set Params

  ref_id <- "0c15dcfe"
  agency <- "USAID"
  cntry <- "Nigeria"

  meta$source <- meta$source %>% paste("- Ref. ID =", ref_id)

  ai_sites <- c("K66w8LC7LCL", "TeUPDYF4sGG")

# Functions  =====

# LOAD DATA =====

  # DQA Sites

  file_dqa_sites %>% open_path()

  df_ss <- file_dqa_sites %>%
    read_excel(sheet = 1) %>%
    clean_names() %>%
    select(site, satellite = x3)

  df_ss <- df_ss %>%
    fill(site, .direction = "down") %>%
    distinct_all() %>%
    mutate(
      site = case_when(
        site == "Mushin OSS" ~ "Mushin KP One Stop Shop",
        site == "Federal Medical Centre Makurdi" ~ "Federal Medical Center - Makurdi",
        str_detect(site, "Rivers\\)") ~ "KPIF Obio-Akpor KP OSS",
        TRUE ~ str_remove(site, ",$")
      )
    ) %>%
    arrange(site, satellite)

  # Patient ARV

  df_radet <- file_radet %>%
    read_csv(col_types = "c") %>%
    clean_names()

  df_radet %>% glimpse()

  df_radet %>% filter(row_number() >= 1000)

  df_radet %>% distinct(current_status) %>% pull() %>% sort()
  df_radet %>% distinct(dsd_model) %>% prinf()
  df_radet %>% distinct(target_group) %>% prinf()
  df_radet %>% distinct(enrollment_setting) %>% prinf()
  df_radet %>% distinct(state) %>% prinf()
  df_radet %>% distinct(period) %>% prinf()

# MUNGE =====

  ## Get a subset

  df_art <- df_radet %>%
    select(patientuid = person_uuid, gender, age, date_of_birth,
           orgunituid = datim_id, target_group, enrollment_setting,
           status_current = current_status,
           status_previous = previous_status,
           facility_name, lga, state,
           date_art_start = art_start_date,
           date_last_pickup = last_pickup_date,
           date_next_pickup = next_pickup_date,
           art_start_regimen = current_art_regimen,
           date_current_regimen = date_of_current_regimen,
           art_current_regimen = current_art_regimen,
           arv_month_of_refill = months_of_arv_refill)

  df_art %>% glimpse()
  df_art %>% distinct(patientuid)
  df_art %>% distinct(facility_name)

  ## Clean up dates & extract parts of dates

  df_art_pickups <- df_art %>%
    mutate(date_art_start = ymd(date_art_start),
           date_sfy = str_sub(quarter(date_art_start, type = "year.quarter", fiscal_start = 10), 1, 4),
           date_sqtr = str_sub(quarter(date_art_start, type = "year.quarter", fiscal_start = 10), -1),
           date_spd = paste0("FY", str_sub(date_sfy, 3, 4), "Q", date_sqtr),
           date_syear = year(date_art_start),
           date_smonth = paste0("M", str_pad(month(date_art_start), width = 2, pad = "0")),
           date_smonth2 = month(date_art_start, label = TRUE),
           date_sweek = paste0("W", str_pad(week(date_art_start), width = 2, pad = "0")),
           date_sday = paste0("D", str_pad(day(date_art_start), width = 2, pad = "0")),
           date_last_pickup = ymd(date_next_pickup),
           date_lfy = str_sub(quarter(date_last_pickup, type = "year.quarter", fiscal_start = 10), 1, 4),
           date_lqtr = str_sub(quarter(date_last_pickup, type = "year.quarter", fiscal_start = 10), -1),
           date_lpd = paste0("FY", str_sub(date_lfy, 3, 4), "Q", date_lqtr),
           date_lyear = year(date_last_pickup),
           date_lmonth2 = month(date_last_pickup, label = TRUE),
           date_lmonth = month(date_last_pickup),
           date_lweek = paste0("W", str_pad(week(date_last_pickup), width = 2, pad = "0")),
           date_lday = paste0("D", str_pad(day(date_last_pickup), width = 2, pad = "0")),
           date_next_pickup = ymd(date_next_pickup),
           date_nfy = str_sub(quarter(date_next_pickup, type = "year.quarter", fiscal_start = 10), 1, 4),
           date_nqtr = str_sub(quarter(date_next_pickup, type = "year.quarter", fiscal_start = 10), -1),
           date_npd = paste0("FY", str_sub(date_nfy, 3, 4), "Q", date_nqtr),
           date_nyear = year(date_next_pickup),
           date_nmonth = month(date_next_pickup),
           date_nmonth2 = month(date_next_pickup, label = TRUE),
           date_nweek = paste0("W", str_pad(week(date_next_pickup), width = 2, pad = "0")),
           date_nday = paste0("D", str_pad(day(date_next_pickup), width = 2, pad = "0")),
           date_cregimen = ymd(date_current_regimen))

  ## Extract current active patients only

  df_art_active <- df_art_pickups %>%
    filter(str_detect(str_to_lower(status_current), "active")) %>%
    mutate(status_current = str_to_sentence(status_current))

  df_art_active %>% distinct(date_sfy) %>% pull() %>% sort()

  # df_art_active <- df_art_active %>%
  #   filter(orgunituid %in% ai_sites,
  #          date_syear > 2013)

  ## Summaries

  ## Enrollment Cohorts by states

  df_art_cohorts <- df_art_pickups %>%
    summarise(n = n(), .by = c(state)) %>%
    mutate(group = "Enrollees") %>%
    relocate(n, .after = last_col())

  df_art_cohorts <- df_art_pickups %>%
    filter(str_detect(str_to_lower(status_current), "active")) %>%
    summarise(n = n(), .by = c(state)) %>%
    mutate(group = "Active enrollees") %>%
    bind_rows(df_art_cohorts, .)

  df_art_cohorts <- df_art_pickups %>%
    filter(str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)) %>%
    summarise(n = n(), .by = c(state)) %>%
    mutate(group = "Active enr. FY13 to 23") %>%
    bind_rows(df_art_cohorts, .) %>%
    filter(str_detect(state, "LAB", negate = TRUE))

  df_art_cohorts <- df_art_cohorts %>%
    summarise(n = sum(n, na.rm = T), .by = group) %>%
    mutate(state = "USAID") %>%
    bind_rows(df_art_cohorts, .) %>%
    filter(str_detect(state, "LAB", negate = TRUE))

  art_status <- c("Enrollees", "Active enrollees", "Active enr. FY13 to 23")

  cohorts_states <- df_art_cohorts %>%
    filter(group == "Enrollees") %>%
    arrange(desc(n)) %>%
    pull(state)

  df_art_cohorts <- df_art_cohorts %>%
    mutate(group = factor(group, levels = art_status, ordered = T),
           state = factor(state, levels = cohorts_states, ordered = T))

  ## Enrollment cohorts by year and states

  df_art_cohorts_trend <- df_art_pickups %>%
    filter(str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)) %>%
    summarise(n = n(), .by = c(date_sfy, state)) %>%
    filter(str_detect(state, "LAB", negate = TRUE))

  df_art_cohorts_trend <- df_art_pickups %>%
    filter(str_detect(state, "LAB", negate = TRUE),
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)) %>%
    summarise(n = n(), .by = c(date_sfy)) %>%
    mutate(state = "USAID") %>%
    bind_rows(df_art_cohorts_trend, .)

  cohorts_t_states <- df_art_cohorts_trend %>%
    filter(date_sfy == "2023") %>%
    arrange(desc(n)) %>%
    pull(state)

  df_art_cohorts_trend <- df_art_cohorts_trend %>%
    mutate(state = factor(state, levels = cohorts_t_states, ordered = T))


  ## Pickup patterns

  # df_art_lmonths <- df_art_pickups %>%
  #   summarise(n = n_distinct(patientuid),
  #             .by = c(period, orgunituid, facility_name, lga,
  #                     date_syear, date_lmonth, date_lmonth2)) %>%
  #   mutate(date_sweek = paste0(
  #     date_syear, ", ", date_lmonth2))
  #
  # df_art_lweeks <- df_art_pickups %>%
  #   summarise(n = n_distinct(patientuid),
  #             .by = c(period, orgunituid, facility_name, lga,
  #                     date_lyear, date_smonth, date_lweek)) %>%
  #   mutate(date_sweek = paste0(
  #     date_lyear, " W",
  #     str_pad(date_lweek, width = 2, pad = "0")))
  #
  # df_art_ldays <- df_art_pickups %>%
  #   summarise(n = n_distinct(patientuid),
  #             .by = c(period, orgunituid, facility_name, lga,
  #                     date_lyear, date_lmonth, date_art_start))

  df_art_lweeks <- df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2019, 2022)) %>%
    summarise(
      n = n(),
      .by = c(orgunituid, date_lpd, date_lmonth2, date_lweek, date_last_pickup)) %>%
    arrange(orgunituid, date_lpd, date_lmonth2)

# VIZ =====

  ## Enrollment change

  df_art_cohorts %>%
    ggplot(aes(x = group, y = n, fill = group)) +
    geom_hline(yintercept = 0, color = usaid_black) +
    geom_col() +
    geom_text(aes(label = comma(n, accuracy = .1, scale_cut = cut_short_scale())),
              color = "white", size = 4, vjust = 1.2) +
    scale_fill_manual(
      values = c("Enrollees" = burnt_sienna,
                 "Active enrollees" = scooter,
                 "Active enr. FY13 to 23" = genoa)) +
    facet_wrap(~state, nrow = 3, scales = "free_y") +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(legend.title = element_blank(),
          axis.text = element_blank(),
          strip.text = element_text(size = 10, face = "bold"))

  ## Enrollment trends

  df_art_cohorts_trend %>%
    filter(state == "USAID") %>%
    ggplot(aes(x = date_sfy, y = n)) +
    geom_rect(xmin = 6.5, xmax = 10.5, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .1) +
    geom_hline(yintercept = 0, color = usaid_black) +
    geom_col(fill = genoa) +
    geom_text(aes(label = comma(n, accuracy = 1, scale_cut = cut_short_scale())),
              color = "white", size = 4, vjust = 1.3) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~state, nrow = 3) +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(legend.title = element_blank(),
          strip.text = element_text(size = 10, face = "bold"))

  yr_labels <- df_art_cohorts_trend %>%
    distinct(date_sfy) %>%
    arrange(date_sfy) %>%
    mutate(label = ifelse(row_number() %% 2 == 0, "", date_sfy)) %>%
    pull(label)

  df_art_cohorts_trend %>%
    ggplot(aes(x = date_sfy, y = n)) +
    geom_rect(xmin = 6.5, xmax = 10.5, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .1) +
    geom_hline(yintercept = 0, color = usaid_black) +
    geom_col(fill = genoa) +
    scale_x_discrete(labels = yr_labels) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~state, nrow = 3, scales = "free_y") +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(legend.title = element_blank(),
          strip.text = element_text(size = 10, face = "bold"))

  ## Pickup pattern

  ## Lga level

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, date_lfy, date_lpd, date_lmonth2,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>%
    arrange(lga, desc(n)) %>%
    mutate(lga = paste0(lga, " (", state, ")")) %>%
    ggplot(aes(x = date_last_pickup, y = lga, fill = n)) +
    geom_tile(color = grey10k, linewidth = .1) +
    scale_x_date(breaks = "1 weeks") +
    scale_fill_si(palette = "burnt_siennas") +
    labs(x = "", y = "") +
    si_style() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(3, "cm"))

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, date_lfy, date_lpd, date_lmonth2,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>%
    arrange(lga, desc(n)) %>%
    mutate(lga = paste0(lga, " (", state, ")")) %>%
    ggplot(aes(x = state, y = n, group = state)) +
    geom_boxplot(width=.1, cex=.5) +
    coord_flip() +
    labs(y = "", x = "") +
    si_style()


  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, date_lfy, date_lpd, date_lmonth2,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>%
    arrange(lga, desc(n)) %>%
    mutate(lga = paste0(lga, " (", state, ")")) %>%
    ggplot(aes(x = lga, y = n, group = lga)) +
    geom_boxplot(width=.1, cex=.5) +
    coord_flip() +
    labs(y = "", x = "") +
    si_style()

  ## facility level

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           lga %in% c("Mbo", "Eket", "Uyo"),
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(lga, facility_name, date_lfy, date_lpd, date_lmonth2,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>%
    mutate(facility_name = paste0(facility_name, " (", lga, ")")) %>%
    ggplot(aes(x = date_last_pickup, y = facility_name, fill = n)) +
    geom_tile() +
    scale_x_date(breaks = "1 weeks") +
    scale_fill_si(palette = "burnt_siennas") +
    labs(x = "", y = "") +
    si_style() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(3, "cm"))

  ## Facility level

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023),
           between(as.integer(date_lfy), 2023, 2023)
    ) %>% #distinct(orgunituid) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(orgunituid, date_lfy, date_lpd, date_lmonth2,
              date_lweek, date_last_pickup)) %>%
    #distinct(date_lfy) %>%
    filter(date_lpd == "FY23Q4") %>%
    arrange(orgunituid, date_lpd, date_lmonth2) %>%
    ggplot(aes(x = date_last_pickup, y = orgunituid, fill = n)) +
    geom_tile() +
    scale_x_date(breaks = "1 weeks") +
    scale_fill_si()





  df_art_lmonths %>%
    ggplot(aes(x = date_lmonth, y = n)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~facility_name, nrow = 1) +
    si_style()

  df_art_lweeks %>%
    ggplot(aes(x = date_lweek, y = n)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~facility_name, nrow = 1) +
    si_style()

  df_art_sdays %>%
    filter(date_smonth == 11) %>%
    ggplot(aes(x = date_art_start, y = n)) +
    geom_col() +
    facet_wrap(date_smonth ~ facility_name) +
    scale_x_date() +
    coord_flip() +
    si_style()

# OUTPUTS =====

