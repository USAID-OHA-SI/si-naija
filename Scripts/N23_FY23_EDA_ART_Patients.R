# PURPOSE: SI-NAIJA
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: EDA - ART Initiation and Pickups
# REF ID:  0c15dcfe
# LICENSE: MIT
# DATE:    2023-10-24
# UPDATE:  2023-10-30
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
  library(glue)

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
    return_latest("Radet.*.csv")

  file_phcy_radet <- dir_cntry %>%
    file.path("DQAs/FY23/Enhanced Interagency DQA/Data") %>%
    return_latest("pharmacy_radet.*.csv")

  file_dsd <- dir_cntry %>%
    file.path("DQAs/FY23/Enhanced Interagency DQA/Data") %>%
    return_latest("DSD Mapping.*.xlsx")

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

  # Calendar

  pepfar_start <- 2003
  curr_year <- year(curr_date())

  pepfar_calendar <- seq(
    from = ymd(paste0(pepfar_start,"-01-01")),
    to = ymd(paste0(curr_year,"-12-31")),
    by = "days") %>%
    tibble(date_calendar = .) %>%
    mutate(
      date_calendar = ymd(date_calendar),
      date_cyr = as.character(year(date_calendar)),
      date_cfy = str_sub(quarter(date_calendar, type = "year.quarter", fiscal_start = 10), 1, 4),
      date_cqtr = str_sub(quarter(date_calendar, type = "year.quarter", fiscal_start = 10), -1),
      date_cpd = paste0("FY", str_sub(date_cfy, 3, 4), "Q", date_cqtr)
    )



# Functions  =====

# LOAD DATA =====

  ## DQA Sites ----

  #file_dqa_sites %>% open_path()

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

  ## DSD Models ----

  #file_dsd %>% open_path()

  file_dsd %>% excel_sheets()

  ## National DSD Models

  df_dsd_models <- file_dsd %>%
    read_excel(sheet = "National DSD Models", col_names = F) %>%
    clean_names()

  df_dsd_models <- df_dsd_models %>%
    rename(dsd_model = 1) %>%
    filter(!is.na(dsd_model)) %>%
    mutate(
      dsd_group = case_when(
        str_detect(dsd_model, "^FB") ~ "Facility",
        str_detect(dsd_model, "^CB") ~ "Community",
        TRUE ~ "Other"
      )
    ) %>%
    separate(dsd_model, into = c("dsd_code", "dsd_name"), sep = " - ", remove = F)

  ## Mapped DSD Models

  df_dsd_mapping <- file_dsd %>%
    read_excel(sheet = "Mapped DSD Models", col_names = T) %>%
    clean_names()

  df_dsd_mapping <- df_dsd_mapping %>%
    rename(dsd_model = mapped_national_dsd_models) %>%
    mutate(radet_dsd_models = str_trim(radet_dsd_models),
           radet_dsd_models = str_remove(radet_dsd_models, '\\[.*\\] '),
           radet_dsd_models = str_remove_all(radet_dsd_models, '\\"'),
           radet_dsd_models = str_trim(radet_dsd_models),
           dsd_model = case_when(
             str_detect(dsd_model, "Not classified") ~ "Non-DSD Model",
             TRUE ~ dsd_model
           ))

  df_dsd_mapping <- df_dsd_mapping %>%
    left_join(df_dsd_models, by = "dsd_model") %>%
    mutate(
      dsd_group = case_when(
        is.na(dsd_group) ~ dsd_model,
        TRUE ~ dsd_group
      )
    )

  ## Patient ARV Pickups ----

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

  ## Get a subset ----

  df_art <- df_radet %>%
    select(patientuid = personuuid, gender, age, date_of_birth,
           orgunituid = datim_id,
           target_group, enrollment_setting, dsd_model,
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

  ## Clean up dates & extract parts of dates ----

  df_art_pickups <- df_art %>%
    filter(!is.na(date_last_pickup),
           year(ymd(date_next_pickup)) >= pepfar_start,
           year(ymd(date_next_pickup)) <= curr_year) %>%
    mutate(date_art_start = ymd(date_art_start),
           date_sfy = str_sub(quarter(date_art_start, type = "year.quarter", fiscal_start = 10), 1, 4),
           date_sqtr = str_sub(quarter(date_art_start, type = "year.quarter", fiscal_start = 10), -1),
           date_spd = paste0("FY", str_sub(date_sfy, 3, 4), "Q", date_sqtr),
           date_syear = year(date_art_start),
           date_smonth = floor_date(date_art_start, unit = "month"),
           date_sweek = floor_date(date_art_start, unit = "week") + 1, # Week Starting on a Monday
           date_last_pickup = ymd(date_next_pickup),
           date_lfy = str_sub(quarter(date_last_pickup, type = "year.quarter", fiscal_start = 10), 1, 4),
           date_lqtr = str_sub(quarter(date_last_pickup, type = "year.quarter", fiscal_start = 10), -1),
           date_lpd = paste0("FY", str_sub(date_lfy, 3, 4), "Q", date_lqtr),
           date_lyear = year(date_last_pickup),
           date_lmonth = floor_date(date_last_pickup, unit = "month"),
           date_lweek = floor_date(date_last_pickup, unit = "week") + 1, # Mondays
           date_next_pickup = ymd(date_next_pickup),
           date_nfy = str_sub(quarter(date_next_pickup, type = "year.quarter", fiscal_start = 10), 1, 4),
           date_nqtr = str_sub(quarter(date_next_pickup, type = "year.quarter", fiscal_start = 10), -1),
           date_npd = paste0("FY", str_sub(date_nfy, 3, 4), "Q", date_nqtr),
           date_nyear = year(date_next_pickup),
           date_nmonth = floor_date(date_next_pickup, unit = "month"),
           date_nweek = floor_date(date_next_pickup, unit = "week") + 1,
           date_cregimen = ymd(date_current_regimen))

  # Append DSD Models & more clean up ----

  df_art_pickups <- df_art_pickups %>%
    left_join(df_dsd_mapping,
              by = c("dsd_model" = "radet_dsd_models"),
              relationship = "many-to-many") %>%
    rename(dsd_model_ref = dsd_model.y)

  df_art_pickups <- df_art_pickups %>%
    mutate(across(starts_with(c("status_", "target_group")), str_to_upper))


  # Explore  ----

  df_art_pickups %>% glimpse()

  df_art_pickups %>%
    distinct(date_lfy) %>%
    pull() %>%
    sort()

  df_art_pickups %>%
    distinct(date_sfy) %>%
    pull() %>%
    sort()

  df_art_pickups %>%
    distinct(date_last_pickup) %>%
    arrange(date_last_pickup) %>%
    pull() %>%
    range(na.rm = T)

  df_art_pickups %>%
    distinct(gender) %>%
    pull()

  df_art_pickups %>%
    distinct(age) %>%
    pull() %>%
    sort()

  df_art_pickups %>%
    distinct(enrollment_setting) %>%
    pull()

  df_art_pickups %>%
    distinct(target_group) %>%
    pull()

  df_art_pickups %>%
    distinct(status_current) %>%
    pull()

  df_art_pickups %>%
    distinct(dsd_model_ref) %>%
    pull() %>%
    sort()

  df_art_pickups %>%
    distinct(dsd_group) %>%
    pull() %>%
    sort()

  ## Extract current active patients only ----

  df_art_active <- df_art_pickups %>%
    filter(str_detect(str_to_lower(status_current), "active")) %>%
    mutate(status_current = str_to_sentence(status_current))

  df_art_active %>% distinct(date_sfy) %>% pull() %>% sort()
  df_art_active %>% distinct(date_lfy) %>% pull() %>% sort()

  ## Summaries

  ## Enrollment Cohorts by states

  df_art_cohorts <- df_art_pickups %>%
    summarise(n = n_distinct(patientuid), .by = c(state)) %>%
    mutate(group = "Enrollees") %>%
    relocate(n, .after = last_col()) %>%
    filter(str_detect(state, "LAB", negate = T))

  df_art_cohorts <- df_art_pickups %>%
    filter(str_detect(str_to_lower(status_current), "active")) %>%
    summarise(n = n_distinct(patientuid), .by = c(state)) %>%
    mutate(group = "Active enrollees") %>%
    bind_rows(df_art_cohorts, .)

  df_art_cohorts <- df_art_pickups %>%
    filter(str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)) %>%
    summarise(n = n_distinct(patientuid), .by = c(state)) %>%
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

  ## Active patients - Enrollment cohorts by year, states and enrollment setting ----

  df_art_pickups %>% distinct(enrollment_setting)

  df_art_pickups %>%
    distinct(enrollment_setting, target_group) %>%
    arrange(enrollment_setting, target_group) %>%
    prinf()

  df_art_pickups %>%
    distinct(enrollment_setting, dsd_model_ref) %>%
    arrange(enrollment_setting, dsd_model_ref) %>%
    prinf()

  ## Enrollment Settings

  df_art_cohorts_trend <- df_art_pickups %>%
    mutate(
      enrollment_setting = case_when(
        enrollment_setting %ni% c("Facility", "Community") ~ "Other",
        TRUE ~ enrollment_setting
      )
    ) %>%
    filter(str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)) %>%
    summarise(n = n(), .by = c(date_sfy, state, enrollment_setting)) %>%
    filter(str_detect(state, "LAB", negate = TRUE))

  df_art_cohorts_trend <- df_art_pickups %>%
    filter(str_detect(state, "LAB", negate = TRUE),
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)) %>%
    summarise(n = n(), .by = c(date_sfy, enrollment_setting)) %>%
    mutate(state = "USAID") %>%
    bind_rows(df_art_cohorts_trend, .)

  df_art_cohorts_trend <- df_art_cohorts_trend %>%
    filter(enrollment_setting != "Other") %>%
    group_by(date_sfy, state) %>%
    mutate(p = n / sum(n, na.rm = T)) %>%
    ungroup()

  cohorts_t_states <- df_art_cohorts_trend %>%
    summarise(n = sum(n), .by = c(date_sfy, state)) %>%
    filter(date_sfy == "2023") %>%
    arrange(desc(n)) %>%
    pull(state)

  df_art_cohorts_trend <- df_art_cohorts_trend %>%
    mutate(state = factor(state, levels = cohorts_t_states, ordered = T))


  ## Enrollment Settings & Target Group

  df_art_cohorts_trend_target <- df_art_pickups %>%
    mutate(
      enrollment_setting = case_when(
        enrollment_setting %ni% c("Facility", "Community") ~ "Other",
        TRUE ~ enrollment_setting
      ),
      target_group = case_when(
        is.na(target_group) ~ "Other",
        TRUE ~ target_group
      )
    ) %>%
    filter(str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)) %>%
    summarise(n = n_distinct(patientuid), .by = c(date_sfy, state, enrollment_setting, target_group)) %>%
    filter(str_detect(state, "LAB", negate = TRUE))

  df_art_cohorts_trend_target <- df_art_cohorts_trend_target %>%
    summarise(n = sum(n, na.rm = T), .by = c(date_sfy, enrollment_setting, target_group)) %>%
    mutate(state = "USAID") %>%
    bind_rows(df_art_cohorts_trend_target, .)

  states_4_active_cohorts <- df_art_cohorts_trend_target %>%
    summarise(n = sum(n, na.rm = T), .by = state) %>%
    arrange(desc(n)) %>%
    pull(state)

  df_art_cohorts_trend_target <- df_art_cohorts_trend_target %>%
    mutate(state = factor(state, levels = states_4_active_cohorts, ordered = T),
           enrollment_setting = factor(enrollment_setting,
                                       levels = c("Community", "Facility", "Other"), ordered = T),
           target_group = factor(target_group,
                                 levels = c("GEN POP", "FSW", "MSM", "PWID", "SP",
                                            "TRANSGENDER", "PED", "CHILDREN OF KP", "PRISON", "Other"), ordered = T))


# VIZ =====

  ## Enrollment change

  df_art_cohorts %>%
    ggplot(aes(x = group, y = n, fill = group)) +
    geom_hline(yintercept = 0, color = usaid_black) +
    geom_col() +
    geom_text(aes(label = comma(n, accuracy = .1, scale_cut = cut_short_scale())),
              color = "white", size = 4.5, vjust = 1.5) +
    scale_fill_manual(
      values = c("Enrollees" = burnt_sienna,
                 "Active enrollees" = scooter,
                 "Active enr. FY13 to 23" = genoa)) +
    facet_wrap(~state, nrow = 3, scales = "free_y") +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 14, face = "bold"),
          axis.text = element_blank(),
          strip.text = element_text(size = 14, face = "bold"))

    si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 Enrollments categories.png"),
              plot = last_plot(),
              dpi = 320,
              scale = 1.8,
              width = 10,
              height = 5)

  ## Enrollment trends - OU Levels

  df_art_cohorts_trend %>%
    summarise(n = sum(n), .by = c(date_sfy, state)) %>%
    filter(state == "USAID") %>%
    ggplot(aes(x = date_sfy, y = n)) +
    geom_rect(xmin = 6.5, xmax = 10.5, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .1) +
    geom_hline(yintercept = 0, color = usaid_black) +
    geom_col(fill = genoa) +
    geom_text(aes(label = comma(n, accuracy = 1, scale_cut = cut_short_scale())),
              color = "white", size = 5, vjust = 1.5) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~state, nrow = 3) +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 14, face = "bold"))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 enrollments by year.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 5)

  ## Enrollment trends - OU & Enrollment Settings Levels

  df_art_cohorts_trend %>%
    filter(state == "USAID", enrollment_setting != "Other") %>%
    ggplot(aes(x = date_sfy, y = n,
               fill = enrollment_setting)) +
    geom_rect(xmin = 6.5, xmax = 10.5, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .1) +
    geom_hline(yintercept = 0, color = usaid_black) +
    geom_col() +
    geom_text(data = df_art_cohorts_trend %>%
                filter(state == "USAID", enrollment_setting == "Facility"),
              aes(label = percent(p, accuracy = 1),
                  group = enrollment_setting),
              color = "white", size = 7, position = position_stack(vjust = .18)) +
    geom_text(data = df_art_cohorts_trend %>%
                filter(state == "USAID", enrollment_setting == "Community", date_sfy > 2016),
              aes(label = percent(p, accuracy = 1)),
              color = "white", size = 7, vjust = -9) +
    scale_fill_manual(values = c("Facility" = genoa, "Community" = burnt_sienna)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~state, nrow = 1) +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 14, face = "bold"))

    si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 enrollments by year and setting.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 5)

    # Target Group vs Enrollment Settings

    target_groups <- c("GEN POP", "FSW", "MSM", "PWID",
                       "TRANSGENDER", "PRISON", "PED", "SP",
                       "CHILDREN OF KP", "Other")

    ## KP Groups

    df_art_cohorts_trend_target %>%
      filter(state == "USAID",
             enrollment_setting != "Other",
             target_group %in% target_groups[c(2:6, 9)]) %>%
      ggplot(aes(x = date_sfy, y = n, group = enrollment_setting, fill = enrollment_setting)) +
      geom_rect(xmin = 6.5, xmax = 10.5, ymin = 0, ymax = Inf,
                fill = trolley_grey_light, alpha = .1) +
      geom_hline(yintercept = 0, color = usaid_black) +
      geom_col() +
      scale_fill_manual(values = c("Facility" = genoa, "Community" = burnt_sienna)) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      facet_wrap(~ target_group, nrow = 2, strip.position = "top", scales = "free_y") +
      labs(x = "", y = "") +
      si_style_ygrid() +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 14, face = "bold"),
            strip.text = element_text(size = 14, face = "bold"))

    si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 KP Enrollments by year - setting and target groups.png"),
            plot = last_plot(),
            dpi = 320,
            scale = 1.8,
            width = 10,
            height = 5)

    ## GenPop and other groups

    df_art_cohorts_trend_target %>%
      filter(state == "USAID",
             enrollment_setting != "Other",
             target_group %in% target_groups[c(1, 7:8, 10)]) %>%
      ggplot(aes(x = date_sfy, y = n, group = enrollment_setting, fill = enrollment_setting)) +
      geom_rect(xmin = 6.5, xmax = 10.5, ymin = 0, ymax = Inf,
                fill = trolley_grey_light, alpha = .1) +
      geom_hline(yintercept = 0, color = usaid_black) +
      geom_col() +
      scale_fill_manual(values = c("Facility" = genoa, "Community" = burnt_sienna)) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      facet_wrap(~ target_group, nrow = 2, strip.position = "top", scales = "free_y") +
      labs(x = "", y = "") +
      si_style_ygrid() +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 14, face = "bold"),
            strip.text = element_text(size = 14, face = "bold"))

    si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 GEnPop and other Enrollments by year - setting and target groups.png"),
            plot = last_plot(),
            dpi = 320,
            scale = 1.8,
            width = 10,
            height = 5)

    ## GenPop and other groups - merge other into Gen. Pop

    df_art_cohorts_trend_target %>%
      filter(state == "USAID",
             enrollment_setting != "Other",
             target_group %in% target_groups[c(1, 7:8, 10)]) %>%
      mutate(
        target_group = case_when(
          target_group == "Other" ~ "GEN POP",
          TRUE ~ target_group
        )
      ) %>%
      ggplot(aes(x = date_sfy, y = n, group = enrollment_setting, fill = enrollment_setting)) +
      geom_rect(xmin = 6.5, xmax = 10.5, ymin = 0, ymax = Inf,
                fill = trolley_grey_light, alpha = .1) +
      geom_hline(yintercept = 0, color = usaid_black) +
      geom_col() +
      scale_fill_manual(values = c("Facility" = genoa, "Community" = burnt_sienna)) +
      scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
      facet_wrap(~ target_group, nrow = 2, strip.position = "top", scales = "free_y") +
      labs(x = "", y = "") +
      si_style_ygrid() +
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 14, face = "bold"),
            strip.text = element_text(size = 14, face = "bold"))

    si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 GEnPop and other Enrollments by year - setting and target groups2.png"),
            plot = last_plot(),
            dpi = 320,
            scale = 1.8,
            width = 10,
            height = 5)



  ## Enrollment trends - State Levels

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
          legend.text = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 14, face = "bold"))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 enrollments by state and year.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 5)

  df_art_cohorts_trend %>%
    filter(enrollment_setting != "Other") %>%
    ggplot(aes(x = date_sfy, y = n, fill = enrollment_setting)) +
    geom_rect(xmin = 6.5, xmax = 10.5, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .1) +
    geom_hline(yintercept = 0, color = usaid_black) +
    geom_col(position = position_stack()) +
    scale_fill_manual(values = c("Facility" = genoa, "Community" = burnt_sienna)) +
    scale_x_discrete(labels = yr_labels) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~state, nrow = , scales = "free_y") +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 14, face = "bold"))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 enrollments by state and year and setting.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 5)

  df_art_cohorts_trend %>%
    filter(state %in% c("USAID", "Akwa Ibom", "Lagos", "Bayelsa",
                        "Cross River", "Niger")) %>%
    filter(enrollment_setting != "Other") %>%
    ggplot(aes(x = date_sfy, y = n, fill = enrollment_setting)) +
    geom_rect(xmin = 6.5, xmax = 10.5, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .1) +
    geom_hline(yintercept = 0, color = usaid_black) +
    geom_col() +
    geom_text(data = df_art_cohorts_trend %>%
                filter(state %in% c("USAID", "Akwa Ibom", "Lagos", "Bayelsa", "Cross River", "Niger"),
                       #enrollment_setting == "Community",
                       date_sfy > 2018),
              aes(y = n, label = percent(p, accuracy = 1)),
              color = "white", size = 4,
              position = position_stack(vjust = .8)) +
    scale_fill_manual(values = c("Facility" = genoa, "Community" = burnt_sienna)) +
    scale_x_discrete(labels = yr_labels) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~state, nrow = 2, scales = "free_y") +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 14, face = "bold"))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 enrollments by state and year and setting with labels.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 5)


  ## Active Enrollees

  df_art_active_clients <- df_art_active %>%
    filter(date_sfy != meta$curr_fy +1,
           !is.na(enrollment_setting)) %>%
    summarise(n = n_distinct(patientuid),
              .by = c(state, enrollment_setting))

  df_art_active_clients <- df_art_active_clients %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = T)),
              .by = enrollment_setting) %>%
    mutate(state = agency) %>%
    bind_rows(df_art_active_clients, .)

  df_art_active_clients <- df_art_active_clients %>%
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = T)),
              .by = c(state)) %>%
    mutate(enrollment_setting = "Total") %>%
    bind_rows(df_art_active_clients, .)

  df_art_active_clients <- df_art_active_clients %>%
    group_by(state) %>%
    mutate(p = n / n[enrollment_setting == "Total"]) %>%
    ungroup()

  df_art_active_clients <- df_art_active_clients %>%
    mutate(enrollment_setting = factor(enrollment_setting,
                                       levels = c("Facility", "Community", "Total")))


  df_art_active_clients %>%
    group_by(state) %>%
    mutate(
      lbl_color = case_when(
        #n > 10000 ~ "white",
        #TRUE ~ usaid_darkgrey,
        n[enrollment_setting == "Community"] - n[enrollment_setting == "Facility"] > 0 & enrollment_setting == "Community" ~ "white",
        TRUE ~ grey90k
      ),
      state = paste0(state, " (", comma(n[enrollment_setting == "Total"]), ")")
    ) %>%
    ungroup() %>%
    filter(str_detect(state, agency, negate = T),
           enrollment_setting != "Total") %>%
    ggplot(aes(x = enrollment_setting, y = reorder(state, n), fill = n)) +
    geom_tile(color = grey10k, linewidth = .1, show.legend = F) +
    geom_text(aes(label = percent(p, accuracy = 1), color = lbl_color), size = 8) +
    scale_x_discrete(position = "top") +
    scale_fill_si(palette = "genoas", na.value = trolley_grey_light) +
    scale_color_identity() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.text.x = element_text(size = 18, face = "bold"),
          axis.text.y = element_text(size = 15, face = "bold"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 Table Active Enrollees by enrollment settings.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.5,
          width = 10,
          height = 5)

  ## Pickup pattern ----

  ## Lga level pickups for Akwa Ibom

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    arrange(lga, desc(n)) %>%
    mutate(lga = paste0(lga, " (", state, ")")) %>%
    ggplot(aes(x = date_last_pickup, y = reorder(lga, n), fill = n)) +
    geom_tile(color = grey10k, linewidth = .1) +
    scale_x_date(breaks = "1 weeks", date_labels = "%a\n%d %b\n%Y", sec.axis = dup_axis()) +
    scale_fill_si(palette = "burnt_siennas", na.value = grey80k,
                  breaks = seq(0, 600, 100), limits = c(0, 600)) +
    coord_equal(ratio = 1) +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 Akwa Ibom - ARV Pickup by lga.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.5,
          width = 10,
          height = 5)

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(date_lfy, date_lpd, date_lmonth, date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    arrange(date_last_pickup, desc(n)) %>%
    ggplot(aes(x = date_last_pickup, y = n, fill = n)) +
    geom_col() +
    scale_x_date(breaks = "1 weeks", date_labels = "%a\n%d %b\n%Y", expand = c(0, 0)) +
    scale_y_continuous(expand = c(.005, 0)) +
    scale_fill_si(palette = "burnt_siennas", na.value = grey80k,
                  breaks = seq(0, 4500, 500), limits = c(0, 4500)) +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1),
          panel.grid = element_blank(),
          panel.border = element_blank())

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(date_lfy, date_lpd, date_lweek)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    arrange(date_lweek, desc(n)) %>%
    ggplot(aes(x = date_lweek, y = n, fill = n)) +
    geom_col() +
    scale_x_date(breaks = "1 weeks", date_labels = "%a\n%d %b\n%Y", expand = c(0, 0)) +
    scale_y_continuous(expand = c(.005, 0)) +
    scale_fill_si(palette = "burnt_siennas", na.value = grey80k,
                  breaks = seq(0, 11000, 1000), limits = c(0, 11000)) +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1),
          panel.grid = element_blank(),
          panel.border = element_blank())


  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(date_lfy, date_lpd, date_lmonth)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    arrange(date_lmonth, desc(n)) %>%
    ggplot(aes(x = date_lmonth, y = n, fill = n)) +
    geom_col() +
    scale_x_date(breaks = "1 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
    scale_y_continuous(expand = c(.005, 0)) +
    scale_fill_si(palette = "burnt_siennas", na.value = grey80k,
                  breaks = seq(0, 42000, 5000), limits = c(0, 42000)) +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1),
          panel.grid = element_blank(),
          panel.border = element_blank())

  ## Distribution of pickup volumes ----

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>%
    arrange(lga, desc(n)) %>%
    mutate(lga = paste0(lga, " (", state, ")"))

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, date_lfy, date_lpd, date_lmonth,
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

  ## facility level ----

  ## High Vol Sites - USAID ----

  df_sites_vol <- df_art_pickups %>%
    filter(str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(lga, facility_name)) %>%
    arrange(desc(n))

  hvol_sites <- df_sites_vol %>%
    filter(n >= 1000) %>%
    distinct(facility_name) %>%
    pull()

  t10_sites <- df_sites_vol %>%
    filter(row_number() <= 10) %>%
    distinct(facility_name) %>%
    pull()

  ## High Vol Sites - Akwa Ibom ----

  df_ai_sites_vol <- df_art_pickups %>%
    filter(state == "Akwa Ibom",
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(lga, facility_name)) %>%
    arrange(desc(n))

  ai_hvol_sites <- df_ai_sites_vol %>%
    filter(n >= 1000) %>%
    distinct(facility_name) %>%
    pull()

  ai_t10_sites <- df_ai_sites_vol %>%
    filter(row_number() <= 10) %>%
    distinct(facility_name) %>%
    pull()

  ## High Volume sites ----

  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           lga %in% c("Mbo", "Eket", "Uyo"),
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(lga, facility_name, date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>%
    mutate(facility_name = paste0(facility_name, " (", lga, ")")) %>%
    ggplot(aes(x = date_last_pickup, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = grey10k, linewidth = .1) +
    scale_x_date(breaks = "1 weeks", sec.axis = dup_axis()) +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light) +
    labs(x = "", y = "") +
    si_style_nolines() +
    coord_equal(ratio = 1) +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 Akwa Ibom - ARV Pickup by site.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.5,
          width = 10,
          height = 5)


  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           facility_name %in% ai_t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(lga, facility_name, date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>%
    mutate(facility_name = paste0(facility_name, " (", lga, ")")) %>%
    ggplot(aes(x = date_last_pickup, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = grey10k, linewidth = .1) +
    scale_x_date(breaks = "1 weeks", sec.axis = dup_axis()) +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light) +
    labs(x = "", y = "") +
    si_style_nolines() +
    coord_equal() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 Akwa Ibom - ARV Pickup t10 site.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.5,
          width = 10,
          height = 5)


  df_art_pickups %>%
    filter(state == "Akwa Ibom",
           facility_name %in% ai_t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(lga, facility_name, enrollment_setting, date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023",
           date_lpd == "FY23Q4",
           enrollment_setting %in% c("Facility", "Community")
           ) %>%
    mutate(facility_name = paste0(facility_name, " (", lga, ")")) %>%
    ggplot(aes(x = date_last_pickup, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = grey10k, linewidth = .1) +
    scale_x_date(breaks = "1 weeks", sec.axis = dup_axis()) +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light) +
    facet_wrap(~enrollment_setting, ncol = 1) +
    labs(x = "", y = "") +
    si_style_xline() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 Akwa Ibom - ARV Pickup t10 site by setting.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.5,
          width = 10,
          height = 5)

  ## USAID Top 10 ----

  df_art_pickups %>%
    filter(facility_name %in% t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, facility_name, date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    mutate(
      facility_name = case_when(
        nchar(facility_name) > 15 ~ paste(str_sub(facility_name, 1, 15), "..."),
        TRUE ~ facility_name
      ),
      #facility_name = paste0(facility_name, " (", state, ": ", lga, ")")
      facility_name = paste0(facility_name, " (", state, ")")
    ) %>%
    ggplot(aes(x = date_last_pickup, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = "white", linewidth = .2) +
    scale_x_date(breaks = "1 weeks", sec.axis = dup_axis(),
                 date_breaks = "1 weeks", date_labels = "%a\n%d %b\n%Y") +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light,
                  breaks = seq(0, 500, 50), limits = c(0, 500)) +
    coord_equal() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(aspect.ratio = .2,
          strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.text = element_text(size = 10),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 USAID - ARV Daily Pickup t10 site.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 4)


  df_art_pickups %>%
    filter(facility_name %in% t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    ggplot(aes(x = date_last_pickup, y = n, fill = n)) +
    geom_col() +
    scale_x_date(breaks = "1 weeks",
                 date_breaks = "1 weeks",
                 date_labels = "%a\n%d %b\n%Y", expand = c(0, 0)) +
    scale_y_continuous(labels = comma, expand = c(.005, 0)) +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light,
                  breaks = seq(0, 2000, 500), limits = c(0, 2000)) +
    labs(x = "", y = "") +
    si_style_ygrid() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.text = element_text(size = 10),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))


  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 USAID - ARV Cumulative Daily Pickup t10 site.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 2)


  df_art_pickups %>%
    filter(facility_name %in% t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, facility_name, date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    mutate(
      facility_name = case_when(
        nchar(facility_name) > 15 ~ paste(str_sub(facility_name, 1, 15), "..."),
        TRUE ~ facility_name
      ),
      #facility_name = paste0(facility_name, " (", state, ": ", lga, ")")
      facility_name = paste0(facility_name, " (", state, ")"),
      #date_lmonth = factor(date_lmonth, factor(levels = c(ymd("2023-07-01"), ymd("2023-08-01"), ymd("2023-09-01")), ordered = T))
    ) %>%
    ggplot(aes(x = date_last_pickup, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = "white", linewidth = .2) +
    scale_x_date(breaks = "1 weeks", sec.axis = dup_axis(),
                 date_breaks = "1 weeks", date_labels = "%a\n%d %b") +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light,
                  breaks = seq(0, 500, 50), limits = c(0, 500)) +
    facet_wrap(~format(date_lmonth, "%b %Y"), nrow = 3, scales = "free_x") +
    #coord_equal() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(aspect.ratio = .2,
          strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.text = element_text(size = 8),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 USAID - ARV Daily Pickup t10 site2.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 7,
          height = 10)



  df_art_pickups %>%
    filter(facility_name %in% t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, facility_name, enrollment_setting,
              date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    mutate(
      facility_name = case_when(
        nchar(facility_name) > 15 ~ paste(str_sub(facility_name, 1, 15), "..."),
        TRUE ~ facility_name
      ),
      #facility_name = paste0(facility_name, " (", state, ": ", lga, ")")
      facility_name = paste0(facility_name, " (", state, ")")
    ) %>%
    ggplot(aes(x = date_last_pickup, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = "white", linewidth = .2) +
    scale_x_date(breaks = "1 weeks", sec.axis = dup_axis(),
                 date_breaks = "1 weeks", date_labels = "%a\n%d %b\n%Y") +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light,
                  breaks = seq(0, 500, 50), limits = c(0, 500)) +
    facet_wrap(~enrollment_setting, nrow = 2) +
    coord_equal() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.text = element_text(size = 9),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 USAID - ARV Daily Pickup t10 site by setting.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 5)

  # Pickup by cohorts

  2013:2023 %>%
    walk(function(.year) {

      print(.year)

      viz_cohort_pickup <- df_art_pickups %>%
        filter(facility_name %in% t10_sites,
               str_detect(str_to_lower(status_current), "active"),
               as.integer(date_sfy) == .year
        ) %>%
        summarise(
          n = n_distinct(patientuid),
          .by = c(state, lga, facility_name, enrollment_setting,
                  date_lfy, date_lpd, date_lmonth,
                  date_lweek, date_last_pickup)) %>%
        filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
        mutate(
          facility_name = case_when(
            nchar(facility_name) > 15 ~ paste(str_sub(facility_name, 1, 15), "..."),
            TRUE ~ facility_name
          ),
          #facility_name = paste0(facility_name, " (", state, ": ", lga, ")")
          facility_name = paste0(facility_name, " (", state, ")")
        ) %>%
        ggplot(aes(x = date_last_pickup, y = reorder(facility_name, n), fill = n)) +
        geom_tile(color = "white", linewidth = .2) +
        scale_x_date(breaks = "1 weeks", sec.axis = dup_axis(),
                     date_breaks = "1 weeks", date_labels = "%a\n%d %b\n%Y") +
        scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light,
                      #breaks = seq(0, 500, 50), limits = c(0, 500)
                      ) +
        facet_wrap(~enrollment_setting, nrow = 2) +
        coord_equal() +
        labs(x = "", y = "") +
        si_style_nolines() +
        theme(strip.text = element_text(face = "bold", size = 15),
              strip.placement = "outside",
              legend.title = element_blank(),
              legend.key.width = unit(3, "cm"),
              legend.key.height = unit(.4, "cm"),
              axis.text = element_text(size = 9),
              axis.line.x = element_line(color = usaid_black),
              axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

      si_save(filename = file.path(dir_graphics, glue::glue("USAID - FY23Q4 USAID - ARV Daily Pickup for Cohort {.year} t10 site by setting.png")),
              plot = last_plot(),
              dpi = 320,
              scale = 1.8,
              width = 10,
              height = 5)
    })


  df_art_pickups %>%
    filter(facility_name %in% t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023),
           enrollment_setting == "Community",
           str_detect(dsd_code, "CBM(1|2|3)")
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, facility_name, enrollment_setting,
              dsd_code, dsd_model_ref,
              date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    mutate(
      facility_name = case_when(
        nchar(facility_name) > 15 ~ paste(str_sub(facility_name, 1, 15), "..."),
        TRUE ~ facility_name
      )
    ) %>%
    ggplot(aes(x = date_last_pickup, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = "white", linewidth = .2) +
    scale_x_date(breaks = "1 weeks", sec.axis = dup_axis(),
                 date_breaks = "1 weeks", date_labels = "%a\n%d %b\n%Y") +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light,
                  #breaks = seq(0, 500, 50), limits = c(0, 500)
                  ) +
    facet_wrap(~dsd_model_ref, ncol = 1) +
    coord_equal() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(aspect.ratio = .1,
          strip.text = element_text(face = "bold", size = 10),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.text = element_text(size = 10),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 USAID - ARV Daily Pickup t10 site in community setting and dsd CBM1-3.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 5)

  df_art_pickups %>%
    filter(facility_name %in% t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023),
           enrollment_setting == "Community",
           str_detect(dsd_code, "CBM(4|5|6)")
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, facility_name, enrollment_setting,
              dsd_code, dsd_model_ref,
              date_lfy, date_lpd, date_lmonth,
              date_lweek, date_last_pickup)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    mutate(
      facility_name = case_when(
        nchar(facility_name) > 15 ~ paste(str_sub(facility_name, 1, 15), "..."),
        TRUE ~ facility_name
      )
    ) %>%
    ggplot(aes(x = date_last_pickup, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = "white", linewidth = .2) +
    scale_x_date(breaks = "1 weeks", sec.axis = dup_axis(),
                 date_breaks = "1 weeks", date_labels = "%a\n%d %b\n%Y") +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light,
                  #breaks = seq(0, 500, 50), limits = c(0, 500)
    ) +
    facet_wrap(~dsd_model_ref, ncol = 1) +
    coord_equal() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(aspect.ratio = .1,
          strip.text = element_text(face = "bold", size = 10),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.text = element_text(size = 10),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 USAID - ARV Daily Pickup t10 site in community setting and dsd CBM4-6.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.8,
          width = 10,
          height = 5)

  df_art_pickups %>%
    filter(facility_name %in% t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, facility_name, enrollment_setting,
              date_lfy, date_lyear, date_lpd, date_lmonth, date_lweek)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    mutate(
      facility_name = case_when(
        nchar(facility_name) > 15 ~ paste(str_sub(facility_name, 1, 15), "..."),
        TRUE ~ facility_name
      ),
      #facility_name = paste0(facility_name, " (", state, ": ", lga, ")")
      facility_name = paste0(facility_name, " (", state, ")")
    ) %>%
    ggplot(aes(x = date_lweek, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = grey10k, linewidth = .1) +
    scale_x_date(breaks = "1 weeks", date_labels = "%a\n%d %b\n%Y", sec.axis = dup_axis()) +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light,
                  breaks = seq(0, 800, 100), limits = c(0, 800)) +
    facet_wrap(~enrollment_setting, nrow = 2) +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 USAID - ARV Weekly Pickup t10 site by setting.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.5,
          width = 10,
          height = 7)


  df_art_pickups %>%
    filter(facility_name %in% t10_sites,
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023)
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(state, lga, facility_name, enrollment_setting,
              date_lfy, date_lyear, date_lpd, date_lmonth)) %>%
    filter(date_lfy == "2023", date_lpd == "FY23Q4") %>% #pull(n) %>% max()
    mutate(
      facility_name = case_when(
        nchar(facility_name) > 15 ~ paste(str_sub(facility_name, 1, 15), "..."),
        TRUE ~ facility_name
      ),
      #facility_name = paste0(facility_name, " (", state, ": ", lga, ")")
      facility_name = paste0(facility_name, " (", state, ")")
    ) %>%
    ggplot(aes(x = date_lmonth, y = reorder(facility_name, n), fill = n)) +
    geom_tile(color = grey10k, linewidth = .1) +
    scale_x_date(breaks = "1 months", date_labels = "%b %Y", sec.axis = dup_axis()) +
    scale_fill_si(palette = "burnt_siennas", na.value = trolley_grey_light,
                  breaks = seq(0, 2500, 500), limits = c(0, 2500)) +
    facet_wrap(~enrollment_setting, nrow = 2) +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(strip.text = element_text(face = "bold", size = 15),
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.key.width = unit(3, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.line.x = element_line(color = usaid_black),
          axis.ticks.x = element_line(color = usaid_black, linewidth = 1))

  si_save(filename = file.path(dir_graphics, "USAID - FY23Q4 USAID - ARV Monthly Pickup t10 site by setting.png"),
          plot = last_plot(),
          dpi = 320,
          scale = 1.5,
          width = 10,
          height = 7)



  ## top 10 facilities pickup pattern ----

  fac_top10 <- df_art_pickups %>%
    filter(state == "Akwa Ibom",
           lga %in% c("Mbo", "Eket", "Uyo"),
           str_detect(str_to_lower(status_current), "active"),
           between(as.integer(date_sfy), 2013, 2023),
           date_lfy == "2023",
           date_lpd == "FY23Q4"
    ) %>%
    summarise(
      n = n_distinct(patientuid),
      .by = c(lga, facility_name, orgunituid)) %>%
    filter(n >= 1000) %>%
    pull(orgunituid)

  c(2013:2023) %>%
    walk(function(.year) {

      print(.year)

      plot_pickup <- df_art_pickups %>%
        filter(state == "Akwa Ibom",
               lga %in% c("Mbo", "Eket", "Uyo"),
               str_detect(str_to_lower(status_current), "active"),
               date_sfy == .year,
               date_lfy == "2023",
               date_lpd == "FY23Q4"
        ) %>%
        summarise(
          n = n_distinct(patientuid),
          .by = c(lga, facility_name, date_lfy, date_lpd, date_last_pickup)
        ) %>%
        mutate(facility_name = paste0(facility_name, " (", lga, ")")) %>%
        ggplot(aes(x = date_last_pickup,
                   y = reorder(facility_name, n), fill = n)) +
        geom_tile(color = grey10k, linewidth = .1) +
        scale_x_date(breaks = "1 weeks") +
        scale_fill_si(palette = "burnt_siennas",
                      na.value = trolley_grey_light) +
        labs(x = "", y = "",
             title = "USAID - IKWA IBOM ARV PICKUP PATTERNS",
             subtitle = glue::glue("ART start year = {.year}, Pickup Period = FY23Q4")) +
        si_style_nolines() +
        theme(plot.title = element_markdown(),
              plot.subtitle = element_markdown(),
              legend.title = element_blank(),
              legend.key.width = unit(3, "cm"),
              legend.key.height = unit(.4, "cm"))

      si_save(filename = file.path(dir_graphics, glue::glue("USAID - Cohort {.year} - Reporting FY23Q4 for Akwa Ibom - ARV Pickup by high volume site.png")),
              plot = plot_pickup,
              dpi = 320,
              scale = 1.5,
              width = 10,
              height = 5)
    })



  ### ----
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

