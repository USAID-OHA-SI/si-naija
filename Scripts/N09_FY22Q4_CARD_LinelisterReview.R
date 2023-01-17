##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Patients Linelister Validation
##  LICENCE: MIT
##  DATE:    2022-10-11
##  UPDATED: 2022-10-11

# Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(grabr)
  library(glamr)
  library(gisr)
  library(sf)
  library(janitor)
  library(glue)
  library(gt)
  library(ggtext)
  library(patchwork)
  library(lubridate)

  # library(fontawesome)
  # library(emojifont)
  # library(extrafont)
  # font_import()

  source("./Scripts/N00_Utilities.R")
  source("./Scripts/N00_Viz_Utilities.R")

# GLOBALS ----

  ## Dirs ----

  dir_merdata <- si_path("path_msd")
  dir_llister <- "../../PEPFAR/COUNTRIES/Nigeria/Linelister"
  dir_geodata <- si_path("path_vector")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_images <- "Images"

  #dir_llister %>% open_path()

  ## Files ----

  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY20.*_Nig", recursive = T)

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*_Nig")

  file_subnat <- dir_merdata %>%
    return_latest(pattern = "NAT_SUBNAT")

  file_radet <- dir_llister %>%
    file.path("data") %>%
    list.files(pattern = "FY.*RADET") %>%
    str_extract("\\d{2}-\\d{2}-\\d{4}") %>%
    dmy() %>%
    max(na.rm = T) %>%
    format("%d-%m-%Y") %>%
    list.files(path = file.path(dir_llister, "data"),
               pattern = .,
               full.names = T)

  file_date <- file_radet %>%
    str_extract("\\d{2}-\\d{2}-\\d{4}") %>%
    dmy()

  # Get last day of the previous month
  rep_end_date <- rollback(file_date)


  #open_path(file_radet)

  ## Params ----

  agency <- "USAID"

  cntry <- "Nigeria"

  ou_uid <- get_ouuid(cntry)

  fac_level <- get_ouorglevel(operatingunit = cntry, org_type = "facility")

  src_msd <- file_psnu_im %>% source_info(return = "source")
  curr_pd <- file_psnu_im %>% source_info(return = "period")
  curr_fy <- file_psnu_im %>% source_info(return = "fiscal_year")

  curr_fy_start <- curr_fy %>%
    "-"(1) %>%
    paste0(., "-10-01") %>%
    ymd()

  curr_fy_end <- curr_fy %>%
    paste0(., "-09-30") %>%
    ymd()

  ## ART Status
  status = list(
    on = c("active", "active-new", "active-current", "active-transfer in"),
    off = c("stopped", "iit"),
    unknown = c("transferred out")
  )

# TASKS ----

  ## 1. Active Clients
  ## 1.1. Active (disagg by Age/Sex, State, IM/IP)
  ## 1.2. Active-restart
  ## 1.3. Active-Xfer in
  ## 1.4. ML (IIT, Stopped, Xfer Out, Died)
  ## 2. New Clients
  ## 3. Clients with VL Results
  ## 4. Clients with VL Results showing suppression


# FUNCTIONS ----

  #' Patient's Age Bands
  #'
  calculate_age <- function(.data, rep_date = NULL) {

    curr_date <- ymd(Sys.Date())

    if(is.null(rep_date)) rep_date <- curr_date()

    .data %>%
      mutate(
        date_of_birth = ymd(date_of_birth),
        date_of_birth = case_when(
          is.Date(date_of_birth) | date_of_birth < rep_date ~ date_of_birth,
          TRUE ~ NA_real_
        ),
        age = trunc((date_of_birth %--% rep_date) / years(1)),
        age_at_art_start = case_when(
          date_of_birth < art_start_date ~ trunc((date_of_birth %--% art_start_date) / years(1)),
          TRUE ~ NA_real_
        ),
        age_coarse = case_when(
          age < 15 ~ "<15",
          age > 15 ~ "15+",
          TRUE ~ NA_character_),
        age_group = case_when(
          age < 15 ~ "<15",
          age > 15 & age <= 19 ~ "15-19",
          age > 19 & age <= 24 ~ "20-24",
          age > 24 ~ "25+",
          TRUE ~ NA_character_)
      ) %>%
      relocate(age, age_coarse, age_group, age_at_art_start, .after = date_of_birth)
  }

  #' Patient's Appointments
  #'
  check_appointments <- function(.data, rep_date = NULL) {

    curr_date <- ymd(Sys.Date())

    if(is.null(rep_date)) rep_date <- curr_date()

    .data %>%
      mutate(
        days_of_arv_refill = months_of_arv_refill * 30,
        next_pickup_date = ymd(last_pickup_date) + days(days_of_arv_refill),
        missed_apptmt = next_pickup_date < rep_date,
        days_since_missed_apptmt = case_when(
          missed_apptmt == TRUE ~ trunc((next_pickup_date %--% rep_date) / days(1)),
          missed_apptmt != TRUE ~ 0,
          TRUE ~ NA_real_
        ),
        days_since_last_pickup = case_when(
          missed_apptmt != TRUE ~ trunc((last_pickup_date %--% rep_date) / days(1)),
          TRUE ~ NA_real_
        ),
        days_to_next_pickup = case_when(
          missed_apptmt != TRUE ~ days_of_arv_refill - days_since_last_pickup,
          TRUE ~ NA_real_
        )
      )
  }

  #' Patient's ART Status
  #'
  check_art_status <- function(.data, rep_date = NULL) {

    curr_date = ymd(Sys.Date())

    if (!is.null(rep_date)) rep_date <- curr_date

    .data %>%
      mutate(
        art_status = case_when(
          str_detect(current_art_status, "Active") & days_since_missed_apptmt <= 28 ~ "On",
          str_detect(current_art_status, "Active") & days_since_missed_apptmt > 28 ~ "Off",
          str_to_lower(current_art_status) %in% status$off ~ "Off",
          str_to_lower(current_art_status) == "dead" ~ NA_character_,
          TRUE ~ NA_character_
        ),
        years_on_art = case_when(
          art_status == "On" & days_since_missed_apptmt == 0 ~  round((art_start_date %--% rep_date) / years(1), 1),
          art_status == "On" & days_since_missed_apptmt > 0 ~ round((art_start_date %--% next_pickup_date) / years(1), 1),
          art_status == "Off" ~ round((art_start_date %--% last_pickup_date) / years(1), 1),
          TRUE ~ NA_real_
        ),
        months_on_art = case_when(
          art_status == "On" & days_since_missed_apptmt == 0 ~  round((art_start_date %--% rep_date) / months(1), 1),
          art_status == "On" & days_since_missed_apptmt > 0 ~ round((art_start_date %--% next_pickup_date) / months(1), 1),
          art_status == "Off" ~ round((art_start_date %--% last_pickup_date) / months(1), 1),
          TRUE ~ NA_real_
        ),
        days_on_art = case_when(
          art_status == "On" & days_since_missed_apptmt == 0 ~  round((art_start_date %--% rep_date) / days(1), 1),
          art_status == "On" & days_since_missed_apptmt > 0 ~ round((art_start_date %--% next_pickup_date) / days(1), 1),
          art_status == "Off" ~ round((art_start_date %--% last_pickup_date) / days(1), 1),
          TRUE ~ NA_real_
        )
      )
  }


  #' VL Testing Eligibility
  #'
  check_vl_testing <- function(.data) {
    .data %>%
      mutate(
        current_regimen_type = case_when(
          str_detect(str_to_lower(current_art_regimen),
                     "^tdf[^[:alnum:]]3tc[^[:alnum:]]dtg$") ~ "TLD",
          TRUE ~ "Non-TLD"
        ),
        time_for_vl_test = case_when(
          art_status == "On" & current_regimen_type == "TLD" & days_on_art <= 90 ~ "No",
          art_status == "On" & current_regimen_type == "TLD" & days_on_art > 90 ~ "Yes",
          art_status == "On" & current_regimen_type != "TLD" & days_on_art <= 180 ~ "No",
          art_status == "On" & current_regimen_type != "TLD" & days_on_art > 180 ~ "Yes",
          TRUE ~ NA_character_
        )
      )
  }

  #' VL Testing Validity
  #'
  check_vl_validity <- function(.data, rep_date = NULL) {

    curr_date = ymd(Sys.Date())

    if (!is.null(rep_date)) rep_date <- curr_date

    .data %>%
      mutate(
        days_since_last_vl_sample = round((date_of_viral_load_sample_collection %--% rep_date) / days(1), 1),
        is_vl_sample_valid = case_when(
          days_since_last_vl_sample <= 365 ~ "Yes",
          days_since_last_vl_sample > 365 ~ "No",
          TRUE ~ NA_character_
        ),
        days_for_vl_results = round((date_of_viral_load_sample_collection %--% date_of_current_viral_load) / days(1), 1),
        days_since_last_vl_results = case_when(
          days_for_vl_results > 0 ~ round((date_of_current_viral_load %--% rep_date) / days(1), 1),
          TRUE ~ NA_real_
        ),
        is_vl_results_valid = case_when(
          days_since_last_vl_results <= 365 ~ "Yes",
          days_since_last_vl_results > 365 ~ "No",
          TRUE ~ NA_character_
        )
      )
  }

# IMPORT Datasets ----

  # df_site <- file_site_im %>%
  #   read_msd()
  #
  # df_psnu <- file_psnu_im %>%
  #   read_msd() %>%
  #   filter(fiscal_year == curr_fy,
  #          funding_agency == agency,
  #          str_detect(indicator, "TX_.*"))
  #
  # df_psnu %>%
  #   filter(!is.na(targets))

  #file_radet %>% open_path()

  df_radet <- read_excel(file_radet, sheet = 1)

  df_cols <- df_radet %>%
    names() %>%
    tibble(colname = .)

  cols_notes <- c(
    " \\(Kg\\)",
    "\\(c/ml\\)",
    "\\(new, relapsed etc\\)",
    "\\(yyyy-mm-dd\\)",
    " \\(yyyy-mm-dd",
    "yyyy/mm/dd \\(Enter format as specified\\)",
    "\\(Hexadecimal\\/Base64 Unique Identifier\\)")

  cols_notes <- paste(cols_notes, collapse = "|")

  df_radet <- df_radet %>%
    rename_with(
      .fn = ~str_replace(., cols_notes, ""),
      .cols = everything()) %>%
    rename_with(~str_trim(., side = "both")) %>%
    rename(lga = `L.G.A`) %>%
    clean_names()

  df_cols <- df_cols %>%
    mutate(name = names(df_radet))

  # Patients List
  df_patients <- df_radet %>%
    distinct(patient_id, patients_hospital_number, ip_name)

  # HIV TX Services
  df_services <- df_radet %>%
    distinct(state, lga, patients_hospital_number, facility_name, ip_name)

  # Partners List
  df_partners <- df_services %>%
    distinct(ip_name, patients_hospital_number)

  # List of Health Facilities
  df_orgs <- df_services %>%
    distinct(state, lga, patients_hospital_number, facility_name)

  # List of LGAs
  df_lga <- df_services %>%
    distinct(state, lga)

  # IM TX Report
  df_radet_im <- df_radet %>%
    dplyr::filter(ip_name == first(df_radet$ip_name)) %>%
    select(patient_id, art_enrollment_setting, sex, date_of_birth,
           art_start_date, last_pickup_date, months_of_arv_refill,
           current_art_status, date_of_current_art_status,
           current_regimen_line, current_art_regimen,
           current_art_regimen_start_date = date_of_start_of_current_art_regimen,
           previous_art_status, confirmed_date_of_previous_art_status,
           regimen_line_at_art_start, regimen_at_art_start,
           date_of_viral_load_sample_collection, current_viral_load,
           date_of_current_viral_load, viral_load_indication,
           dsd_model, date_commenced_dsd, date_of_return_of_dsd_client_to_facility)

# MUNGING ----

  ## TX Eligibility
  df_radet_im %>%
    calculate_age(rep_date = file_date)


  df_radet_im_clean <- df_radet_im %>%
    filter(last_pickup_date > curr_fy_start) %>%
    calculate_age(rep_date = file_date) %>%
    check_appointments(rep_date = file_date) %>%
    check_art_status(rep_date = file_date) %>%
    check_vl_testing() %>%
    check_vl_validity(rep_date = file_date) %>%
    relocate(years_on_art, months_on_art, days_on_art,
             .after = age_at_art_start) %>%
    relocate(days_of_arv_refill, next_pickup_date,
             missed_apptmt, days_since_missed_apptmt,
             days_since_last_pickup, days_to_next_pickup,
             art_status,
             .after = months_of_arv_refill) %>%
    relocate(current_regimen_type, .before = current_regimen_line) %>%
    relocate(time_for_vl_test, days_since_last_vl_sample,
             is_vl_sample_valid, days_for_vl_results,
             days_since_last_vl_results, is_vl_results_valid,
             .after = date_of_current_viral_load)

  df_radet_im_clean %>%
    distinct(current_art_status)

  df_radet_im_clean %>%
    distinct(previous_art_status)

  arv_codes <- df_radet_im_clean %>%
    distinct(current_art_regimen) %>%
    separate(current_art_regimen,
             into = paste0("code", 1:3),
             sep = "-|\\+")

  arv_codes %>%
    pivot_longer(cols = everything(),
                 names_to = "position",
                 values_to = "l3code") %>%
    distinct(l3code) %>%
    arrange(l3code) %>%
    mutate(
      l1code = case_when(
        l3code == "3TC" ~ "L",
        l3code == "ABC" ~ "A",
        l3code == "ATV/r" ~ "",
        l3code == "AZT" ~ "Z",
        l3code == "DTG" ~ "D",
        l3code == "EFV" ~ "E",
        l3code == "LPV/r" ~ "",
        l3code == "NVP" ~ "N",
        l3code == "TDF" ~ "T",
        TRUE ~ NA_character_
      )
    )


# EXPLORE ----

  ## Verify uniqueness

  df_radet_im %>%
    count(ip_name, state, patient_id) %>%
    arrange(desc(n)) %>%
    filter(n > 1)

  ## Active Patients

  df_radet_im %>%
    distinct(current_art_status) %>%
    arrange(desc(current_art_status))

  df_radet_im %>%
    distinct(current_art_status)

  df_radet_im %>%
    count(ip, state, current_art_status) %>%
    arrange(desc(n)) %>%
    filter(n > 1)

  df_radet_im %>%
    filter(date_of_current_art_status > rep_end_date) %>%
    distinct(date_of_current_art_status) %>%
    arrange()

  df_radet_im %>%
    filter(date_of_current_art_status > rep_end_date) %>%
    count(ip, state, current_art_status) %>%
    prinf()

  ## ARV Regiments

  ### Start of ART
  df_radet %>%
    distinct(ip_name, state, regimen_at_art_start, regimen_line_at_art_start) %>%
    separate(regimen_line_at_art_start, into = c("patient_type", "line", "note")) %>%
    arrange(ip_name, state, patient_type, line)

  ### Current ART
  df_radet %>%
    distinct(ip_name, state, current_art_regimen, current_regimen_line) %>%
    separate(current_regimen_line, into = c("patient_type", "line", "note")) %>%
    arrange(ip_name, state, patient_type, line)

  df_radet %>%
    distinct(current_art_regimen, current_regimen_line) %>%
    arrange(current_regimen_line, current_art_regimen)

  df_radet %>%
    distinct(current_art_regimen, current_regimen_line) %>%
    separate(current_regimen_line, into = c("patient_type", "line", "note")) %>%
    arrange(patient_type, line)

  ## Number of Months of ARV Refill
  df_radet_im %>%
    count(ip, state, sex, months_of_arv_refill) %>%
    arrange(state, desc(n)) %>%
    pivot_wider(names_from = sex, values_from = n)

  ##

  df_radet_im %>%
    select(ip, state, lga, patient_id, date_of_birth, sex,
           age, age_coarse, age_group,
           art_start_date, art_start_age, art_length, art_enrollment_setting,
           last_pickup_date, months_of_arv_refill, months_since_arv_pickup,
           months_of_arv_remaining) %>% head()

# VIZ ----

  df_p <- df_radet_im_clean %>%
    filter(row_number() == 2) %>%
    pivot_longer(cols = -patient_id,
                 names_to = "var",
                 values_to = "value",
                 values_transform = as.character)

  df_p %>%
    filter(str_detect(var, "^date_.*|.*_date$"),
           !is.na(value),
           var != "date_of_birth") %>%
    mutate(value = as.Date(value)) %>%
    ggplot(aes(x = value, y = 1, label = paste(var, value))) +
    geom_hline(yintercept = 1, size = 20, color = scooter) +
    geom_point(shape = 21, size = 15, color = usaid_black, fill = "white") +
    geom_text(vjust = -3, angle = 45)



