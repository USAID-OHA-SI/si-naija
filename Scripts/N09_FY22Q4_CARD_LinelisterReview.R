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

  dir_llister %>% open_path()

  ## Files ----

  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY20.*_Nig")

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*_Nig")

  file_subnat <- dir_merdata %>%
    return_latest(pattern = "NAT_SUBNAT")

  file_radet <- dir_llister %>%
    file.path("data") %>%
    return_latest("RADET")

  file_date <- file_radet %>%
    str_extract("(?<=RADET-).*(?=.xlsx)") %>%
    dmy()

  #open_path(file_radet)

  file_shp <- dir_geodata %>%
    return_latest(pattern = "VcPepfarPolygons")

  ## Params ----

  agency <- "USAID"

  cntry <- "Nigeria"

  ou_uid <- get_ouuid(cntry)

  fac_level <- get_ouorglevel(operatingunit = cntry, org_type = "facility")

  src_msd <- file_psnu_im %>% source_info(return = "source")
  curr_pd <- file_psnu_im %>% source_info(return = "period")
  curr_fy <- file_psnu_im %>% source_info(return = "fiscal_year")

# TASKS ----

  ## 1. Active Clients
  ## 1.1. Active (disagg by Age/Sex, State, IM/IP)
  ## 1.2. Active-restart
  ## 1.3. Active-Xfer in
  ## 1.4. ML (IIT, Stopped, Xfer Out, Died)
  ## 2. New Clients
  ## 3. Clients with VL Results
  ## 4. Clients with VL Results showing suppression


# IMPORT Datasets ----

  df_radet <- file_radet %>% read_excel(sheet = 1)

  df_radet %>% as_tibble() %>% glimpse()
  df_radet %>% head(100) %>% view

  cols_notes <- c(
    " \\(Kg\\)",
    "\\(c/ml\\)",
    "\\(new, relapsed etc\\)",
    "\\(yyyy-mm-dd\\)",
    " \\(yyyy-mm-dd",
    "yyyy/mm/dd \\(Enter format as specified\\)",
    "\\(Hexadecimal\\/Base64 Unique Identifier\\)")

  cols_notes <- cols_notes %>% paste(collapse = "|")

  df_radet <- df_radet %>%
    rename_with(
      .fn = ~str_replace(., cols_notes, ""),
      .cols = everything()) %>%
    rename_with(~str_trim(., side = "both")) %>%
    rename(lga = `L.G.A`) %>%
    janitor::clean_names()

  df_radet %>% glimpse()

  df_radet %>%
    distinct(ip) %>%
    arrange(ip)

  df_radet %>%
    distinct(ip, state) %>%
    arrange(ip) %>%
    prinf()

  df_radet %>%
    distinct(patient_id)

  df_radet %>%
    count(current_art_status) %>%
    prinf()

  df_radet %>%
    count(current_art_status) %>%
    prinf()


# MUNGING ----

  df_active <- df_radet %>%
    #mutate(age = trunc(difftime(file_date, ymd(date_of_birth), units = "days") / 365.25)) %>%
    mutate(
      date_of_birth = ymd(date_of_birth),
      date_of_birth = case_when(
        !is.Date(date_of_birth) | date_of_birth > file_date ~ NA_Date_,
        TRUE ~ date_of_birth
      ),
     age = trunc((date_of_birth %--% file_date) / years(1)),
     art_start_age = trunc((date_of_birth %--% art_start_date) / years(1)),
     art_length = (art_start_date %--% file_date) / years(1),
     month_of_arv_past = (last_pickup_date %--% file_date) / month(1)) %>%
    select(ip, state, lga, patient_id, date_of_birth, age, sex,
           art_start_date, art_start_age, art_length, art_enrollment_setting,
           last_pickup_date, months_of_arv_refill, month_of_arv_past)

