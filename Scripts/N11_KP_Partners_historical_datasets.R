##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: KP Partners - Historical Datasets
##  LICENCE: MIT
##  DATE:    2022-11-21
##  UPDATED: 2022-11-21

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

  source("./Scripts/N00_Utilities.R")

# GLOBALS ----

## Dirs ----

  dir_merdata <- glamr::si_path("path_msd")
  dir_geodata <- si_path("path_vector")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_images <- "Images"

#dir_llister %>% open_path()
dir_dataout %>% open_path()

## Files ----

  file_psnu_im2 <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*\\d.txt$", recursive = T)

  file_psnu_im1 <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY15.*\\d.txt$", recursive = T)

#open_path(dir_merdata)

## Params ----

  agency <- "USAID"

  cntry <- "Nigeria"

  ou_uid <- get_ouuid(cntry)

  fac_level <- grabr::get_ouorglevel(operatingunit = cntry, org_type = "facility")

  src_msd <- file_psnu_im2 %>% source_info(return = "source")
  curr_pd <- file_psnu_im2 %>% source_info(return = "period")
  curr_fy <- file_psnu_im2 %>% source_info(return = "fiscal_year")

  inds <- c("KP_PREV", "HTS_TST", )

  kp_mechs <- c("KP CARE 1", "KP CARE 2",
                "Integrated MSM Prevention Program")

  exclude_cols <- c("typemilitary", "dreams",
                    "prime_partner_duns", "prime_partner_uei",
                    "award_number")

# LOAD DATA

  df_nga <- file_psnu_im1 %>%
    c(file_psnu_im2) %>%
    map_dfr(function(.file) {
      .file %>%
        read_msd() %>%
        filter(operatingunit == cntry,
               funding_agency == agency)
    })

  ## All Historical Datasets
  df_nga %>%
    select(-ends_with("uid"),
           -all_of(exclude_cols)) %>%
    write_csv(x = .,
              file = "./Dataout/1 - USAID-Nigeria FY15-Present Datasets.csv",
              na = "")

  df_nga %>% glimpse()

  df_nga %>% distinct(fiscal_year)
  df_nga %>% distinct(indicator)
  df_nga %>% distinct(fiscal_year, mech_code, mech_name, prime_partner_name)

  ## Specific Tech Areas
  df_nga %>%
    select(-ends_with("uid"),
           -all_of(exclude_cols)) %>%
    filter(
      str_detect(
        indicator,
        "^PrEP_|KP_PREV|HTS|TX_NEW|TX_CURR|TX_ML|TX_ML_.*|TX_NET_.*|TX_PVLS")) %>%
    write_csv(x = .,
              file = "./Dataout/2 - USAID-Nigeria FY15-Present Key TechAreas Datasets.csv",
              na = "")

  ## KP Partners
  df_nga %>%
    select(-ends_with("uid"),
           -all_of(exclude_cols)) %>%
    filter(
      str_detect(
        indicator,
        "^PrEP_|KP_PREV|HTS|TX_NEW|TX_CURR|TX_ML|TX_ML_.*|TX_NET_.*|TX_PVLS"),
      mech_name %in% kp_mechs) %>%
    write_csv(x = .,
              file = "./Dataout/3 - USAID-Nigeria FY15-Present Key TechAreas and KP Mechs Datasets.csv",
              na = "")

