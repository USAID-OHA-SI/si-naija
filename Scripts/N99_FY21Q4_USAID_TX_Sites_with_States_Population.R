##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: ART Saturation Classification
##  LICENCE: MIT
##  DATE:    2021-12-02


# DEPENDENCIES ----

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glitr)
  library(glamr)
  library(gisr)
  library(sf)
  library(gophr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(ggrepel)
  library(ggnewscale)
  library(patchwork)
  library(glue)

# SETUP ----

  # DIRs ----
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  # Files ----

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_FY19.*_N.*.zip$")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM__FY19.*_N.*.zip$")

  # NAT Data - get the latest NAT_SUBNAT file
  file_natsub <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_NAT_SUBNAT_.*.zip$")

  # Shapefile path
  file_shp <- dir_geodata %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

# GLOBALS ----

  # Country name
  cntry <- "Nigeria"
  agency = "USAID"

# FUNCTIONS ----


# LOAD DATA ----

  # MSD Data
  df_sites_all <- file_site_im %>% read_msd()

  df_psnu_all <- file_psnu_im %>% read_msd()

  # Rep. Periods
  curr_pd <- df_psnu_all %>% identifypd(pd_type = "full")
  curr_fy <- df_psnu_all %>% identifypd(pd_type = "year")

  # Sub-Nat
  df_nat <- file_natsub %>%
    read_msd() %>%
    filter(countryname == cntry)

  # SPATIAL DATA
  terr <- gisr::get_raster(path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- get_attributes(country = cntry)

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(name))



# MUNGE ----

  # TX_CURR ----
  df_psnu_all %>% glimpse()
  df_psnu_all %>% distinct(fundingagency)

  # USAID States
  df_psnu_usaid <- df_psnu_all %>%
    filter(fiscal_year == curr_fy,
           fundingagency == agency,
           psnu != "_Military Nigeria") %>%
    distinct(fundingagency, psnuuid, psnu)

  # HTS & TX Sites count
  df_sites_usaid <- df_sites_all %>%
    filter(fiscal_year == curr_fy,
           fundingagency == agency,
           indicator %in% c("TX_CURR", "HTS_TST"),
           standardizeddisaggregate == "Total Numerator") %>%
    #group_by(fiscal_year, psnuuid, psnu, indicator) %>%
    group_by(fiscal_year, indicator) %>%
    summarise(targets = sum(targets, na.rm = T),
              cummulative = sum(cumulative, na.rm = T),
              sites = n_distinct(orgunituid, sitename))

  # % of POPs
  df_pops <- df_nat %>%
    filter(fiscal_year %in% c(curr_fy, curr_fy +1),
           indicator %in% c("PLHIV", "POP_EST"),
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, psnuuid, psnu, indicator) %>%
    summarise(value = sum(targets, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = value)

  df_pops <- df_pops %>%
    left_join(df_psnu_usaid, by = c("psnuuid", "psnu")) %>%
    mutate(fundingagency = ifelse(psnu == "Taraba", "USAID", fundingagency),
           fundingagency = ifelse(is.na(fundingagency), "Others", fundingagency)) %>%
    group_by(fiscal_year, fundingagency) %>%
    summarise(across(c(PLHIV, POP_EST), sum, na.rm = TRUE), .groups = "drop") %>%
    group_by(fiscal_year) %>%
    mutate(PLHIV_SHARE = PLHIV / sum(PLHIV) * 100,
           POP_EST_SHARE = POP_EST / sum(POP_EST)* 100) %>%
    ungroup()



