##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: TX_CURR - Akwa Ibom Sites
##  LICENCE: MIT
##  DATE:    2021-10-26


# PACKAGES ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glitr)
  library(glamr)
  library(gisr)
  library(sf)
  library(janitor)
  library(gt)

# Paths ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")
  dir_cntry <- "../../PEPFAR/COUNTRIES/Nigeria/Locations Data"

# Files ----

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v2_1_N.*.zip$")

  file_sites <- dir_cntry %>%
    return_latest(pattern = "^PEPFAR Supported Site")

  # Shapefile path
  file_shp <- dir_geodata %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

# Params ----

  cntry <- "Nigeria"

  aoi_states <- c("Akwa Ibom", "Cross River")

  agency <- "USAID"

  cntry_uid <- get_ouuid(cntry)
  site_lvl <- get_ouorglevel(cntry, org_type = "facility")

# DATA ----

  # SITE x IM
  df_sites <- file_site_im %>% read_msd()

  curr_fy <- df_sites %>% identifypd(pd_type = "year")
  curr_pd <- df_sites %>% identifypd(pd_type = "full")

  # LOCATION
  df_locs <- extract_locations(country = cntry) %>%
    extract_facilities() %>%
    select(uid = id, name, longitude, latitude)

  # df_locs %>%
  #   write_csv(file = paste0(dir_dataout, "/Nigeria_Sites_locations.csv"),
  #             na = "")

  df_attrs <- get_attributes(country = cntry)

  # df_attrs %>%
  #   write_csv(file = paste0(dir_dataout, "/Nigeria_org_attributes.csv"),
  #             na = "")

  df_sites_types <- file_sites %>% read_excel(sheet = "Data") %>%
    clean_names()

  # GEO ----
  terr <- get_raster(path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(name))

# MUNGING ----

  # Sites Locations
  spdf_pepfar %>%
    st_drop_geometry() %>%
    distinct(label)

  spdf_adm0 <- spdf_pepfar %>%
    filter(label == 'country')

  export_spdf(spdf_adm0, "./GIS/Nigeria_adm0.shp")

  spdf_adm0 %>% gview()

  spdf_adm1 <- spdf_pepfar %>%
    filter(label == 'prioritization')

  spdf_adm1 %>% gview()

  export_spdf(spdf_adm1, "./GIS/Nigeria_psnu.shp")

  spdf_akwaibom <- spdf_pepfar %>%
    filter(name == "Akwa Ibom", label == 'prioritization')

  spdf_xriver <- spdf_pepfar %>%
    filter(name == "Cross River", label == 'prioritization')

  spdf_adm2 <- spdf_pepfar %>%
    filter(label == 'community') %>%
    clean_column(colname = "name")

  spdf_adm2 %>% gview()

  export_spdf(spdf_adm2, "./GIS/Nigeria_community.shp")

  # TX_CURR Data
  df_tx_curr <- df_sites %>%
    filter(#fiscal_year == curr_fy,
           fundingagency == agency,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           sitename != "Data reported above Site level") %>%
    group_by(fiscal_year, psnuuid, psnu, communityuid, community, orgunituid, sitename) %>%
    summarise(across(c("cumulative", starts_with("qtr")),
                     sum, na.rm = T), .groups = "drop") %>%
    ungroup() %>%
    clean_column(colname = "community")

  # df_tx_curr %>%
  #   reshape_msd() %>%
  #   select(-period_type) %>%
  #   pivot_wider(names_from = period, values_from = value) %>%
  #   write_csv(file = paste0(dir_dataout, "/Nigeria_TX_CURR_Sites_IM.csv"),
  #             na = "")


  # AOI: Akwa Ibom & Cross River
  df_aoi <- df_tx_curr %>%
    filter(psnu %in% aoi_states) %>%
    relocate(cumulative, .after = last_col()) %>%
    left_join(df_locs, by = c("orgunituid" = "uid", "sitename" = "name"))

  df_aoi %>%
    select(psnu, sitename, qtr1:cumulative) %>%
    write_csv(file = paste0(dir_dataout, "/Nigeria_FY21_TX_CURR_Sites_In_AkwaIbom_and_CrossRiver.csv"),
              na = "")

  df_tx_curr %>%
    filter(community == "Maiduguri") %>%
    left_join(df_locs, by = c("orgunituid" = "uid")) %>%
    select(state = psnu, community, name, latitude, longitude) %>%
    write_csv(file = paste0(dir_dataout, "/Nigeria_FY21_TX_CURR_Sites_locations_in_Maiduguri.csv"),
              na = "")

  df_gaoi <- df_aoi %>%
    filter(!is.na(longitude) & !is.na(latitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))

# Dataout

  df_sites_types %>%
    write_csv(file = paste0(dir_dataout, "/Nigeria_FY21_TX_CURR_Sites_In_AkwaIbom_CrossRiver_and_Borno.csv"),
              na = "")

  df_sites_types %>%
    filter(state %in% aoi_states) %>%
    clean_column(colname = "lga") %>%
    select(state, lga, facility, facility_types, tx_curr) %>%
    arrange(state, facility_types) %>%
    write_csv(file = paste0(dir_dataout, "/Nigeria_FY21_TX_CURR_Sites_In_AkwaIbom_and_CrossRiver.csv"),
              na = "")

  df_sites_types %>%
    clean_column(colname = "lga") %>%
    filter(state == "Borno", lga == "Maiduguri") %>%
    select(state, lga, facility, facility_types, latitude, longitude) %>%
    write_csv(file = paste0(dir_dataout, "/Nigeria_FY21_TX_CURR_Sites_locations_in_Maiduguri.csv"),
              na = "")

# VIZ ----

  df_gaoi %>%
    ggplot() +
    geom_sf(data = spdf_adm0, size = 20, color = "#8ce4fe", fill = NA) +
    geom_sf(data = spdf_adm0, size = 15, color = "#74ccec", fill = NA) +
    geom_sf(data = spdf_adm0, size = 10, color = "#5bb5d5", fill = NA) +
    geom_sf(data = spdf_adm0, size = 5, color = "#419fbe", fill = NA) +
    geom_sf(data = spdf_adm0, size = 1, color = grey50k, fill = "white") +
    geom_sf(data = spdf_adm1, lty = "dotted", fill = NA) +
    geom_sf(data = spdf_adm0, size = 1, color = grey50k, fill = NA) +
    geom_sf(data = spdf_adm1 %>% filter(name %in% aoi_states),
            size = 1, color = "white") +
    geom_sf() +
    geom_sf_label(data = spdf_adm1, aes(label = name)) +
    coord_sf(xlim = c(7.5, 9.5), ylim = c(4.5, 6.9)) +
    si_style_map()



