##  PROJECT: Performance Summary Tables
##  AUTHOR:  jdavis | USAID
##  PURPOSE: Pull Summary clinical cascade data
##  LICENCE: MIT
##  DATE:    2021-01-19


# PACKAGES -------------------------------------------------

library(tidyverse)
library(readxl)
library(glitr)
library(scales)
library(glamr)
library(sf)
library(raster)
library(gisr)
library(janitor)
library(extrafont)

# GLOBAL --------------------------------------------------

  # Load configs
  source("./Scripts/N00_Config.R")

  # Country name
  cntry <- "Nigeria"

  # file
  file_targets <- list.files(
    path = data,
    pattern = "^Site Tool_Nig.*_\\d{14}_F.*.xlsx$",
    full.names = TRUE
  )

  # Latest MSD PSNU x IM File
  file_msd <- list.files(
    path = merdata,
    pattern = "MER_S.*_PSNU_IM_.*_\\d{8}_v.*_N.*.zip",
    full.names = TRUE
  ) %>%
    sort() %>%
    last()

  # Latest MSD Site x IM File
  file_msd_sites <- list.files(
    path = merdata,
    pattern = "MER_S.*_Site_IM_.*_\\d{8}_v.*_N.*.zip",
    full.names = TRUE
  ) %>%
    sort() %>%
    last()

  # Geodata
  file_shp <- list.files(
      path = shpdata,
      pattern = "VcPepfarPolygons.shp$",
      recursive = TRUE,
      full.names = TRUE
    ) %>%
    sort() %>%
    last()


# DATA ----

  # Geodata
  spdf_pepfar <- file_shp %>% read_sf()

  spdf_pepfar %>% glimpse()

  # OUs
  df_ous <- glamr::identify_ouuids(datim_user(), datim_pwd())

  df_ous %>% glimpse()

  ou_uid <- df_ous %>%
    dplyr::filter(type == "OU", country == cntry) %>%
    pull(uid)

  ou_uid

  # Levels
  df_lvls <- glamr::identify_levels(datim_user(), datim_pwd())

  df_lvls %>% glimpse()

  df_lvls %>%
    filter(operatingunit == cntry) %>%
    glimpse()

  # Orgs
  locs <- gisr::extract_locations(cntry, datim_user(), datim_pwd())

  locs %>% extract_facilities()
  #locs %>% extract_

  ids <- locs %>% filter(level == 6) %>% pull(id)

  nga_comm <- spdf_pepfar %>%
    filter(uid %in% ids)

  # NGA Boundaries
  spdf_pepfar %>%
    filter(uid == ou_uid) %>%
    plot(col = NA, main = cntry)

  gisr::get_admin0(cntry) %>%
    dplyr::select(admin) %>%
    plot()

  # Basemap
  #terrain_map(cntry, mask = TRUE)
  terr <- terrain_map(cntry, terr_path = rasdata)
  terrain_map(cntry, terr_path = rasdata, mask = TRUE)
  terrain_map(cntry, terr_path = rasdata, add_neighbors = TRUE) # update extend boundaries


  # MSD Data

  # Sites
  df_msd_sites <- file_msd_sites %>%
    read_msd() %>%
    reshape_msd(clean = TRUE)

  df_msd_sites %>% glimpse()


  # MSD Community TX
  df_comm <- df_msd_sites %>%
    clean_agency() %>%
    filter(operatingunit == country,
           indicator %in% c("TX_CURR", "TX_NEW"),
           period_type %in% c("targets", "cumulative"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup",
           communityuid != "?") %>%
    group_by(fundingagency, snu1, psnuuid, psnu,
             communityuid, community, indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup()

  df_comm <- df_sites %>%
    clean_column("community") %>%
    glimpse()

  df_comm %>% glimpse()

  df_geom <- spdf_pepfar %>%
    left_join(df_comm %>%
      filter(fundingagency == "USAID",
           indicator == "TX_CURR",
           period == "FY20"),
      by = c("uid" = "communityuid")
    ) %>%
    filter(!is.na(indicator))

  terr +
    geom_sf(data = df_geom, aes(fill = val)) +
    geom_sf(data = nga_comm, fill = NA, color = grey10k) +
    facet_wrap(~period_type) +
    scale_fill_si(palette = "genoas", reverse = T, discrete = F) +
    si_style_map()

