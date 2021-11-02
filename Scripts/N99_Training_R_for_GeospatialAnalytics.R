##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: Training - R for Geospatial Analytics
##  LICENCE: MIT
##  DATE:    2021-10-28

# PACKAGES ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glitr)
  library(glamr)
  library(gisr)
  library(sf)
  library(raster)
  library(rnaturalearth)
  library(janitor)
  library(gt)
  library(scales)
  library(rpivotTable)

# DIRECTORIES ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  dir_cntry <- "../../GEODATA/QGIS-Training Data/"

  dir.exists(dir_cntry)

  dir_cntry_data <- paste(dir_cntry, "TX-Nigeria/", sep = "")
  dir_cntry_gis <- paste(dir_cntry, "GIS/", sep = "")

  dir_cntry_data %>% list.files()
  dir_cntry_gis %>% list.files()

  dir_graphics %>% list.files()

# Files ----

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    list.files(pattern = "MER_Structured_Datasets_Site_IM_FY19-22_20210917_v2_1_Nigeria",
               full.names = TRUE)

  file_sites_tx <- dir_cntry_data %>%
    list.files(pattern = "^PEPFAR Supported Site",
               full.names = TRUE)

  file_sites_locs <- dir_cntry_data %>%
    list.files(pattern = "^Nigeria_Sites",
               full.names = TRUE)

  file_orgs_attrs <- dir_cntry_data %>%
    list.files(pattern = "^Nigeria_org",
               full.names = TRUE)

  # Shapefile path
  file_shp_cntry <- dir_cntry_gis %>%
    list.files(pattern = "Nigeria_adm0.shp", full.names = TRUE)

  file_shp_states <- dir_cntry_gis %>%
    list.files(pattern = "Nigeria_psnu.shp", full.names = TRUE)

  file_shp_lga <- dir_cntry_gis %>%
    list.files(pattern = "Nigeria_community.shp", full.names = TRUE)

# Params ----

  cntry <- "Nigeria"

  aoi_states <- c("Akwa Ibom", "Cross River")

  agency <- "USAID"

# DATA ----

  # Reading data with
  # readr::read_rds()
  # readr::read_csv()
  # readxl::read_excel()
  # utils::read.csv()
  # utils::read.delim()
  # vroom::vroom()
  # sf::read_sf()

  # SITE x IM ----
  df_msd <- file_site_im %>% read_msd()

  curr_fy <- df_msd %>% identifypd(pd_type = "year")
  curr_pd <- df_msd %>% identifypd(pd_type = "full")

  # LOCATION Data ----

  # Read excel file sheets ----
  excel_sheets(path = file_sites_tx)

  # Site in AI & CR
  df_sites_tx <- read_excel(path = file_sites_tx, sheet = "Data")

  str(df_sites_tx)

  glimpse(df_sites_tx)

  df_sites_tx %>% clean_names() %>% glimpse()

  df_sites_tx %>%
    rename(Facility_uid = `Facility Datim ID`) %>%
    glimpse()

  df_sites_tx <- df_sites_tx %>%
    clean_names()

  # All sites
  df_site_locs <- read_csv(file = file_sites_locs)

  # All org attributes
  df_orgs_attrs <- read_csv(file = file_orgs_attrs)

  unique(df_orgs_attrs$label)

  df_orgs_attrs %>%
    filter(label == "country")

  df_orgs_attrs <- df_orgs_attrs %>%
    filter(label == "prioritization",
           !name %in% c("General Hospital Isolo", "_Military Nigeria", "Obubra General Hospital"))


  # Geospatial Data from local directories ----

  # country
  sf_cntry <- read_sf(file_shp_cntry)

  sf_cntry %>% gview()

  # States
  sf_states <- read_sf(file_shp_states)

  sf_states %>% gview()

  sf_states %>%
    ggplot2::ggplot() +
    geom_sf(fill = scooter, color = grey10k, size = .5) +
    geom_sf(data = sf_cntry, fill = NA, color = grey80k, size = 1) +
    geom_sf(data = sf_cntry, fill = NA, color = grey10k, size = .4) +
    geom_sf_text(data = sf_states, aes(label = name), size = 2.5, color = "white") +
    theme_void()

  # LGA / Community
  sf_lga <- read_sf(file_shp_lga)

  # Geospatial Data from Natural Earth ----

  # All countries
  ne_cntries <- ne_countries(returnclass = "sf")

  ne_cntries %>% gview()

  ne_cntries %>% dview()

  ne_cntries %>% glimpse()

  ne_cntries %>% filter(adm0_a3 == "NGA") %>% gview()
  ne_cntries %>% filter(adm0_a3 == "NGA") %>% dview()

  # Country boundaries
  ne_country <- ne_countries(country = "Nigeria", returnclass = "sf")

  ne_country %>% gview()

  # States boundaries
  ne_states <- ne_states(country = "Nigeria", returnclass = "sf")

  ne_states %>% gview()

  # Geospatial Data from GADM ----

  # Country
  gadm_country <- raster::getData(name = "GADM",
                                  download = TRUE,
                                  country = "NGA",
                                  level = 0,
                                  path = "./GIS")

  gadm_country %>%
    st_as_sf() %>%
    gview()

  gview(st_as_sf(gadm_country))

  # States
  gadm_states <- raster::getData(name = "GADM",
                                 download = TRUE,
                                 country = "NGA",
                                 level = 1,
                                 path = "./GIS")

  # LGA/Community
  gadm_lga <- raster::getData(name = "GADM",
                              download = TRUE,
                              country = "NGA",
                              level = 2,
                              path = "./GIS")



# MUNGING ----

  # Selection ----
  ## select()
  ne_cntries %>% glimpse()

  ne_cntries %>%
    dplyr::select(iso3 = sov_a3, name, admin, type) %>%
    st_drop_geometry() %>%
    view()


  ## filter()

  ne_cntries %>%
    dplyr::select(iso3 = sov_a3, name, admin, type, continent) %>%
    #filter(iso3 == "NGA") %>%
    filter(continent == "Africa") %>%
    st_drop_geometry() %>%
    view()

  # Transformation ----
  ## Mutate
  ne_cntries %>%
    dplyr::select(iso3 = sov_a3, name, admin, type, continent) %>%
    filter(continent == "Africa") %>%
    separate(type, into = c("type", "type2"), sep = " ") %>%
    st_drop_geometry() %>%
    view()

  ## Separate

  ## Combine
  ne_cntries %>%
    dplyr::select(iso3 = sov_a3, name, admin, type, continent) %>%
    filter(continent == "Africa") %>%
    mutate(fullname = paste(iso3, name, sep = " - "),
           fullname2 = paste0(name, " (50%)")) %>%
    st_drop_geometry() %>%
    view()
  ## Pivots Tables

  # group_by and summarize ----

# VIZ ----

  sf_states %>%
    ggplot2::ggplot() +
    geom_sf(fill = scooter, color = grey10k, size = .5) +
    geom_sf(data = sf_cntry, fill = NA, color = grey80k, size = 1) +
    geom_sf(data = sf_cntry, fill = NA, color = grey10k, size = .4) +
    geom_sf_text(data = sf_states, aes(label = name), size = 2.5, color = "white") +
    theme_void()