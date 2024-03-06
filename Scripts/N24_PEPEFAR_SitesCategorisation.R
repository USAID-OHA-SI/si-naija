# PURPOSE: SI-Naija
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Health Facilities Categorization
# REF ID:  7ac6ff3b
# LICENSE: MIT
# DATE:    2024-02-12
# UPDATE:  2024-02-12
# NOTES:   Use of Site attributes

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(glamr)
  library(gophr)
  library(grabr)
  library(glitr)
  library(gisr)
  library(sf)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(janitor)
  library(glue)
  library(gtExtras)
  library(gt)

  source("./Scripts/N00_Utilities.R")

# LOCALS & SETUP ====

  # Set Params

  ref_id <- "7ac6ff3b"
  agency <- "USAID"
  cntry <- "Nigeria"

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

  file_nat <- dir_mer %>% return_latest("NAT_SUBNAT")
  file_psnu <- dir_mer %>% return_latest("PSNU_IM_FY22-.*_Nigeria")
  file_site <- dir_mer %>% return_latest("Site_IM_FY22-.*_Nigeria")

  file_site2 <- file.path(dir_cntry, "MER") %>%
    return_latest("FY24 Q1 TX_CURR.*.csv")

  file_facilities <- file.path(dir_cntry, "Locations Data") %>%
    return_latest("USAID Comprehensive.*Facility List.*.xlsx")

  file_attrs <- file.path(dir_cntry, "Locations Data") %>%
    return_latest("Org.* Unit Attributes")

  file_usaid_locs <- file.path(dir_cntry, "Locations Data") %>%
    return_latest("GEOC.*SUPPORTED SITES.*")

  file_partners <- file.path(dir_cntry, "Locations Data") %>%
    return_latest(".*IM and Partners Cov.*")

  # Meta

  get_metadata(file_psnu)

  meta <- metadata

  cntry_uid <- get_ouuid(cntry)


# Functions  =====

  #' @title Plot lat/lon
  #'
  gview_sites <- function(.locs,
                          lat = "latitude",
                          lon = "longitude") {

    .locs %>%
      ggplot(aes(x = !!sym(lon), y = !!sym(lat))) +
      geom_point(size = 5, shape = 21,
                 color = grey10k,
                 fill = usaid_lightblue,
                 alpha = .8) +
      coord_map() +
      si_style() +
      labs(x = "", y = "")
  }

  #' @title Export Map plot
  #'
  export_map <- function(mplot, name, ...) {
    si_save(plot = mplot,
            filename = file.path(dir_graphics, glue::glue("{name}.png")),
            scale = 1.4,
            dpi = 350,
            width = 8,
            height = 6,
            ...)
  }


  #' @title Convert sf Bounding Box to data frame
  #'
  #' @param spdf   Spatial Data Frame, preferably and sf object
  #' @param expand Expansion rate as a numerical value and in the spdf unit. Number of rows/columns to extend by.
  #'
  #' @return A data frame containing the bounds of latitude and longitude
  #'
  bbox_as_df <- function(spdf, expand = 0) {
    # Extract bounding box
    spext <- sf::st_bbox(spdf)

    # Extend bbox if asked
    if (expand > 0) {
      spext <- terra::extend(terra::ext(spext), {{expand}})
    }

    # Convert bbox to data frame
    spext %>%
      base::as.list() %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = "bound",
                          values_to = "value") %>%
      dplyr::mutate(axis = str_sub(bound, 1, 1),
                    bound = str_remove(bound, "x|y")) %>%
      tidyr::pivot_wider(names_from = axis, values_from = value) %>%
      dplyr::rename(longitude = x, latitude = y)
  }

# LOAD DATA =====

  # Activities Locations ----

  df_msd <- file_psnu %>%
    read_psd() %>%
    filter(fiscal_year == meta$curr_fy,
           funding_agency == agency,
           str_detect(psnu, "_Mil", negate = TRUE),
           str_detect(indicator, "TX"))

  df_tx_cov <- df_msd %>%
    distinct(psnu, mech_name, prime_partner_name) %>%
    clean_mechs() %>%
    clean_partners()

  df_tx_cov %>%
    select(State = psnu, IM = mech_name) %>%
    distinct_all() %>%
    mutate(TechArea = "HIV") %>%
    write_csv(na = "", file = file.path(dir_cntry, "Locations Data/HIV_Partners.csv"))

  #file_partners %>% open_path()
  #file_partners %>% excel_sheets()

  df_ims <- c("COVID19", "OVC", "HIV", "TB") %>%
    map(~read_excel(path = file_partners,
                    sheet = .x,
                    col_types = "text")) %>%
    bind_rows()

  df_ims %>% glimpse()

  df_ims <- df_ims %>%
    clean_names() %>%
    select(-starts_with("x"))

  df_tech_areas <- df_ims %>%
    distinct(state, tech_area) %>%
    mutate(tech_area = factor(
      tech_area, levels = c("HIV", "TB", "OVC", "COVID-19"))) %>%
    arrange(state, tech_area) %>%
    summarise(tech_areas = paste(tech_area, collapse = ", "),
              .by = state) %>%
    arrange(tech_areas)

  df_tech_areas2 <- df_ims %>%
    distinct(state, tech_area) %>%
    mutate(tech_area = ifelse(
      tech_area == "HIV",
      "Clinical HIV Services",
      tech_area)) %>%
    mutate(tech_area = factor(
      tech_area, levels = c("Clinical HIV Services", "TB", "OVC", "COVID-19")
    ))

  df_ims_cov <- df_ims %>%
    summarise(im_cov = paste(sort(im), collapse = ", "),
              .by = c(state, tech_area)) %>%
    mutate(tech_area = factor(
      tech_area, levels = c("HIV", "TB", "OVC", "COVID-19"))) %>%
    arrange(state, tech_area)

  df_ims_cov <- df_ims_cov %>%
    pivot_wider(names_from = tech_area, values_from = im_cov) %>%
    rename_with(str_to_upper)

  df_ims_cov %>%
    write_csv(na = "", file = file.path(dir_cntry, "Locations Data/HAT_TechAreas and mechs.csv"))

  # Geo data ----

  ## VC POlygons
  spdf_pepfar <- get_vcpolygons(folderpath = si_path("path_vector"))

  ## Country boundaries
  spdf_cnty <- spdf_pepfar %>%
    filter(uid == cntry_uid) %>%
    st_make_valid()

  # spdf_cnty %>% bbox_as_df()
  # spdf_cnty %>% bbox_as_df(.5)
  # spdf_cnty %>% bbox_as_df(1)

  ext_cntry <- spdf_cnty %>% bbox_as_df()

  ## Hexbins
  spdf_hex <- spdf_cnty %>%
    st_make_valid() %>%
    get_hexbins(size = 30000, clip = F) %>%
    filter(st_geometry_type(x) != "POINT") %>%
    select(id, area) %>%
    mutate(puid = cntry_uid)

  spdf_hex_big <- spdf_cnty %>%
    st_make_valid() %>%
    get_hexbins(size = 100000)


  ## PSNU Attributes -----

  df_org_attrs <- cntry_uid %>%
    get_ouorgs(ouuid = .,
               level = get_ouorglevel(cntry, org_type = "prioritization"),
               username = datim_user(),
               password = datim_pwd())

  ## States boundaries
  spdf_states <- spdf_pepfar %>%
    left_join(df_org_attrs, by = "uid") %>%
    filter(!is.na(orgunit))

  spdf_tech_areas <- spdf_states %>%
    left_join(df_tech_areas, by = c("orgunit" = "state")) %>%
    filter(!is.na(tech_areas))

  spdf_tech_areas %>%
    st_drop_geometry() %>%
    distinct(tech_areas)

  spdf_tech_areas <- spdf_tech_areas %>%
    mutate(
      tech_color = case_when(
        tech_areas == "HIV" ~ old_rose,
        tech_areas == "HIV, TB, OVC, COVID-19" ~ burnt_sienna,
        tech_areas == "HIV, OVC, COVID-19" ~ scooter,
        tech_areas == "TB, COVID-19" ~ denim_light,
        tech_areas == "COVID-19" ~ denim,
      ))

  spdf_tech_areas2 <- spdf_states %>%
    left_join(df_tech_areas2, by = c("orgunit" = "state")) %>%
    filter(!is.na(tech_area))

  spdf_tech_areas2 %>%
    st_drop_geometry() %>%
    distinct(tech_area)

  spdf_tech_areas2 <- spdf_tech_areas2 %>%
    mutate(
      tech_color = case_when(
        tech_area == "Clinical HIV Services" ~ old_rose,
        tech_area == "TB" ~ burnt_sienna,
        tech_area == "OVC" ~ scooter,
        tech_area == "COVID-19" ~ genoa
      ))

  ## Terrain data ----

  dt_terr <- get_raster(folderpath = si_path("path_raster"))

  # Health Facilities - USAID Sites

  ## Sites - Comprehensive List ----

  #file_facilities %>% open_path()
  file_facilities %>% excel_sheets()

  facs_sheet <- "Comprehensive List"

  df_facs <- file_facilities %>%
    read_excel(sheet = facs_sheet, skip = 4, col_types = "text") %>%
    clean_names()

  df_facs %>% glimpse()

  # df_facs %>% filter(if_any(ends_with("tude"), ~str_detect(.x, "\\w+")))
  # df_facs %>% filter(if_any(ends_with("tude"), ~str_detect(.x, "[:alpha:]")))
  # df_facs %>% filter(if_any(ends_with("tude"), ~str_detect(.x, "[:punct:]")))
  # df_facs %>% filter(if_any(ends_with("tude"), ~is.na(.x)))

  df_facs_err <- df_facs %>%
    filter(if_any(ends_with("tude"), \(.x) {is.na(.x) | str_detect(.x, "[:alpha:]")})) %>%
    select(1:designation)

  df_facs_err %>% glimpse()

  df_facs_err %>%
    mutate(
      status = case_when(
        is.na(latitude) | is.na(longitude) ~ "Missing",
        !is.na(latitude) & !is.na(longitude) &
          (str_detect(latitude, "[:alpha:]") |
          str_detect(longitude, "[:alpha:]")) ~ "Not a number",
        is.numeric(latitude) & is.numeric(longitude) ~ "Not a numeric",
        TRUE ~ "Valid"
      )
    ) %>%
    group_by(im, state, status) %>%
    summarise(n = n_distinct(s_n)) %>%
    ungroup()

  df_facs_err %>%
    filter(if_any(ends_with("tude"), ~!is.na(.x))) %>%
    mutate(across(all_of(c("latitude", "longitude")), ~str_remove_all(.x, "[:alpha:]|\'"))) %>%
    mutate(across(all_of(c("latitude", "longitude")), ~as.numeric(.x))) %>%
    filter(
      if_any(latitude, ~between(.x, ext_cntry$latitude[1], ext_cntry$latitude[2])),
      if_any(longitude, ~between(.x, ext_cntry$longitude[1], ext_cntry$longitude[2]))
      #,latitude < 12
    ) %>%
    gview_sites()

  df_facs_err %>%
    write_csv(na = "", file = file.path(dir_cntry, "Locations Data/USAID Comprehensive - missing coordinates.csv"))

  df_facs %>% gview_sites()

  df_facs_valid <- df_facs %>%
    filter(if_any(ends_with("tude"), ~!is.na(.x))) %>%
    mutate(across(ends_with("tude"), ~str_remove_all(.x, "[:alpha:]|\'"))) %>% #view
    mutate(across(ends_with("tude"), ~as.numeric(.x)))

  df_facs_valid %>%
    filter(!between(latitude, ext_cntry$latitude[1], ext_cntry$latitude[2]) |
           !between(longitude, ext_cntry$longitude[1], ext_cntry$longitude[2])) %>%
    gview_sites()

  df_facs_valid %>%
    filter(between(latitude, ext_cntry$latitude[1], ext_cntry$latitude[2]),
           between(longitude, ext_cntry$longitude[1], ext_cntry$longitude[2])) %>%
    gview_sites()

  # Health Facilities - USAID Sites (Updated) ----

  file_usaid_locs %>% excel_sheets()

  #file_usaid_locs %>% open_path()

  df_facs2 <- file_usaid_locs %>%
    read_excel(sheet = 1) %>%
    clean_names() %>%
    mutate(across(ends_with("tude"), as.numeric))

  df_facs2 %>% glimpse()

  df_facs2 <- df_facs2 %>%
    rename(facility_name = facility,
           lat = latitude, lon = longitude) %>%
    left_join(df_locs, by = c("facility_datim_id" = "orgunituid")) %>%
    filter(is.na(latitude))

  df_facs2 %>% distinct(state)


  ## Site Attrs (NO SERVICE LEVEL AVAILABLE HERE) ----

  df_attrs <- file_attrs %>%
    read_csv() %>%
    clean_names() %>%
    select(-moh_site_id, hierarchy)

  df_attrs %>% glimpse()

  ## Site location (+ Facility Type from Site Attrs) ----

  df_locs <- datim_pull_hierarchy(cntry_uid, add_geom = T)

  df_locs <- df_locs %>%
    filter(level == max(level, na.rm = T)) %>%
    left_join(df_attrs[, c("datim_uid", "facility_type")],
              by = c("orgunituid" = "datim_uid"))

  ## Site x IM ----

  #df_msd <- file_site %>% read_psd()
  df_msd <- file_site2 %>% vroom::vroom()

  df_msd %>% glimpse()

  df_msd <- df_msd %>%
    clean_names() %>%
    rename(
      operatingunit = orgunitlevel1,
      psnu = orgunitlevel2,
      communit = orgunitlevel3,
      facility = orgunitlevel4,
      orgunituid = organisationunitid,
      orgunit = organisationunitname,
      funding_agency = name_10,
      sex = name_13,
      age = name_15,
      value = total
    ) %>%
    select(-matches("\\d$")) %>%
    mutate(indicator = "TX_CURR",
           standdardizeddisaggregate = "Total Numerator",
           .after = funding_agency) %>%
    clean_agency()

# MUNGE =====

  ## Sites ----

  df_facs_clean <- df_facs %>%
    select(id = s_n, datim_uid, facility_name = facility_name_implementing_agent,
           facility_type, service_level, ownership, pop_setting = population_setting,
           im, partner_info, state, lga, ward, latitude, longitude,
           art:art_data_reported_to_datim) %>%
    mutate(across(ends_with("tude"), ~str_remove_all(.x, "[:alpha:]|\'"))) %>%
    mutate(across(ends_with("tude"), ~as.numeric(.x)))

  df_facs_svc <- df_facs_clean %>%
    pivot_longer(cols = art:art_data_reported_to_datim)

  df_facs_cat <- df_facs_clean %>%
    distinct(datim_uid, facility_name, state, service_level)

  ## TX_CURR - Site x Agency ----

  df_msd %>% distinct(funding_agency)

  df_msd %>%
    distinct(funding_agency, psnu, orgunituid) %>%
    count(funding_agency, psnu)

  df_msd %>%
    distinct(funding_agency, orgunituid) %>%
    count(funding_agency)

  df_tx_agency <- df_msd %>%
    summarise(across(value, ~sum(.x, na.rm = T)),
              .by = c(funding_agency, orgunituid, orgunit, psnu))

  ## ADD Site Location data

  ## Datim Sites locations
  df_locs %>% glimpse()

  df_locs %>%
    write_csv(na = "",
              file = file.path(dir_cntry, "Locations Data/Nigeria - Organisation hierarchy.csv"))

  ## Flag USG Agencies Sites ----

  df_tx_agency_svc <- df_locs %>%
    select(orgunituid, latitude, longitude, facility_type) %>%
    left_join(df_tx_agency, by = c("orgunituid")) %>%
    mutate(
      funding_agency = case_when(
        is.na(funding_agency) ~ "NON-PEPFAR",
        TRUE ~ funding_agency
      ),
      funding_agency = factor(
        funding_agency,
        levels = c("USAID", "CDC", "NON-PEPFAR"),
        ordered = T
      ),
      facility_type = case_when(
        is.na(facility_type) ~ "Other",
        TRUE ~ facility_type
      ))

  df_tx_miss_locs <- df_tx_agency_svc %>%
    filter(funding_agency == "USAID",
           if_any(ends_with("tude"), ~is.na(.x))) %>%
    count(funding_agency, psnu) %>%
    arrange(desc(n), psnu)

  # Update USAID Sites

  df_tx_agency_svc %>% glimpse()

  df_tx_agency_svc %>% distinct(facility_type)

  df_facs_clean %>% glimpse()

  ## Add Service Levels & update lat/long
  df_tx_agency_svc <- df_facs_clean %>%
    select(datim_uid, lat = latitude, lon = longitude, service_level) %>%
    left_join(
      df_tx_agency_svc, .,
      by = c("orgunituid" = "datim_uid")
    ) %>%
    mutate(
      latitude = case_when(
        !is.na(lat) ~ lat,
        TRUE ~ latitude
      ),
      longitude = case_when(
        !is.na(lon) ~ lon,
        TRUE ~ longitude
      )
    )

  df_tx_agency_svc <- df_facs2 %>%
    select(facility_datim_id, lat2 = lat, lon2 = lon) %>%
    left_join(df_tx_agency_svc, ., by = c("orgunituid" = "facility_datim_id")) %>%
    mutate(
      latitude = case_when(
        is.na(latitude) & !is.na(lat2) ~ lat2,
        TRUE ~ latitude
      ),
      longitude = case_when(
        is.na(longitude) & !is.na(lon2) ~ lon2,
        TRUE ~ longitude
      )
    ) %>%
    relocate(service_level, .after = facility_type)

  df_tx_agency_svc_cov <- df_tx_agency_svc %>%
    filter(funding_agency %in% c("USAID", "CDC")) %>%
    summarise(
      sites = n_distinct(orgunituid),
      tx_curr = sum(value, na.rm = T),
      .by = c(funding_agency, facility_type)
    )

  df_tx_agency_svc_cov <- df_tx_agency_svc_cov %>%
    summarise(across(all_of(c("sites", "tx_curr")), ~ sum(.x, na.rm = T)),
              .by = facility_type) %>%
    mutate(funding_agency = "PEPFAR") %>%
    bind_rows(df_tx_agency_svc_cov, .) %>%
    group_by(funding_agency) %>%
    mutate(p_sites = sites / sum(sites),
           p_tx_curr = tx_curr / sum(tx_curr),
           funding_agency = factor(funding_agency,
                                   levels = c("USAID", "CDC", "PEPFAR"),
                                   ordered = T)) %>%
    ungroup() %>%
    arrange(funding_agency, facility_type) %>%
    relocate(sites, p_sites, tx_curr, p_tx_curr,
             .after = facility_type)

  ## missing coordinates

  df_tx_agency_svc %>%
    filter(funding_agency == "USAID",
           !(between(latitude, ext_cntry$latitude[1], ext_cntry$latitude[2]) |
           between(longitude, ext_cntry$longitude[1], ext_cntry$longitude[2])) |
             is.na(latitude)) %>%
    arrange(psnu, facility_type) %>%
    select(-c(lat, lat2, lon, lon2))

  ## Summary stats for viz ----

  facs_pepfar <- df_tx_agency_svc %>%
    mutate(
      funding_agency = case_when(
        funding_agency %in% c("USAID", "CDC") ~ "PEPFAR",
        TRUE ~ funding_agency
      )
    ) %>%
    count(funding_agency) %>%
    mutate(p = n / sum(n))

  facs_pepfar_factype <- df_tx_agency_svc %>%
    filter(funding_agency %in% c("USAID", "CDC")) %>%
    count(funding_agency, facility_type) %>%
    mutate(p = n / sum(n))

  facs_pepfar_prim <- df_tx_agency_svc %>%
    filter(funding_agency %in% c("USAID", "CDC")) %>%
    count(facility_type) %>%
    mutate(p = n / sum(n))

  facs_agency <- df_tx_agency_svc %>%
    filter(funding_agency != "NON-PEPFAR") %>%
    count(funding_agency) %>%
    mutate(p = n / sum(n))

  ## Site geo-data ----

  spdf_sites <- df_tx_agency_svc %>%
    select(-c(lat, lat2, lon, lon2)) %>%
    filter(!is.na(latitude),
           between(latitude, ext_cntry$latitude[1], ext_cntry$latitude[2]),
           between(longitude, ext_cntry$longitude[1], ext_cntry$longitude[2])) %>%
    spdf_points()

  ## Summaries Sites TX_CURR by HEX Bins

  spdf_tx <- spdf_hex %>%
    st_join(spdf_sites[spdf_sites$value > 0, c("orgunituid", "value")], left = T) %>%
    group_by(id) %>%
    summarise(sites = n_distinct(orgunituid, na.rm = T),
              value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    st_drop_geometry() %>%
    left_join(spdf_hex, ., by = "id")


# VIZ =====

  ## Basemap

  bmap <- terrain_map(countries = cntry,
                      adm0 = spdf_cnty,
                      adm1 = spdf_states,
                      terr = dt_terr,
                      mask = T)

  ## IM Coverage ----

  # "HIV" ~ old_rose,
  # "HIV, TB, OVC, COVID-19" ~ burnt_sienna,
  # "HIV, OVC, COVID-19" ~ scooter,
  # "TB, COVID-19" ~ denim_light,
  # "COVID-19" ~ denim,

  map_ims1 <- bmap +
    geom_sf(data = spdf_tech_areas,
            aes(fill = tech_color), color = grey50k,
            linewidth = .8, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = orgunit)) +
    scale_fill_identity() +
    labs(subtitle = glue(
      "<span style='color:{old_rose}'>**HIV**</span> **-** <span style='color:{burnt_sienna}'>**HIV, TB, OVC & COVID-19**</span> **-** \
      <span style='color:{scooter}'>**HIV, OVC & COVID-19**</span> **-** <span style='color:{denim_light}'>**TB, COVID-19**</span> **-** \
      <span style='color:{denim}'>**COVID-19**</span>"
    )) +
    theme(plot.subtitle = element_markdown())

  export_map(map_ims1, "Nigeria - IM Coverage")

  # "HIV" ~ old_rose,
  # "TB" ~ burnt_sienna,
  # "OVC" ~ scooter,
  # "COVID-19" ~ genoa

  map_ims2 <- bmap +
    geom_sf(data = spdf_tech_areas2,
            aes(fill = tech_color), color = grey10k,
            linewidth = .5, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = orgunit), size = 3) +
    scale_fill_identity() +
    facet_wrap(~tech_area) +
    theme(plot.subtitle = element_markdown(),
          strip.text = element_text(face = "bold", size = 15))

  export_map(map_ims2, "Nigeria - IM Coverage 2")

  map_hex <- bmap +
    geom_sf(data = spdf_hex_big,
            fill = NA, color = grey50k,
            linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7)

  export_map(map_hex, "Nigeria - Country Hexbins")

  # IM/Tech Areas Coverage

  df_ims_cov %>%
    gt(
      rowname_col = "STATE",
      groupname_col = "category"
    ) %>%
    tab_header(
      title = md("**USAID - HIV-AIDS & TB MECHANISMS**"),
      subtitle = "Distribution of Implementing Mechanisms by State & Technical Areas"

    ) %>%
    tab_options(
      table.font.size = 12,
      table.border.right.width = 1,
      table.border.left.color = grey10k
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "-"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = list(
        cell_text(color = usaid_black, style = "italic", weight = "bold")
      ),
      locations = cells_body(
        columns = 1,
        rows = everything()
      )
    ) %>%
    gtsave(filename = file.path(dir_graphics, glue::glue("HAT-IM Distribution.png")))


  # Country Map
  map_cntry <- bmap +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = orgunit))

  export_map(map_cntry, "Nigeria - Country Basemap")

  ## Summary of TX ----

  spdf_sites %>% gview

  spdf_sites %>%
    st_filter(spdf_cnty) %>%
    gview()

  bmap +
    geom_sf(data = spdf_tx,
            aes(fill = value),
            color = grey10k, linewidth = .6, linetype = "dotted") +
    scale_fill_si() +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.key.width = unit(.2, "npc"),
          legend.title = element_blank())

  ## Health facility by service level

  df_tx_agency_svc %>% glimpse()

  facs_pepfar
  facs_agency
  facs_pepfar_prim
  facs_pepfar_factype

  map_phc <- bmap +
    geom_sf(data = spdf_sites %>%
              mutate(
                facility_type = case_when(
                  facility_type != "Primary Health Center" ~ "Other",
                  TRUE ~ facility_type
                ),
                facility_type = factor(
                  facility_type,
                  levels = c("Primary Health Center", "Other"),
                  ordered =  T
                )) %>%
              arrange(desc(facility_type)),
            aes(fill = facility_type),
            shape = 21, size = 3, color = grey10k, alpha = .8) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states,
                 aes(label = str_replace(orgunit, " ", "\n")),
                 color = usaid_black, fontface = "bold") +
    scale_fill_manual(values = c("Primary Health Center" = denim,
                                 "Other" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES CATEGORIZATION",
         subtitle = "**PEPFAR** Supports **~1,800** health facilities. **45%**
         are **Primary Health Centers**<br/>where **20% [322K / 1.6M]**
         HIV+ have been enrolled in ART") +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_phc, "Nigeria - PHC and TX_CURR Enrollement")

  # Filter out sites outside
  map_phc2 <- bmap +
    geom_sf(data = spdf_sites %>%
            st_filter(spdf_cnty) %>%
            mutate(
              facility_type = case_when(
                facility_type != "Primary Health Center" ~ "Other",
                TRUE ~ facility_type
              ),
              facility_type = factor(
                facility_type,
                levels = c("Primary Health Center", "Other"),
                ordered =  T
              )) %>%
              arrange(desc(facility_type)),
          aes(fill = facility_type),
          shape = 21, size = 3, color = grey10k, alpha = .8) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states,
                 aes(label = str_replace(orgunit, " ", "\n")),
                 color = usaid_black, fontface = "bold") +
    scale_fill_manual(values = c("Primary Health Center" = denim,
                                 "Other" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES CATEGORIZATION",
         subtitle = "**PEPFAR** Supports **~1,800** health facilities. **45%**
         are **Primary Health Centers**<br/>where **20% [322K / 1.6M]**
         HIV+ have been enrolled in ART") +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_phc2, "Nigeria - PHC and TX_CURR Enrollement - cleaned")


  ## USAID - PHC Health facility by service level

  map_usaid_phc <- bmap +
    geom_sf(data = spdf_sites %>%
            st_filter(spdf_cnty) %>%
            filter(funding_agency == "USAID") %>%
            mutate(
             facility_type = case_when(
               facility_type != "Primary Health Center" ~ "Other",
               TRUE ~ facility_type
             ),
             facility_type = factor(
               facility_type,
               levels = c("Primary Health Center", "Other"),
               ordered =  T
             )) %>%
            arrange(desc(facility_type)),
          aes(fill = facility_type),
          shape = 21, size = 3, color = grey10k, alpha = .8) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = str_replace(orgunit, " ", "\n"))) +
    scale_fill_manual(values = c("Primary Health Center" = denim,
                                 "Other" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES [PRIMARY HEALTH CENTERS]",
         subtitle = "**USAID** Supports **600+** health facilities. **32%**
         are **Primary Health Centers**<br/>where **16% [95K / 599K]**
         HIV+ have been enrolled in ART") +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_usaid_phc, "Nigeria - USAID PHC and TX_CURR Enrollement")

  ## USAID - Health facility by service level

  map_usaid_host <- bmap +
    geom_sf(data = spdf_sites %>%
              st_filter(spdf_cnty) %>%
              filter(funding_agency == "USAID") %>%
              mutate(
                facility_type = case_when(
                  facility_type != "Hospital" ~ "Other",
                  TRUE ~ facility_type
                ),
                facility_type = factor(
                  facility_type,
                  levels = c("Hospital", "Other"),
                  ordered =  T
                )) %>%
              arrange(desc(facility_type)),
            aes(fill = facility_type),
            shape = 21, size = 3, color = grey10k, alpha = .8) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = str_replace(orgunit, " ", "\n")),
                 color = usaid_black, fontface = "bold") +
    scale_fill_manual(values = c("Hospital" = denim,
                                 "Other" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES [HOSPITALS]",
         subtitle = "**USAID** Supports **600+** health facilities. **59%**
         are **Hospitals**<br/>where **62% [374K / 599K]**
         HIV+ have been enrolled in ART") +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_usaid_host, "Nigeria - USAID Hospital and TX_CURR Enrollement")

  ## Other USAID Facilities

  map_usaid_other <- bmap +
    geom_sf(data = spdf_sites %>%
              st_filter(spdf_cnty) %>%
              filter(funding_agency == "USAID") %>%
              mutate(
                facility_type = case_when(
                  facility_type %ni% c("Hospital", "Primary Health Center") ~ "Other",
                  TRUE ~ facility_type
                ),
                facility_type = factor(
                  facility_type,
                  levels = c( "Hospital", "Primary Health Center", "Other"),
                  ordered =  T
                )) %>%
              arrange(desc(facility_type)),
            aes(fill = facility_type),
            shape = 21, size = 3, color = grey10k, alpha = .8) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = str_replace(orgunit, " ", "\n")),
                 color = usaid_black, fontface = "bold") +
    scale_fill_manual(values = c("Hospital" = denim,
                                 "Primary Health Center" = usaid_red,
                                 "Other" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES") +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_usaid_other, "Nigeria - USAID Fact Groups and TX_CURR Enrollement")

  ## Health facility by funding agency

  map_agencies <- bmap +
    geom_sf(data = spdf_sites %>%
              st_filter(spdf_cnty) %>%
              filter(funding_agency %in% c("USAID", "CDC")) %>%
              mutate(
                facility_type = case_when(
                  facility_type != "Hospital" ~ "Other",
                  TRUE ~ facility_type
                ),
                funding_agency = factor(
                  funding_agency,
                  levels = c("USAID", "CDC"),
                  ordered =  T
                )) %>%
              arrange(desc(funding_agency)),
            aes(fill = funding_agency),
            shape = 21, size = 3, color = grey10k, alpha = .8) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = str_replace(orgunit, " ", "\n")),
                 color = usaid_black, fontface = "bold") +
    scale_fill_manual(values = c("USAID" = denim,
                                 "CDC" = scooter_light,
                                 "NON-PEPFAR" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES [PEPFAR ONLY]",
         subtitle = glue("PEPFAR Supports about **~1,800** health facilities.
         **45%** are **Primary Health Centers** <br>where **20% [322K / 1.6M]**
         HIV+ have been enrolled in life-saving Antiretroviral Therapy (ART)<br>
         <span style='color:{denim}'>**USAID**</span> supports **37%** of patients
         at **35%** of the sites while <span style='color:{scooter_light}'>**CDC**</span> has **62%** and **65%** respectively.")) +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_agencies, "Nigeria - TX_CURR Distribution by Agency")

  # NOn-Supported facilities

  map_nonpepfar <- bmap +
    geom_point(data = df_tx_agency_svc %>%
                 filter(funding_agency == "NON-PEPFAR"),
               aes(longitude, latitude, fill = funding_agency),
               shape = 21, size = 3, color = grey10k, alpha = .8,
               show.legend = F) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = str_replace(orgunit, " ", "\n")),
                 color = usaid_black, fontface = "bold") +
    scale_fill_manual(values = c("USAID" = denim,
                                 "CDC" = scooter_light,
                                 "NON-PEPFAR" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES [NON-PEPFAR]",
         subtitle = glue("**{percent(facs_pepfar$p[facs_pepfar$funding_agency == 'NON-PEPFAR'])}** [{comma(facs_pepfar$n[facs_pepfar$funding_agency == 'NON-PEPFAR'])}] of **health facilities** are not supported by PEPFAR")) +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_nonpepfar, "Nigeria - Non-PEPFAR Supported Facilities")


# OUTPUTS =====

  df_tx_agency_svc %>%
    select(-ends_with("tude")) %>%
    write_csv(na = "",
              file = file.path(dir_cntry, "MER", "PEPFAR_Nigeria_primary_health_coverage.csv"))

  df_tx_agency_svc_cov %>%
    write_csv(na = "",
              file = file.path(dir_cntry, "MER", "PEPFAR_Nigeria_primary_health_coverage.csv"))
