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

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21-.*_Nigeria")
  file_site <- si_path() %>% return_latest("Site_IM_FY21-.*_Nigeria")

  file_site2 <- file.path(dir_cntry, "MER") %>%
    return_latest("FY24 Q1 TX_CURR.*.csv")

  file_facilities <- file.path(dir_cntry, "COPs/COP23-YR2/Data") %>%
    return_latest("HIV_AIDS Services Facility List")

  file_attrs <- file.path(dir_cntry, "Locations Data") %>%
    return_latest("Org.* Unit Attributes")

  file_usaid_locs <- file.path(dir_cntry, "Locations Data") %>%
    return_latest("GEOC.*SUPPORTED SITES.*")

  # Meta

  get_metadata(file_psnu)

  meta <- metadata

  cntry_uid <- get_ouuid(cntry)


# Functions  =====

  #' @title Export Map plot
  #'
  export_map <- function(mplot, name) {
    si_save(plot = mplot,
            filename = file.path(dir_graphics, glue::glue("{name}.png")),
            scale = 1.4,
            dpi = 350,
            width = 8, height = 6)
  }

# LOAD DATA =====

  # Geo data

  ## VC POlygons
  spdf_pepfar <- get_vcpolygons(folderpath = si_path("path_vector"))

  ## Country boundaries
  spdf_cnty <- spdf_pepfar %>% filter(uid == cntry_uid)

  ext_cntry <- st_bbox(spdf_cnty) %>%
    as.list() %>%
    as_tibble() %>%
    pivot_longer(everything()) %>%
    mutate(axis = str_sub(name, 1, 1),
           name = str_remove(name, "x|y")) %>%
    pivot_wider(names_from = axis, values_from = value)


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

  ## States boundaries
  spdf_states <- cntry_uid %>%
    get_ouorgs(level = get_ouorglevel(cntry, org_type = "prioritization")) %>%
    left_join(spdf_pepfar, ., by = "uid") %>%
    filter(!is.na(orgunit))

  ## Terrain data
  dt_terr <- get_raster(folderpath = si_path("path_raster"))

  # Health Facilities - USAID Sites

  file_facilities %>% excel_sheets()

  facs_sheet <- "Comprehensive List"

  df_facs <- file_facilities %>%
    read_excel(sheet = facs_sheet, skip = 4) %>%
    clean_names()

  df_facs %>% glimpse()

  # Health Facilities - USAID Sites (Updated)

  file_usaid_locs %>% excel_sheets()

  df_facs2 <- file_usaid_locs %>%
    read_excel(sheet = 1) %>%
    clean_names() %>%
    mutate(across(ends_with("tude"), as.numeric))

  df_facs2 %>% glimpse()

  df_facs2 %>% distinct(state)

  ## Site Attrs

  df_attrs <- file_attrs %>%
    read_csv() %>%
    clean_names() %>%
    select(-moh_site_id, hierarchy)

  ## Site locs

  df_locs <- datim_pull_hierarchy(cntry_uid, add_geom = T)

  df_locs <- df_locs %>%
    filter(level == max(level, na.rm = T)) %>%
    left_join(df_attrs[, c("datim_uid", "facility_type")],
              by = c("orgunituid" = "datim_uid"))

  df_facs2 %>%
    rename(facility_name = facility,
           lat = latitude,
           lon = longitude) %>%
    left_join(df_locs, by = c("facility_datim_id" = "orgunituid")) %>%
    filter(is.na(latitude))

  ## Site x IM

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

  ## Sites

  df_facs_clean <- df_facs %>%
    select(id = s_n, datim_uid, facility_name = facility_name_implementing_agent,
           facility_type, service_level, ownership, pop_setting = population_setting,
           im, partner_info, state, lga, ward, latitude, longitude,
           art:art_data_reported_to_datim)

  df_facs_svc <- df_facs_clean %>%
    pivot_longer(cols = art:art_data_reported_to_datim)

  df_facs_cat <- df_facs_clean %>%
    distinct(datim_uid, facility_name, state, service_level)

  ## TX_CURR

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

  df_tx_agency_svc <- df_locs %>%
    select(orgunituid, facility_type, latitude, longitude) %>%
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

  # Update USAID Sites

  df_tx_agency_svc <- df_facs2 %>%
    select(facility_datim_id, lat = latitude, lon = longitude) %>%
    left_join(df_tx_agency_svc, ., by = c("orgunituid" = "facility_datim_id")) %>%
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
           !(between(latitude, ext_cntry$y[1], ext_cntry$x[2]) |
           between(longitude, ext_cntry$x[1], ext_cntry$x[2])) |
             is.na(latitude)) %>%
    arrange(psnu, facility_type) %>%
    select(-c(lat, lon))

  ## Summary stats

  facs_pepfar <- df_tx_agency_svc %>%
    mutate(
      funding_agency = case_when(
        funding_agency %in% c("USAID", "CDC") ~ "PEPFAR",
        TRUE ~ funding_agency
      )
    ) %>%
    count(funding_agency) %>%
    mutate(p = n / sum(n))

  facs_pepfar_prim <- df_tx_agency_svc %>%
    filter(funding_agency %in% c("USAID", "CDC")) %>%
    count(funding_agency, facility_type) %>%
    mutate(p = n / sum(n))

  facs_agency <- df_tx_agency_svc %>%
    filter(funding_agency != "NON-PEPFAR") %>%
    count(funding_agency) %>%
    mutate(p = n / sum(n))

  ## Site geo-data

  spdf_sites <- df_tx_agency_svc %>%
    select(-c(lat, lon)) %>%
    filter(!is.na(latitude),
           between(latitude, ext_cntry$y[1], ext_cntry$x[2]),
           between(longitude, ext_cntry$x[1], ext_cntry$x[2])) %>%
    spdf_points()

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

  map_hex <- bmap +
    geom_sf(data = spdf_hex_big,
            fill = NA, color = grey50k,
            linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7)

  export_map(map_hex, "Nigeria - Country Hexbins")

  map_cntry <- bmap +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = orgunit))

  export_map(map_cntry, "Nigeria - Country Basemap")

  ## Summary of TX

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

  map_phc <- bmap +
    geom_point(data = df_tx_agency_svc %>%
                 filter(funding_agency %in% c("USAID", "CDC"),
                        between(latitude, ext_cntry$y[1], ext_cntry$x[2]),
                        between(longitude, ext_cntry$x[1], ext_cntry$x[2])) %>%
                 mutate(facility_type = case_when(
                   facility_type != "Primary Health Center" ~ "Other",
                   TRUE ~ facility_type
                 )),
               aes(longitude, latitude, fill = facility_type),
               shape = 21, size = 3, color = grey10k, alpha = .5) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = orgunit)) +
    scale_fill_manual(values = c("Primary Health Center" = denim,
                                 "Other" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES",
         subtitle = "**PEPFAR** Supports **~1,800** health facilities. **45%**
         are **Primary Health Centers** where **20% [322K / 1.6M]**
         HIV+ have been enrolled in ART") +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_phc, "Nigeria - PHC and TX_CURR Enrollement")

  ## USAID - PHC Health facility by service level

  map_usaid_phc <- bmap +
    geom_point(data = df_tx_agency_svc %>%
                 filter(funding_agency == "USAID",
                        between(latitude, ext_cntry$y[1], ext_cntry$x[2]),
                        between(longitude, ext_cntry$x[1], ext_cntry$x[2])) %>%
                 mutate(facility_type = case_when(
                   facility_type != "Primary Health Center" ~ "Other",
                   TRUE ~ facility_type
                 )),
               aes(longitude, latitude, fill = facility_type),
               shape = 21, size = 3, color = grey10k, alpha = .5) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = orgunit)) +
    scale_fill_manual(values = c("Primary Health Center" = denim,
                                 "Other" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES",
         subtitle = "**USAID** Supports **600+** health facilities. **32%**
         are **Primary Health Centers** where **16% [95K / 599K]**
         HIV+ have been enrolled in ART") +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_usaid_phc, "Nigeria - USAID PHC and TX_CURR Enrollement")

  ## USAID - Health facility by service level

  map_usaid_host <- bmap +
    geom_point(data = df_tx_agency_svc %>%
                 filter(funding_agency == "USAID",
                        between(latitude, ext_cntry$y[1], ext_cntry$x[2]),
                        between(longitude, ext_cntry$x[1], ext_cntry$x[2])) %>%
                 mutate(facility_type = case_when(
                   facility_type != "Hospital" ~ "Other",
                   TRUE ~ facility_type
                 )),
               aes(longitude, latitude, fill = facility_type),
               shape = 21, size = 3, color = grey10k, alpha = .5) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = orgunit)) +
    scale_fill_manual(values = c("Hospital" = denim,
                                 "Other" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES",
         subtitle = "**USAID** Supports **600+** health facilities. **59%**
         are **Hospitals** where **62% [374K / 599K]**
         HIV+ have been enrolled in ART") +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_usaid_host, "Nigeria - USAID Hospital and TX_CURR Enrollement")

  ## Health facility by funding agency

  map_agencies <- bmap +
    geom_point(data = df_tx_agency_svc %>%
                 filter(funding_agency %in% c("USAID", "CDC"),
                        between(latitude, ext_cntry$y[1], ext_cntry$x[2]),
                        between(longitude, ext_cntry$x[1], ext_cntry$x[2])),
               aes(longitude, latitude, fill = funding_agency),
               shape = 21, size = 3, color = grey10k, alpha = .5) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = orgunit)) +
    scale_fill_manual(values = c("USAID" = denim,
                                 "CDC" = scooter_light,
                                 "NON-PEPFAR" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES",
         subtitle = glue("PEPFAR Supports about **~1,800** health facilities.
         **45%** are **Primary Health Centers** <br>where **20% [322K / 1.6M]**
         HIV+ have been enrolled in life-saving Antiretroviral Therapy (ART)<br>
         <span style='color:{denim}'>**USAID**</span> supports **37%** of patients
         at **35%** of the sites while <span style='color:{scooter_light}'>**CDC**</span> has **62%** and **62%**")) +
    si_style_map() +
    theme(legend.title = element_blank(),
          plot.subtitle = element_markdown())

  export_map(map_agencies, "Nigeria - TX_CURR Distribution by Agency")

  # NOn-Supported facilities

  map_nonpepfar <- bmap +
    geom_point(data = df_tx_agency_svc %>%
                 filter(funding_agency == "NON-PEPFAR"),
               aes(longitude, latitude, fill = funding_agency),
               shape = 21, size = 3, color = grey10k, alpha = .5,
               show.legend = F) +
    geom_sf(data = spdf_states,
            fill = NA, color = grey50k, linewidth = .6, linetype = "dotted") +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey10k, linewidth = 2) +
    geom_sf(data = spdf_cnty,
            fill = NA, color = grey90k, linewidth = .7) +
    geom_sf_text(data = spdf_states, aes(label = orgunit)) +
    scale_fill_manual(values = c("USAID" = denim,
                                 "CDC" = scooter_light,
                                 "NON-PEPFAR" = old_rose_light)) +
    labs(x = "", y = "",
         title = "NIGERIA - HEALTH FACILITIES",
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
