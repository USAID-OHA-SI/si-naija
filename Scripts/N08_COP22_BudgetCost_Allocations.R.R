##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: COP22 - PEPFAR Budget/Costing Allocations
##  LICENCE: MIT
##  DATE:    2022-01-12


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

  source("./Scripts/N00_Utilties.R")

# Paths ----

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
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v2_1_N.*.zip$")

  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v2_1_N.*.zip$")

  # MER Data - get the latest MSD NAT SUBNAT
  file_nat_subnat <- dir_merdata %>%
    return_latest(pattern = "^MER_.*NAT_SUBNAT_FY15-22_\\d{8}_v\\d{1}_\\d{1}.zip$")

  # Shapefile path
  file_shp <- dir_geodata %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE)

# Params ----

  cntry <- "Nigeria"
  agency <- "USAID"
  agencies <- c("USAID", "CDC")

  inds <- c("PLHIV", "POP_EST",
            "HTS_TST", "HTS_TST_POS",
            "TX_CURR", "TX_RTT", "TX_NEW", "TX_ML", "TX_NET_NEW",
            "TX_PVLS", "TX_PVLS_D")

  ttl_disaggs <- c("Total Numerator", "Total Denominator")

  rep_pd <- file_site_im %>% identify_pd()
  rep_fy <- rep_pd %>%
    str_sub(3, 4) %>%
    paste0("20", .) %>%
    as.integer()

# Data ----

  # Location ----

  ## Terrain Raster
  terr <- get_raster(path = dir_terr)

  ## GEO - PEPFAR Orgs boundaries
  spdf_pepfar <- file_shp %>% read_sf()

  # Sites
  site_lvl <- get_ouorglevel(operatingunit = cntry, org_type = "facility")

  df_sites_locs <- extract_locations(country = cntry, level = site_lvl) %>%
    extract_facilities() %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    select(id, name, latitude, longitude)

  # Attibutes
  df_attrs <- get_attributes(country = cntry)

  spdf_nga <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(operatingunit))

  spdf_nga %>% glimpse()
  spdf_nga %>% pull(label) %>% unique()


  spdf_adm0 <- spdf_nga %>% filter(label == "country")
  spdf_adm1 <- spdf_nga %>% filter(label == "prioritization")
  spdf_adm2 <- spdf_nga %>% filter(label == "community")

  # MSD - Nat SubNat ----
  df_nats <- file_nat_subnat %>% read_msd()

  df_nats <- df_nats %>%
    filter(countryname == cntry,
           indicator %in% inds[1:2],
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, psnuuid, psnu, snuprioritization, indicator) %>%
    summarise(value = sum(targets, na.rm = TRUE), .groups = "drop") %>%
    set_prioritization() %>%
    relocate(snupriority, .after = snuprioritization)

  # Reshape for mapping
  df_nats2 <- df_nats %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    clean_names()

  # MSD - Sites x IM ----
  df_sites <- file_site_im %>%
    read_msd() %>%
    clean_agency()

  df_sites %>% glimpse()
  df_sites %>% distinct(fundingagency)

  # Above Sites Targets
  df_psnu_targets <- df_sites %>%
    filter(fundingagency != "Dedup",
           indicator %in% inds[3:length(inds)],
           !is.na(targets),
           standardizeddisaggregate %in% ttl_disaggs) %>%
    group_by(fiscal_year, fundingagency, psnuuid, psnu, indicator) %>%
    summarise(across(targets, sum, na.rm = T), .groups = "drop") %>%
    rename(period = fiscal_year, value = targets) %>%
    mutate(period_type = "targets") %>%
    relocate(period, .before = 1) %>%
    relocate(period_type, .before = value)

  df_psnu_targets %>% glimpse()
  df_psnu_targets %>% head()

  # Agency Coverage & Prioritization
  df_cov <- df_psnu_targets %>%
    filter(period == rep_fy, indicator == "TX_CURR") %>%
    select(fundingagency, psnuuid, psnu) %>%
    right_join(df_nats2 %>% filter(fiscal_year == rep_fy),
               by = c("psnuuid", "psnu")) %>%
    mutate(fundingagency = case_when(
      is.na(fundingagency) & psnu == "Taraba" ~ "USAID",
      is.na(fundingagency) & psnu == "Abia" ~ "CDC",
      is.na(fundingagency) & psnu %in% c("Ebonyi", "Anambra") ~ "GF",
      TRUE ~ fundingagency
    ))

  # Results by Agency
  df_agency_cascade <- df_sites %>%
    clean_indicator() %>%
    filter(fundingagency != "DEDUP",
           indicator %in% inds[3:length(inds)],
           standardizeddisaggregate %in% ttl_disaggs) %>%
    group_by(fiscal_year, fundingagency, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets), sum, na.rm = T),
              .groups = "drop") %>%
    group_by(fiscal_year, indicator) %>%
    mutate(cum_prop = cumulative / sum(cumulative, na.rm = T),
           targets_prop = targets / sum(targets, na.rm = T)) %>%
    ungroup()

  # Results Agency x Site
  df_sites_cascade <- df_sites %>%
    clean_indicator() %>%
    filter(fundingagency != "DEDUP",
           indicator %in% inds[3:length(inds)],
           #sitetype != "Above Site",
           standardizeddisaggregate %in% ttl_disaggs) %>%
    group_by(fiscal_year, fundingagency,
             psnuuid, psnu, communityuid, community,
             orgunituid, sitename, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative), sum, na.rm = T),
              .groups = "drop") %>%
    reshape_msd()

  df_sites_cascade %>% glimpse()
  df_sites_cascade %>% distinct(indicator)

  # HTS YIELD
  df_sites_cascade <- df_sites_cascade %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS")) %>%
    group_by(period, fundingagency,
             psnuuid, psnu, communityuid, community,
             orgunituid, sitename, period_type) %>%
    summarise(value = value[indicator == "HTS_TST_POS"] /
                value[indicator == "HTS_TST"], .groups = "drop") %>%
    mutate(indicator = "HTS_YIELD") %>%
    bind_rows(df_sites_cascade, .) %>%
    arrange(period, fundingagency, psnu, community, sitename, indicator)

  # TX_CURR_LAGS
  df_sites_cascade <- df_sites_cascade %>%
    filter(indicator == "TX_CURR",
           period_type == "results") %>%
    group_by(fundingagency,
             psnuuid, psnu, communityuid, community,
             orgunituid, sitename, period_type) %>%
    mutate(TX_CURR_LAG1 = lag(value, 1, order_by = period),
           TX_CURR_LAG2 = lag(value, 2, order_by = period)) %>%
    ungroup() %>%
    dplyr::select(-c(indicator, value)) %>%
    pivot_longer(cols = starts_with("TX_CURR_"),
                 names_to = "indicator",
                 values_to = "value") %>%
    bind_rows(df_sites_cascade, .) %>%
    arrange(period, fundingagency, psnu, community, sitename, indicator)

  # Linkage
  df_sites_cascade <- df_sites_cascade %>%
    group_by(period, fundingagency,
             psnuuid, psnu, communityuid, community,
             orgunituid, sitename, period_type) %>%
    summarise(value = value[indicator == "TX_NEW"] /
                value[indicator == "HTS_TST_POS"],
              .groups = "drop") %>%
    mutate(indicator == "LINKAGE") %>%
    bind_rows(df_sites_cascade, .)

  # Retention
  df_sites_cascade <- df_sites_cascade %>%
    group_by(period, fundingagency,
             psnuuid, psnu, communityuid, community,
             orgunituid, sitename, period_type) %>%
    summarise(value = value[indicator == "TX_CURR"] /
                (value[indicator == "TX_CURR_LAG1"] + value[indicator == "TX_NEW"]),
              .groups = "drop") %>%
    mutate(indicator = "RETENTION") %>%
    bind_rows(df_sites_cascade, .)

# VIZ ----

  spdf_adm1_prio <- spdf_adm1 %>%
    left_join(df_cov, by = c("uid" = "psnuuid")) %>%
    st_transform(crs = st_crs(3857)) %>%
    #st_drop_geometry() %>%
    #st_area()
    #units::set_units(km^2)
    mutate(
      size = units::set_units(st_area(.), km^2),
      color_agency = case_when(
        fundingagency == "USAID" ~ usaid_blue,
        fundingagency == "CDC" ~ usaid_lightblue,
        TRUE ~ trolley_grey_light
      ),
      color_prio = case_when(
        snupriority == "Red" ~ old_rose,
        snupriority == "Yellow" ~ golden_sand,
        snupriority == "Green" ~ genoa,
        TRUE ~ grey60k
      ))

  spdf_adm1_prio %>% dview()

  ggplot() +
    geom_sf(data = spdf_adm1_prio,
            aes(fill = color_agency),
            color = grey10k,
            size = .2) +
    geom_sf_label(data = spdf_adm1_prio,
                  aes(label = psnu, color = color_prio),
                  fill = "white",
                  size = 4,
                  label.size = NA,
                  fontweight = "bold") +
    scale_fill_identity() +
    scale_color_identity() +
    si_style_map()

  spdf_adm1_prio %>%
    pull(psnu) %>%
    first() %>%
    map(function(.x) {

      spdf <- spdf_adm1_prio %>% filter(psnu == .x)

      m <- ggplot() +
        geom_sf(data = spdf,
                aes(fill = color_agency),
                color = grey10k,
                size = .2) +
        geom_sf_label(data = spdf,
                      aes(label = psnu, color = color_prio),
                      fill = "white",
                      size = 4,
                      label.size = NA,
                      fontface = "bold") +
        scale_fill_identity() +
        scale_color_identity() +
        labs(x = "", y = "") +
        si_style_map()

      print(m)

      return(m)
    })
