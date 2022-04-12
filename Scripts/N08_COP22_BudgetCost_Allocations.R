##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: COP22 - PEPFAR Budget/Costing Allocations
##  LICENCE: MIT
##  DATE:    2022-01-12
##  UPDATE:  2020-02-23


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
  library(scales)
  library(glue)
  library(ggtext)
  library(cowplot)
  library(ggbeeswarm)
  #library(ggforce)
  library(extrafont)

  source("Scripts/N00_Utilities.R")

# Paths ----

  dir_data <- "Data"
  dir_data_cop <- "Data/COP22"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

# Files ----

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "Site_IM_FY20.*_N.*.zip$")

  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "PSNU_IM_FY20.*_N.*.zip$")

  # MER Data - get the latest MSD NAT SUBNAT
  file_nat_subnat <- dir_merdata %>%
    return_latest(pattern = "NAT_SUBNAT.*.zip$")

  # Program Data sets
  file_sites <- dir_data_cop %>%
    return_latest(pattern = "^Site Volume .*.xls")

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

  #rep_pd <- file_site_im %>% identify_pd()
  rep_pd <- "FY21Q4"

  rep_fy <- rep_pd %>%
    str_sub(3, 4) %>%
    paste0("20", .) %>%
    as.integer()

# Data ----

  # Location ----

  ## Terrain Raster
  terr <- gisr::get_raster(path = dir_terr)

  ## GEO - PEPFAR Orgs boundaries
  spdf_pepfar <- file_shp %>% read_sf()

  # Sites
  site_lvl <- get_ouorglevel(operatingunit = cntry, org_type = "facility")

  df_sites_locs_raw <- extract_locations(country = cntry, level = site_lvl) %>%
    extract_facilities() %>%
    select(id, name, latitude, longitude)

  df_sites_locs <- df_sites_locs_raw %>%
    filter(!is.na(latitude), !is.na(longitude))

  # orgs
  df_attrs <- get_attributes(country = cntry)

  spdf_nga <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(operatingunit))

  spdf_nga %>% glimpse()
  spdf_nga %>% pull(label) %>% unique()

  # Extract boundaries
  spdf_adm0 <- spdf_nga %>% filter(label == "country")
  spdf_adm1 <- spdf_nga %>% filter(label == "prioritization")
  spdf_adm2 <- spdf_nga %>% filter(label == "community")

  # Country HexBins
  spdf_hex20km <- spdf_adm0 %>%
    st_transform(st_crs(3857)) %>%
    generate_hexbins(size = 20000) %>%
    st_transform(st_crs(4326)) %>%
    st_as_sf() %>%
    mutate(id = row_number())

  spdf_hex30km <- spdf_adm0 %>%
    st_transform(st_crs(3857)) %>%
    generate_hexbins(size = 30000)%>%
    st_transform(st_crs(4326)) %>%
    st_as_sf() %>%
    mutate(id = row_number())

  spdf_hex50km <- spdf_adm0 %>%
    st_transform(st_crs(3857)) %>%
    generate_hexbins(size = 50000)%>%
    st_transform(st_crs(4326)) %>%
    st_as_sf() %>%
    mutate(id = row_number())

  spdf_hex50km %>% gview()
  spdf_hex50km %>% ggplot() + geom_sf(aes(fill = id))


  # Country / Site Data
  utils::browseURL(dirname(file_sites))
  utils::browseURL(file_sites)

  df_psnu_tx_curr_vol <- file_sites %>%
    read_excel(sheet = "SiteTXCURRVol by SNU_Agency +AT", skip = 1) %>%
    clean_names() %>%
    filter(!is.na(agency)) %>%
    mutate(state = case_when(is.na(state) ~ "Lagos", TRUE ~ state)) %>%
    dplyr::select(agency, state, large_volume_site_3:v_low_volume_site_6) %>%
    pivot_longer(cols = large_volume_site_3:v_low_volume_site_6,
                 names_to = "category",
                 values_to = "value") %>%
    mutate(category = str_remove(category, "_volume_site_\\d{1}"),
           category = case_when(
             category == "v_low" ~ "very low",
             TRUE ~ category),
           agency = case_when(
             agency == "HHS/CDC" ~ "CDC",
             TRUE ~ agency)) %>%
    group_by(agency, state) %>%
    mutate(share = value / sum(value, na.rm = TRUE)) %>%
    ungroup()


  df_psnu_tx_curr_sites <- file_sites %>%
    read_excel(sheet = "NoOfSiteVolCategoryBySNU+AT", skip = 0) %>%
    clean_names() %>%
    filter(!is.na(agency)) %>%
    mutate(state = case_when(is.na(state) ~ "Lagos", TRUE ~ state)) %>%
    dplyr::select(agency, state, large_volume_site_3:v_low_volume_site_6) %>%
    pivot_longer(cols = large_volume_site_3:v_low_volume_site_6,
                 names_to = "category",
                 values_to = "sites") %>%
    mutate(category = str_remove(category, "_volume_site_\\d{1}"),
           category = case_when(
             category == "v_low" ~ "very low",
             TRUE ~ category),
           agency = case_when(
             agency == "HHS/CDC" ~ "CDC",
             TRUE ~ agency)) %>%
    group_by(agency, state) %>%
    mutate(sites_share = sites / sum(sites, na.rm = TRUE))

  df_psnu_tx_curr_vol <- df_psnu_tx_curr_vol %>%
    left_join(df_psnu_tx_curr_sites, by = c("agency", "state", "category"))


  # MSD - Nat SubNat ----

  df_subnats_raw <- file_nat_subnat %>% read_msd()

  # Extract Country historical ranking for SCA
  df_hiv_prev <- df_subnats_raw %>%
    filter(indicator %in% c("PLHIV", "POP_EST"),
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, countryname, indicator) %>%
    summarise(value = sum(targets, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    arrange(desc(fiscal_year), desc(PLHIV), countryname) %>%
    mutate(prevalence = round(PLHIV / POP_EST * 100, 2)) %>%
    clean_names()

  # df_hiv_prev %>%
  #   filter(countryname == cntry) %>%
  #   write_csv(file = file.path(dir_dataout, "Nigeria - PLHIV Historical data.csv"), na = "")

  # Explore POP Disags
  df_subnats_raw %>%
    filter(countryname == cntry,
           indicator %in% c("PLHIV", "POP_EST")) %>%
    distinct(fiscal_year, indicator, standardizeddisaggregate, trendscoarse) %>%
    arrange(desc(fiscal_year)) %>%
    prinf()

  # Calculate FY21 Prevalence for PEDS vs Adults
  df_pops <- df_subnats_raw %>%
    filter(countryname == cntry,
           fiscal_year == rep_fy,
           indicator %in% c("PLHIV", "POP_EST"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex")) %>%
    group_by(fiscal_year, psnuuid, psnu, indicator, trendscoarse) %>%
    summarise(value = sum(targets, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    mutate(prevalence = round(PLHIV / POP_EST * 100, 2)) %>%
    clean_names()

  # Set HIV Prioritization by states + POPs
  df_nats <- df_subnats_raw %>%
    filter(countryname == cntry,
           indicator %in% c("PLHIV", "POP_EST"),
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, psnuuid, psnu, snuprioritization, indicator) %>%
    summarise(value = sum(targets, na.rm = TRUE), .groups = "drop") %>%
    set_prioritization() %>%
    relocate(snupriority, .after = snuprioritization)

  # Reshape for mapping
  df_nats2 <- df_nats %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    clean_names()

  # MSD - PSNU/Sites x IM ----

  # PSNU
  df_psnu <- file_psnu_im %>%
    read_msd() %>%
    clean_agency() %>%
    filter(fundingagency != "DEDUP")

  # Sites
  df_sites <- file_site_im %>%
    read_msd() %>%
    clean_agency() %>%
    filter(fundingagency != "DEDUP")

  df_sites %>% glimpse()
  df_sites %>% distinct(fundingagency)

  # IP Coverage table ----
  df_ip <- df_sites %>%
    filter(fiscal_year == rep_fy, primepartner != "TBD") %>%
    distinct(fundingagency, psnu, primepartner)

  df_ip_usaid <- df_ip %>%
    filter(fundingagency == "USAID",
           psnu != "_Military Nigeria") %>%
    group_by(primepartner) %>%
    summarise(states = paste(psnu, collapse = ", ")) %>%
    ungroup() %>%
    rename(`Implementing Partners` = primepartner)

  df_ip_usaid2 <- df_ip %>%
    filter(fundingagency == "USAID",
           psnu != "_Military Nigeria") %>%
    group_by(psnu) %>%
    summarise(n = n_distinct(primepartner),
              partners = paste(primepartner, collapse = ", ")) %>%
    ungroup() %>%
    arrange(desc(n))

  # Agency Sites Coverage ----
  df_agency_sites <- df_sites %>%
    filter(fiscal_year == rep_fy, sitename != "Data reported above Site level") %>%
    distinct(fundingagency, psnuuid, psnu,
             communityuid, community, orgunituid, sitename)

  df_agency_sites <- df_agency_sites %>%
    left_join(df_sites_locs, by = c("orgunituid" = "id")) %>%
    select(-name)

  df_agency_psnu_sites <- df_agency_sites %>%
    group_by(fundingagency, psnuuid, psnu) %>%
    summarise(site_uids = n_distinct(orgunituid),
              site_names = n_distinct(sitename), .groups = "drop")

  # Site Summary of MMD ----

  df_sites_tx_mmd <- df_sites %>%
    filter(fiscal_year == rep_fy,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/ARVDispense/HIVStatus",
           sitename != "Data reported above Site level",) %>%
    group_by(fiscal_year, fundingagency, psnuuid, psnu,
             communityuid, community, orgunituid, sitename,
             indicator, otherdisaggregate) %>%
    summarize(across(c(starts_with("qtr"), cumulative), sum, na.rm = T), .groups = "drop") %>%
    reshape_msd() %>%
    clean_arv_dispense(add_3plus = T) %>%
    group_by(period, fundingagency, psnuuid, psnu,
             communityuid, community, orgunituid, sitename, indicator) %>%
    mutate(share = value / sum(value[!otherdisaggregate %in% c("3+", "tn")]))

  df_sites_tx_mmd <- df_sites_tx_mmd %>%
    left_join(df_sites_locs, by = c("orgunituid" = "id"))

  df_sites_tx_mmd %>% glimpse()


  # Site Summaries ----
  df_sites <- df_sites %>% clean_indicator()

  df_sites %>% glimpse()

  df_sites %>%
    filter(fiscal_year == rep_fy,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, fundingagency, psnu, sitename) %>%
    summarise(
      categery = case_when(

      ))


  df_sites_sum <- df_sites %>%
    filter(sitename != "Data reported above Site level",
           indicator %in% inds[3:length(inds)],
           standardizeddisaggregate %in% ttl_disaggs) %>%
    group_by(fiscal_year, fundingagency, psnuuid, psnu,
             communityuid, community, orgunituid, sitename, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative), sum, na.rm = T), .groups = "drop") %>%
    reshape_msd() %>%
    mutate(fiscal_year = as.integer(paste0("20", str_sub(period, 3, 4)))) %>%
    relocate(fiscal_year, .before = 1)

  df_sites_sum %>% glimpse()


  # Above Sites Summaries - Results / Targets
  df_psnu_sum <- df_sites %>%
    filter(indicator %in% inds[3:length(inds)],
           standardizeddisaggregate %in% ttl_disaggs) %>%
    group_by(fiscal_year, fundingagency, psnuuid, psnu, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets), sum, na.rm = T), .groups = "drop") %>%
    reshape_msd() %>%
    mutate(fiscal_year = as.integer(paste0("20", str_sub(period, 3, 4)))) %>%
    relocate(fiscal_year, .before = 1)

  # Above Sites Summaries - Shares
  df_psnu_sum <- df_psnu_sum %>%
    group_by(fiscal_year, indicator, period_type) %>%
    mutate(ou_share = value / sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(fiscal_year, period, fundingagency, psnu, indicator, period_type) %>%
    group_by(fiscal_year, fundingagency, indicator, period_type) %>%
    mutate(agency_share = value / sum(value, na.rm = TRUE)) %>%
    ungroup()

  df_psnu_sum %>% glimpse()
  df_psnu_sum %>% head()

  # Agency Coverage & Prioritization
  df_nats2 %>%
    filter(fiscal_year == rep_fy +1) %>%
    prinf()

  df_agency_cov <- df_psnu_sum %>%
    filter(fiscal_year %in% c(rep_fy +1),
           !str_detect(psnu, "^_Mil"),
           indicator == "TX_CURR",
           period_type == "targets") %>%
    distinct(fundingagency, psnuuid, psnu) %>% #prinf()
    mutate(
      fundingagency = case_when(
        psnu == "Lagos" ~ "USAID & CDC",
        TRUE ~ fundingagency
      )) %>%
    distinct() %>%
    right_join(df_nats2 %>% filter(fiscal_year == rep_fy), by = c("psnuuid", "psnu")) %>%
    relocate(fiscal_year, .before = 1) %>%
    mutate(fundingagency = case_when(
      is.na(fundingagency) & psnu == "Taraba" ~ "USAID",
      is.na(fundingagency) & psnu == "Abia" ~ "CDC",
      is.na(fundingagency) & psnu %in% c("Ebonyi", "Anambra") ~ "GF",
      TRUE ~ fundingagency
    ))

  df_agency_cov %>% glimpse()

  # Results by Agency ----
  df_agency_cascade <- df_sites %>%
    clean_indicator() %>%
    filter(indicator %in% inds[3:length(inds)],
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

  # PEDS / Adults PLHIV Prevalence ----
  df_pops %>%
    mutate(age = case_when(
      trendscoarse == "<15" ~ "p",
      trendscoarse == "15+" ~ "a",
      TRUE ~ NA_character_
    )) %>%
    select(-fiscal_year, - psnuuid, -trendscoarse) %>%
    arrange(desc(prevalence), psnu) %>%
    pivot_wider(names_from = age,
                values_from = c(plhiv, pop_est, prevalence)) %>%
    select(psnu, ends_with("a"), ends_with("p")) %>%
    #write_csv(file = file.path(dir_dataout, "NIGERIA-C_ALHIV Prevalence.csv"))
    gt(rowname_col = "psnu") %>%
    tab_spanner(label = "PEDS", columns = ends_with("p")) %>%
    tab_spanner(label = "ADULTS", columns = ends_with("p")) %>%
    tab_header(title = "NIGERIA - CY21 C/ALHIV Prevalence",
               subtitle = "Note: Peds are <15 and Adults are 15+") %>%
    cols_label(
      plhiv_p = "PLHIV",
      plhiv_a = "PLHIV",
      pop_est_p = "POP_EST",
      pop_est_a = "POP_EST",
      prevalence_p = "Prevalence",
      prevalence_a = "Prevalence"
    ) %>%
    opt_all_caps(
      all_caps = TRUE
    ) %>%
    fmt_number(
      columns = starts_with(c("PLHIV", "POP_EST")),
      decimals = 0
    ) %>%
    gtsave(filename = file.path(dir_graphics, "NIGERIA-C_ALHIV Prevalence.png"))

  # Partners Coverage Table ----
  df_ip_usaid %>%
    gt(rowname_col = "psnu") %>%
    opt_all_caps( all_caps = TRUE) %>%
    cols_width(
      `Implementing Partners` ~ px(250),
      everything() ~ px(300)) %>%
  gtsave(filename = file.path(dir_graphics, "USIAD - States Coverage by Partner.png"))

  df_ip_usaid2 %>%
    gt() %>%
    opt_all_caps(all_caps = TRUE) %>%
    cols_width(
      `psnu` ~ px(100),
      n ~ px(20)) %>%
    gtsave(filename = file.path(dir_graphics, "USIAD - Partners Coverage by States.png"))


  # Country Basemap ----
  basemap <- terrain_map(countries = cntry,
                         adm0 = spdf_adm0,
                         adm1 = spdf_adm1,
                         terr = terr,
                         mask = TRUE)

  map_cntry <- basemap +
    geom_sf_text(data = spdf_adm1,
            aes(label = name),
            size = 4, color = grey80k)

  map_cntry %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Country Context.png"),
      width = 8, height = 7)

  # hex map
  map_hex <- spdf_hex50km %>%
    ggplot() +
    geom_sf(fill = NA, size = .2) +
    geom_sf(data = spdf_adm1,
            fill = NA, size = .5, color = grey50k) +
    # geom_sf_text(data = spdf_adm1,
    #              aes(label = name),
    #              color = usaid_black,
    #              size = 3) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    labs(x = "", y = "") +
    si_style_map()

  map_hex %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Country Context2.png"),
      width = 8, height = 7)


  # Country hex boxes


  # Agency Coverage ----
  df_agency_cov_colors <- df_agency_cov %>%
    mutate(
      color_fill_agency = case_when(
        fundingagency == "USAID" ~ usaid_medblue,
        fundingagency == "CDC" ~ usaid_lightblue,
        fundingagency == "USAID & CDC" ~ moody_blue,
        TRUE ~ grey30k
      ),
      color_text_psnu = case_when(
        fundingagency == "USAID" ~ grey10k,
        fundingagency == "CDC" ~ grey90k,
        fundingagency == "USAID & CDC" ~ grey10k,
        TRUE ~ grey90k
      ),
      color_fill_prio = case_when(
        snupriority == "red" ~ usaid_red,
        snupriority == "yellow" ~ old_rose,
        snupriority == "green" ~ genoa,
        TRUE ~ grey10k
      ))

  spdf_cov <- spdf_adm1 %>%
    left_join(df_agency_cov_colors,
              by = c("uid" = "psnuuid"))

  # Agency Areas summary ----
  df_agency_psnu_sites %>%
    group_by(fundingagency) %>%
    summarise(sites = sum(site_uids))

  df_sites_cov <- df_sites_sum %>%
    filter(fiscal_year == rep_fy,
           #period == rep_pd,
           period_type == "cumulative",
           indicator == "TX_CURR") %>%
    mutate(fundingagency = case_when(
      psnu == "Lagos" ~ "USAID & CDC",
      TRUE ~ fundingagency
    )) %>%
    group_by(fundingagency) %>%
    summarise(sites = n_distinct(orgunituid),
              art = sum(value, na.rm = T), .groups = "drop")

  n_agencies <- spdf_cov %>%
    st_transform(crs = st_crs(3857)) %>%
    group_by(fundingagency) %>%
    summarise(states = n_distinct(psnu),
              geometry = st_union(geometry), .groups = "drop") %>%
    mutate(area = st_area(geometry),
           area = units::set_units(area, value = km^2),
           area = as.numeric(area),
           area_share = area / sum(area)) %>%
    st_drop_geometry() %>%
    arrange(desc(area), states) %>%
    left_join(df_sites_cov, by = "fundingagency") %>%
    rename(agency = fundingagency) %>%
    mutate(sites_per_km = sites / area,
           patients_per_km = art / area * 1000) %>%
    relocate(sites_per_km, .after = sites)

  n_usaid <- n_agencies %>%
    filter(agency == "USAID") %>%
    pull(states)

  n_cdc <- n_agencies %>%
    filter(agency == "CDC") %>%
    pull(states)

  n_gf <- n_agencies %>%
    filter(agency == "GF") %>%
    pull(states)

  # Table Coverage ----
  tbl_cov <- n_agencies %>%
    gt() %>%
    tab_header(title = "Nigeria - States & Sites Coverage by Agency",
               subtitle = "USAID is covering more than 60% of the country") %>%
    cols_label(states = "States",
               area = "area (KM^2)",
               area_share = "% COUNTRY",
               sites_per_km = "# of S. / 1k KM^2",
               patients_per_km = "# P. / KM^2") %>%
    cols_hide(sites_per_km) %>%
    opt_all_caps(all_caps = TRUE) %>%
    cols_width(
      agency ~ px(120),
      states ~ px(50),
      area ~ px(120),
      everything() ~ px(100)) %>%
    fmt_number(columns = c(area, sites, art, patients_per_km), decimals = 0) %>%
    fmt_number(columns = c(sites_per_km), decimals = 4) %>%
    fmt_percent(columns = area_share, decimals = 1) %>%
    fmt_missing(columns = everything(), missing_text = "-") %>%
    tab_footnote(footnote = "Areas are approximates",
                 locations = cells_column_labels(columns = area)) %>%
    tab_style(
      style = list(cell_text(color = usaid_medblue, weight = "bold")),
      locations = cells_body(columns = agency, rows = agency == "USAID")
    ) %>%
    tab_style(
      style = list(cell_text(color = usaid_lightblue, weight = "bold")),
      locations = cells_body(columns = agency, rows = agency == "CDC")
    ) %>%
    tab_style(
      style = list(cell_text(color = grey30k, weight = "bold")),
      locations = cells_body(columns = agency, rows = agency == "GF")
    ) %>%
    tab_style(
      style = list(cell_text(color = moody_blue, weight = "bold")),
      locations = cells_body(columns = agency, rows = agency == "USAID & CDC")
    )

  tbl_cov %>%
    gtsave(filename = file.path(dir_graphics, "NIGERIA - States & Sites Coverage by Agency.png"))


  # Map of State Coverage ----
  map_cov <- basemap +
    geom_sf(data = spdf_cov,
            aes(fill = color_fill_agency),
            size = .3, color = grey10k) +
    geom_sf_text(data = spdf_cov,
                 aes(label = name, color = color_text_psnu),
                 size = 4) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    scale_fill_identity() +
    scale_color_identity() +
    # labs(title = "COP22 - States Coverage by Agency",
    #      subtitle = glue("<span style='color:{usaid_medblue}'>USAID</span> covers {n_usaid} states, while <span style='color:{usaid_lightblue}'>CDC</span> has {n_cdc}, and <span style='color:{grey30k}'>GF</span> has {n_gf}<br/>Both USAID & CDC cover <span 'color:{moody_blue}'>Lagos</span>")) +
    theme(plot.subtitle = element_markdown())


  map_cov %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of States Coverage by Agency.png"),
      width = 8, height = 7)

  # Map Site Coverage ----

  df_sites_sum %>% glimpse()
  df_sites_sum %>% distinct(fundingagency)
  df_sites_sum %>% distinct(period_type)

  df_sites_agency_locs_map <- df_sites_sum %>%
    filter(fiscal_year == rep_fy) %>%
    distinct(fundingagency, psnu, orgunituid, sitename) %>%
    left_join(df_sites_locs, by = c('orgunituid' = 'id'))

  df_sites_agency_tx_curr_map <- df_sites_sum %>%
    filter(fiscal_year == rep_fy,
           indicator == "TX_CURR",
           period_type == "cumulative") %>%
    left_join(df_sites_locs, by = c('orgunituid' = 'id')) %>%
    mutate(color_fill_tx = case_when(
      fundingagency == "USAID" ~ usaid_medblue,
      fundingagency == "CDC" ~ usaid_lightblue,
      TRUE ~ grey20k
    ))

  # Missing sites ----
  tbl_missing_sites <- df_sites_agency_locs_map %>%
    filter(is.na(latitude)) %>%
    group_by(fundingagency, psnu) %>%
    count() %>%
    ungroup() %>%
    rename(agency = fundingagency) %>%
    arrange(desc(agency), desc(n)) %>%
    gt() %>%
    tab_header(title = "SITES MISSING LAT/LONG") %>%
    opt_all_caps(all_caps = TRUE) %>%
    cols_width(
      psnu ~ px(120),
      everything() ~ px(100)) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = agency, rows = everything())
    ) %>%
    tab_footnote(footnote = "Source: Datim Org. Hierarchy",
                 locations = cells_column_labels(columns = n))


  tbl_missing_sites %>%
    gtsave(filename = file.path(dir_graphics, "NIGERIA - Missing Sites location information.png"))

  # Missing sites - USAID ----
  tbl_missing_sites1 <- df_sites_agency_locs_map %>%
    filter(is.na(latitude)) %>%
    group_by(fundingagency, psnu) %>%
    count() %>%
    ungroup() %>%
    rename(agency = fundingagency) %>%
    arrange(desc(agency), desc(n)) %>%
    filter(agency == "USAID") %>%
    gt() %>%
    tab_header(title = "SITES MISSING LAT/LONG") %>%
    opt_all_caps(all_caps = TRUE) %>%
    cols_hide(agency) %>%
    cols_width(
      psnu ~ px(120),
      everything() ~ px(100)) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = psnu, rows = everything())
    ) %>%
    tab_footnote(footnote = "Source: Datim Org. Hierarchy",
                 locations = cells_column_labels(columns = n))


  tbl_missing_sites1 %>%
    gtsave(filename = file.path(dir_graphics, "NIGERIA - USAID Missing Sites location information.png"))


  # Missing sites - CDC ----
  tbl_missing_sites2 <- df_sites_agency_locs_map %>%
    filter(is.na(latitude)) %>%
    group_by(fundingagency, psnu) %>%
    count() %>%
    ungroup() %>%
    rename(agency = fundingagency) %>%
    arrange(desc(agency), desc(n)) %>%
    filter(agency == "CDC") %>%
    gt() %>%
    tab_header(title = "SITES MISSING LAT/LONG") %>%
    opt_all_caps(all_caps = TRUE) %>%
    cols_hide(agency) %>%
    cols_width(
      psnu ~ px(120),
      everything() ~ px(100)) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = psnu, rows = everything())
    ) %>%
    tab_footnote(footnote = "Source: Datim Org. Hierarchy",
                 locations = cells_column_labels(columns = n))


  tbl_missing_sites2 %>%
    gtsave(filename = file.path(dir_graphics, "NIGERIA - CDC Missing Sites location information.png"))


  # Existing Sites ----
  df_sites_agency_locs_map2 <- df_sites_agency_locs_map %>%
    filter(!is.na(latitude)) %>%
    mutate(color_fill_point = case_when(
      fundingagency == "USAID" ~ grey30k,
      fundingagency == "CDC" ~ grey80k,
      fundingagency == "USAID & CDC" ~ grey80k,
      TRUE ~ NA_character_
    ))

  map_sites_cov <- basemap +
    geom_sf(data = spdf_cov,
            aes(fill = color_fill_agency),
            size = .3, color = grey10k) +
    geom_sf_text(data = spdf_cov,
                 aes(label = name, color = color_text_psnu),
                 size = 3) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_agency_locs_map2,
               aes(x = longitude, y = latitude, fill = color_fill_point),
               shape = 21, size = 2, color = "white", alpha = .8) +
    scale_fill_identity() +
    scale_color_identity() +
    # labs(title = "COP22 - Sites Coverage by Agency") +
    theme(plot.subtitle = element_markdown())


  map_sites_cov %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of Sites Coverage by Agency.png"),
      width = 8, height = 7)


  # Map - TX_CURR ----
  df_sites_agency_tx_curr_map %>%
    filter(value > 0) %>%
    group_by(fundingagency) %>%
    summarise(min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE))

  # Site TX_CURR Distribution
  df_sites_agency_tx_curr_map %>%
    filter(value > 0) %>%
    ggplot(aes(fundingagency, value)) +
    geom_boxplot() +
    geom_quasirandom()

  plot_tx_curr <- df_sites_agency_tx_curr_map %>%
    mutate(group = cut(value,
                       breaks = c(0, 1000, 5000, 10000, 15000),
                       labels = c("low", "high-low", "high-mid", "high"))) %>%
    filter(value > 0) %>%
    ggplot(aes(fundingagency, value)) +
    geom_hline(yintercept = 0, size = 1, color = grey60k) +
    geom_hline(yintercept = 1000, size = 1.2, lty = "dotted", color = usaid_black) +
    ggdist::stat_halfeye(
      aes(fill = fundingagency),
      color = grey40k,
      adjust = .5,
      width = .4,
      justification = -.2,
      .width = 0,
      point_color = NA,
      show.legend = F
    ) +
    geom_boxplot(width = .1, color = trolley_grey,
                 outlier.color = usaid_red, outlier.size = 1) +
    ggdist::stat_dots(side = "left",
                      justification = 1.1) +
    scale_y_continuous(position = "right",
                       #breaks = seq(0, 20000, 2500)) +
                       breaks = c(0, 2500, 5000, 10000, 15000, 20000)) +
    scale_fill_manual(values = c(usaid_lightblue, usaid_medblue),
                      #breaks = seq(0, 20000, 2500),
                      position = "right") +
    #facet_wrap(~group, ncol = 1, scales = "free")
    coord_flip() +
    labs(x = "", y = "") +
    si_style_xgrid()

  plot_tx_curr %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Plot TX_CURR Volume by Site.png"),
      width = 10, height = 5)

  tbl_tx_curr_big_sites <- df_sites_agency_tx_curr_map %>%
    filter(value >= 5000) %>%
    select(fundingagency, psnu, sitename, value) %>%
    arrange(desc(fundingagency), desc(value)) %>%
    filter(fundingagency == "USAID") %>%
    gt() %>%
    tab_header(title = "USAID - HIGH VOLUME SITES (5K+)") %>%
    opt_all_caps(all_caps = TRUE) %>%
    cols_hide(fundingagency) %>%
    cols_label(value = "TX_CURR") %>%
    cols_width(psnu ~ px(120)) %>%
    fmt_integer(columns = value) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = psnu, rows = everything())
    ) %>%
    tab_footnote(footnote = "Source: FY21Q4 MSD",
                 locations = cells_column_labels(columns = value))

  tbl_tx_curr_big_sites %>%
    gtsave(filename = file.path(dir_graphics, "NIGERIA - Plot TX_CURR High Volume Sites.png"))

  # Map Site TX_CURR ----
  map_sites_tx_curr <- basemap +
    geom_sf(data = spdf_cov, fill = NA,
            size = .8, color = grey20k) +
    geom_sf_text(data = spdf_cov,
                 aes(label = name),
                 color = grey80k, size = 3) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_agency_tx_curr_map %>% filter(value > 0),
               aes(x = longitude, y = latitude,
                   fill = color_fill_tx, size = value),
               shape = 21, color = grey20k, alpha = .8) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size(range = c(3, 20), breaks = c(1000, 5000, 10000, 15000, 20000)) +
    # labs(title = "COP22 - TX_CURR Volume by Agency and Sites") +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal")

  map_sites_tx_curr %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of TX_CURR Volume by Agency and Sites.png"),
      width = 8, height = 7)

  # Map HEX TX_CURR ----
  hex30km <- spdf_hex30km %>% st_transform(crs = st_crs(3857))
  hex50km <- spdf_hex50km %>% st_transform(crs = st_crs(3857))

  spdf_sites_tx_curr <- df_sites_agency_tx_curr_map %>%
    filter(value > 0, !is.na(latitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs(4326)) %>%
    st_transform(crs = st_crs(3857)) %>%
    st_join(hex30km, join = st_intersects) %>%
    rename(id_30km = id) %>%
    st_join(hex50km, join = st_intersects) %>%
    rename(id_50km = id) %>%
    st_drop_geometry()

  spdf_sites_tx_curr %>% glimpse()

  spdf_hex30km_sites_tx_curr <- hex30km %>%
    left_join(spdf_sites_tx_curr,
              by = c("id" = "id_30km")) %>% #glimpse()
    group_by(id, fundingagency) %>%
    summarise(across(value, sum, na.rm = T), .groups = "drop")

  spdf_hex50km_sites_tx_curr <- hex50km %>%
    left_join(spdf_sites_tx_curr,
              by = c("id" = "id_50km")) %>%
    group_by(id, fundingagency) %>%
    summarise(across(value, sum, na.rm = T), .groups = "drop")

  map_hex30km_tx_curr <- spdf_hex30km_sites_tx_curr %>%
    filter(value > 0) %>%
    ggplot() +
    geom_sf(aes(fill = value), size = .2, color = grey10k) +
    geom_sf(data = spdf_hex30km, fill = NA, size = .2, color = grey10k) +
    geom_sf(data = spdf_adm1,
            fill = NA, size = .3, color = grey30k) +
    geom_sf_text(data = spdf_adm1,
                 aes(label = name),
                 color = usaid_black,
                 size = 2.5) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    scale_fill_si(palette = "genoas",
                  breaks = c(0, 1000, seq(5000, max(spdf_hex30km_sites_tx_curr$value), 10000))) +
    facet_wrap(~fundingagency, ncol = 2) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(100, "pt"),
          legend.key.height = unit(10, "pt"),
          strip.text = element_text(hjust = .5))

  map_hex30km_tx_curr %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of TX_CURR Volume by Agency and HEX30KM.png"),
      width = 10, height = 5)


  map_hex50km_tx_curr <- spdf_hex50km_sites_tx_curr %>%
    filter(value > 0) %>%
    ggplot() +
    geom_sf(aes(fill = value), size = .2, color = grey10k) +
    geom_sf(data = spdf_hex50km, fill = NA, size = .2, color = grey10k) +
    geom_sf(data = spdf_adm1,
            fill = NA, size = .3, color = grey30k) +
    geom_sf_text(data = spdf_adm1,
                 aes(label = name),
                 color = usaid_black,
                 size = 2.5) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    scale_fill_si(palette = "genoas",
                  breaks = c(0, 1000, seq(5000, max(spdf_hex50km_sites_tx_curr$value), 10000))) +
    facet_wrap(~fundingagency, ncol = 2) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(100, "pt"),
          legend.key.height = unit(10, "pt"),
          strip.text = element_text(hjust = .5)
    )

  map_hex50km_tx_curr %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of TX_CURR Volume by Agency and HEX50KM.png"),
      width = 10, height = 5)




  # Facet map tx_curr by agency ----
  map_sites_tx_curr2 <- basemap +
    geom_sf(data = spdf_cov %>% select(-fundingagency),
            fill = NA,
            size = .8, color = grey20k) +
    geom_sf_text(data = spdf_cov %>% select(-fundingagency),
                 aes(label = name),
                 color = grey80k, size = 3) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_agency_tx_curr_map %>% filter(value > 0),
               aes(x = longitude, y = latitude,
                   fill = color_fill_tx, size = value),
               shape = 21, color = grey10k, alpha = .8) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size(range = c(3, 20), breaks = c(1000, 5000, 10000, 15000, 20000)) +
    facet_wrap(~fundingagency, ncol = 2) +
    # labs(title = "COP22 - TX_CURR Volume by Agency and Sites") +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal")

  map_sites_tx_curr2 %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of TX_CURR Volume by Agency and Sites2.png"),
      width = 10, height = 5)


  # MMD 3+
  df_sites_tx_mmd_35 <- df_sites_tx_mmd %>%
    filter(period == "FY21Q4",
           otherdisaggregate == "3-5",
           !is.na(latitude)) %>%
    mutate(color_fill_tx = case_when(
      fundingagency == "USAID" ~ usaid_medblue,
      fundingagency == "CDC" ~ usaid_lightblue,
      TRUE ~ grey20k
    ))

  df_sites_tx_mmd_3plus <- df_sites_tx_mmd %>%
    filter(period == "FY21Q4",
           otherdisaggregate == "3+",
           !is.na(latitude)) %>%
    mutate(color_fill_tx = case_when(
      fundingagency == "USAID" ~ usaid_medblue,
      fundingagency == "CDC" ~ usaid_lightblue,
      TRUE ~ grey20k
    ))

  df_sites_tx_mmd_6plus <- df_sites_tx_mmd %>%
    filter(period == "FY21Q4",
           otherdisaggregate %in% c("6+"),
           !is.na(latitude)) %>%
    mutate(color_fill_tx = case_when(
      fundingagency == "USAID" ~ usaid_medblue,
      fundingagency == "CDC" ~ usaid_lightblue,
      TRUE ~ grey20k
    ))

  df_sites_tx_mmd_all <- df_sites_tx_mmd %>%
    filter(period == "FY21Q4",
           !is.na(latitude)) %>%
    mutate(color_fill_tx = case_when(
      fundingagency == "USAID" ~ usaid_medblue,
      fundingagency == "CDC" ~ usaid_lightblue,
      TRUE ~ grey20k
    ))

  map_sites_mmd35 <- basemap +
    geom_sf(data = spdf_cov %>% select(-fundingagency),
            fill = NA,
            size = .8, color = grey20k) +
    geom_sf_text(data = spdf_cov %>% select(-fundingagency),
                 aes(label = name),
                 color = grey80k, size = 3) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_tx_mmd_35 %>% filter(value > 0),
               aes(x = longitude, y = latitude,
                   fill = color_fill_tx, size = value),
               shape = 21, color = grey10k, alpha = .8) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size(range = c(3, 20),
               breaks = c(0, 1000,
                          seq(5000, max(df_sites_tx_mmd_35$value), 5000))) +
    facet_wrap(~fundingagency, ncol = 2) +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal")


  map_sites_mmd3plus <-  basemap +
    geom_sf(data = spdf_cov %>% select(-fundingagency),
            fill = NA,
            size = .8, color = grey20k) +
    geom_sf_text(data = spdf_cov %>% select(-fundingagency),
                 aes(label = name),
                 color = grey80k, size = 3) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_tx_mmd_3plus %>% filter(value > 0),
               aes(x = longitude, y = latitude,
                   fill = color_fill_tx, size = value),
               shape = 21, color = grey10k, alpha = .8) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size(range = c(3, 20),
               breaks = c(0, 1000,
                          seq(5000, max(df_sites_tx_mmd_3plus$value), 5000))) +
    facet_wrap(~fundingagency, ncol = 2) +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal")


  map_sites_mmd6plus <- basemap +
    geom_sf(data = spdf_cov %>% select(-fundingagency),
            fill = NA,
            size = .8, color = grey20k) +
    geom_sf_text(data = spdf_cov %>% select(-fundingagency),
                 aes(label = name),
                 color = grey80k, size = 3) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_tx_mmd_6plus %>% filter(value > 0),
               aes(x = longitude, y = latitude,
                   fill = color_fill_tx, size = value),
               shape = 21, color = grey10k, alpha = .8) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size(range = c(3, 20),
               breaks = c(0, 1000,
                          seq(5000, max(df_sites_tx_mmd_6plus$value), 5000))) +
    facet_wrap(~fundingagency, ncol = 2) +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal")

  map_sites_mmd_all <- ggplot() +
    geom_sf(data = spdf_cov %>% select(-fundingagency),
            fill = NA,
            size = .1, color = grey20k) +
    geom_sf_text(data = spdf_cov %>% select(-fundingagency),
                 aes(label = name),
                 color = grey80k, size = 1) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_tx_mmd_all %>% filter(value > 0),
               aes(x = longitude, y = latitude,
                   #fill = color_fill_tx,
                   size = value),
               fill = scooter,
               shape = 21, stroke = .3, color = grey20k, alpha = .6) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size(range = c(1, 8),
               breaks = c(0, 1000,
                          seq(5000, max(df_sites_tx_mmd_all$value), 5000))) +
    facet_wrap(fundingagency ~ otherdisaggregate, ncol = 4) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.position = "right",
          legend.direction = "vertical")

  map_sites_mmd_all %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of USAID Sites TX MMD Volume All.png"),
      width = 10, height = 6)

  map_sites_mmd_all2 <- ggplot() +
    geom_sf(data = spdf_cov %>% select(-fundingagency),
            fill = NA,
            size = .1, color = grey20k) +
    geom_sf_text(data = spdf_cov %>% select(-fundingagency),
                 aes(label = name),
                 color = grey80k, size = 1) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_tx_mmd_all %>% filter(value > 0),
               aes(x = longitude, y = latitude, size = share),
               fill = scooter,
               shape = 21, stroke = .3,
               color = grey10k, alpha = .6) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_size(range = c(1, 5), breaks = c(0, seq(0, 1, .25)), labels = percent) +
    facet_wrap(fundingagency ~ otherdisaggregate, ncol = 4) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(plot.subtitle = element_markdown(),
          legend.title = element_blank(),
          legend.position = "right",
          legend.direction = "vertical")

  map_sites_mmd_all2 %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of USAID Sites TX MMD Share All.png"),
      width = 10, height = 6)

  # USAID ONLY ----
  map_sites_cov1 <- basemap +
    geom_sf(data = spdf_cov %>% filter(fundingagency == "USAID"),
            aes(fill = color_fill_agency),
            size = .3, color = grey10k) +
    geom_sf_text(data = spdf_cov,
                 aes(label = name, color = color_text_psnu),
                 size = 3) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_agency_locs_map2 %>% filter(fundingagency == "USAID"),
               aes(x = longitude, y = latitude, fill = color_fill_point),
               shape = 21, size = 3, color = "white", alpha = .8) +
    scale_fill_identity() +
    scale_color_identity() +
    # labs(title = "COP22 - Sites Coverage by Agency") +
    theme(plot.subtitle = element_markdown())


  map_sites_cov1 %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of USAID Sites Coverage.png"),
      width = 8, height = 7)


  # CDC ONLY ----
  map_sites_cov2 <- basemap +
    geom_sf(data = spdf_cov %>% filter(fundingagency == "CDC"),
            aes(fill = color_fill_agency),
            size = .3, color = grey10k) +
    geom_sf_text(data = spdf_cov,
                 aes(label = name, color = color_text_psnu),
                 size = 3) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_point(data = df_sites_agency_locs_map2 %>% filter(fundingagency == "CDC"),
               aes(x = longitude, y = latitude, fill = color_fill_point),
               shape = 21, size = 3, color = "white", alpha = .8) +
    scale_fill_identity() +
    scale_color_identity() +
    # labs(title = "COP22 - Sites Coverage by Agency") +
    theme(plot.subtitle = element_markdown())


  map_sites_cov2 %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - Map of CDC Sites Coverage.png"),
      width = 8, height = 7)



  # COUNTRY Data ----
  # COUNTRY Data - TX_CURR Category
  plot_heatmap_tx_curr_vol <- df_psnu_tx_curr_vol %>%
    mutate(
      category = factor(category,
                        levels = c("large", "medium", "low", "very low"),
                        labels = c("large", "medium", "small", "very small"),
                        ordered = TRUE),
      lbl_color = case_when(
      share > .299 ~ grey10k,
      TRUE ~ grey80k
    )) %>%
    ggplot(aes(x = category, y = reorder(state, desc(value)), fill = share)) +
    geom_tile(color = grey10k) +
    geom_text(aes(label = comma(value, 1),
                  #color = lbl_color
                  ), size = 3,
              color = grey10k) +
    # scale_fill_si(palette = "genoas", na.value = grey20k,
    #               breaks = c(0,.25, .5, .75, 1), labels = percent, position = "top") +
    scale_fill_stepsn(colors = c("#72c3b4", "#459688", "#2d8073", "#01564b", "#002e24", "#001b0e"),
                      breaks = seq(0, 1, .25),
                      guide = guide_colorsteps(show.limits = F, even.steps = F),
                      na.value = grey40k,
                      limits = c(0, 1),
                      labels = percent,
                      oob = scales::oob_squish,
                      values = scales::rescale(seq(0, 1, by = 0.25), c(0, 1))
                      ) +
    scale_color_identity() +
    scale_x_discrete(position = "top") +
    facet_wrap(~agency, scales = "free") +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(100, "pt"),
          legend.key.height = unit(10, "pt"),
          strip.placement = "outside",
          strip.text = element_text(face = "bold", hjust = .02))


  plot_heatmap_tx_curr_vol %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - TX_CURR Volume Categorisation.png"),
      width = 10, height = 6)


  # COUNTRY Data - TX_CURR Sites Category
  plot_heatmap_tx_curr_sites <- df_psnu_tx_curr_vol %>%
    mutate(
      category = factor(category,
                        levels = c("large", "medium", "low", "very low"),
                        labels = c("large", "medium", "small", "very small"),
                        ordered = TRUE),
      lbl_color = case_when(
        share > .75 ~ grey10k,
        TRUE ~ grey80k
      )) %>%
    ggplot(aes(x = category, y = reorder(state, desc(value)), fill = sites_share)) +
    geom_tile(color = grey10k) +
    geom_text(aes(label = comma(value, 1)), size = 3, color = grey10k) +
    # scale_fill_si(palette = "genoas", na.value = grey20k,
    #               breaks = c(0,.25, .5, .75, 1), labels = percent, position = "top") +
    scale_fill_stepsn(colors = c("#72c3b4", "#459688", "#2d8073", "#01564b", "#002e24", "#001b0e"),
                      breaks = seq(0, 1, .25),
                      guide = guide_colorsteps(show.limits = F, even.steps = F),
                      na.value = grey40k,
                      limits = c(0, 1),
                      labels = percent,
                      oob = scales::oob_squish,
                      values = scales::rescale(seq(0, 1, by = 0.25), c(0, 1))) +
    scale_color_identity() +
    scale_x_discrete(position = "top") +
    facet_wrap(~agency, scales = "free") +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(100, "pt"),
          legend.key.height = unit(10, "pt"),
          strip.placement = "outside",
          strip.text = element_text(face = "bold", hjust = .02))


  plot_heatmap_tx_curr_sites %>%
    si_save(
      filename = file.path(dir_graphics, "NIGERIA - TX_CURR Sites Categorisation.png"),
      width = 10, height = 6)



  # OTHER Old Scripts ----
  # PSNU locations ----
  spdf_adm1_prio <- spdf_adm1 %>%
    left_join(df_cov, by = c("uid" = "psnuuid")) %>%
    st_transform(crs = st_crs(3857)) %>%
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
        labs(x = "", y = "",
             title = "This is a title") +
        #theme(plot.title = element_text(family = "Arial"))
        si_style_map()

      print(m)

      return(m)
    })
