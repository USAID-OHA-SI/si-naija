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
  library(scales)
  library(glue)
  library(ggtext)
  library(cowplot)
  library(extrafont)

  source("Scripts/N00_Utilities.R")

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

  # MSD - Nat SubNat ----

  df_subnats_raw <- file_nat_subnat %>% read_msd()

  # Extract Country historical ranking for SCA
  df_hiv_burden <- df_subnats_raw %>%
    filter(indicator %in% c("PLHIV", "POP_EST"),
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, countryname, indicator) %>%
    summarise(value = sum(targets, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    arrange(desc(fiscal_year), desc(PLHIV), countryname) %>%
    mutate(Prevalence = round(PLHIV / POP_EST * 100, 2))

  # df_hiv_burden %>%
  #   write_csv(file = file.path(dir_dataout, "Nigeria - PLHIV Historical data.csv"), na = "")

  # Explore POP Disaggs
  df_subnats_raw %>%
    filter(countryname == cntry,
           indicator %in% inds[1:2]) %>%
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
    mutate(Prevalence = round(PLHIV / POP_EST * 100, 2)) %>%
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

  # IP Coverage table
  df_ip <- df_sites %>%
    filter(fiscal_year == rep_fy, primepartner != "TBD") %>%
    distinct(fundingagency, psnu, primepartner)

  df_ip_usaid <- df_ip %>%
    filter(fundingagency == "USAID",
           psnu != "_Military Nigeria") %>%
    group_by(primepartner) %>%
    summarise(states = paste(psnu, collapse = ", ")) %>%
    ungroup() %>%
    rename(`Implementing Partner` = primepartner)

  # Agency Sites Coverage
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

  # Site Summaries
  df_sites_sum <- df_sites %>%
    clean_indicator() %>%
    filter(sitename != "Data reported above Site level",
           indicator %in% inds[3:length(inds)],
           standardizeddisaggregate %in% ttl_disaggs) %>%
    group_by(fiscal_year, fundingagency, psnuuid, psnu,
             communityuid, community, orgunituid, sitename, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative), sum, na.rm = T), .groups = "drop") %>%
    reshape_msd() %>%
    mutate(fiscal_year = as.integer(paste0("20", str_sub(period, 3, 4)))) %>%
    relocate(fiscal_year, .before = 1)

  # Above Sites Summaries - Results / Targets
  df_psnu_sum <- df_sites %>%
    clean_indicator() %>%
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

  # Results by Agency
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
      `Implementing Partner` ~ px(250),
      everything() ~ px(300)) %>%
  gtsave(filename = file.path(dir_graphics, "NIGERIA- USIAD Partners Coverage.png"))

  # Country Basemap
  basemap <- terrain_map(countries = cntry,
                         adm0 = spdf_adm0,
                         adm1 = spdf_adm1,
                         terr = terr,
                         mask = TRUE)

  map_cntry <- basemap +
    geom_sf_text(data = spdf_adm1,
            aes(label = name),
            size = 4, color = grey80k)

  # Agency Coverage
  df_agency_cov_colors <- df_agency_cov %>%
    mutate(
      color_fill_agency = case_when(
        fundingagency == "USAID" ~ usaid_medblue,
        fundingagency == "CDC" ~ usaid_lightblue,
        fundingagency == "USAID & CDC" ~ moody_blue,
        TRUE ~ grey30k
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

  # Agency Areas summary
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

  # Table Coverage
  tbl_cov <- n_agencies %>%
    gt() %>%
    cols_label(area = "area (KM^2)",
               area_share = "% of Country",
               sites_per_km = "# of S. / 1k KM^2",
               patients_per_km = "# P. / KM^2") %>%
    opt_all_caps( all_caps = TRUE) %>%
    cols_width(
      agency ~ px(120),
      states ~ px(50),
      everything() ~ px(100)) %>%
    fmt_number(columns = c(area, sites, art, patients_per_km), decimals = 0) %>%
    fmt_number(columns = c(sites_per_km), decimals = 4) %>%
    fmt_percent(columns = area_share, decimals = 1) %>%
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

    #gtsave(filename = file.path(dir_graphics, "NIGERIA- USIAD Partners Coverage.png"))


  # Map of coverage
  map_cov <- basemap +
    geom_sf(data = spdf_cov,
            aes(fill = color_fill_agency),
            size = .2, color = grey10k) +
    geom_sf(data = spdf_adm0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_adm0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    scale_fill_identity() +
    labs(title = "COP22 - States Coverage by Agency",
         subtitle = glue("<span style='color:{usaid_medblue}'>USAID</span> covers {n_usaid} states, while <span style='color:{usaid_lightblue}'>CDC</span> has {n_cdc}, and <span style='color:{grey30k}'>GF</span> has {n_gf}<br/>Both USAID & CDC cover <span 'color:{moody_blue}'>Lagos</span>")) +
    theme(plot.subtitle = element_markdown())








  # PSNU locations ----
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
        labs(x = "", y = "",
             title = "This is a title") +
        #theme(plot.title = element_text(family = "Arial"))
        si_style_map()

      print(m)

      return(m)
    })
