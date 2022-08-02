##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: COP22 / FY23 Targets Allocation
##  LICENCE: MIT
##  DATE:    2022-08-01

## Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glamr)
  library(gisr)
  library(sf)
  library(janitor)
  library(glue)
  library(gt)
  library(ggtext)
  library(patchwork)
  library(cowplot)

  library(fontawesome)
  library(emojifont)
  library(extrafont)

  #font_import()

  source("./Scripts/N00_Utilities.R")
  source("./Scripts/N00_Viz_Utilities.R")

## GLOBALS ----

# Dirs ----

  dir_merdata <- si_path("path_msd")
  dir_geodata <- si_path("path_vector")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_images <- "Images"

  dir_merdata %>% open_path()

# Files ----

  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY20.*_N")

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*_N")

  file_subnat <- dir_merdata %>%
    return_latest(pattern = "NAT_SUBNAT")

  file_shp <- dir_geodata %>%
    return_latest(pattern = "VcPepfarPolygons")

# Params ----

  cntry <- "Nigeria"

  cntry_uid <- get_ouuid(cntry)

  fac_level <- get_ouorglevel(operatingunit = cntry, org_type = "facility")

  agency <- "USAID"

  curr_pd <- file_psnu_im %>% identify_pd()

  curr_fy <- curr_pd %>%
    str_sub(1, 4) %>%
    str_replace("FY", "20") %>%
    as.numeric()

  prev_fy <- curr_fy - 1

# FUNCTIONS ----

# DATA ----

  # Geodata ----

  ras <- get_raster()

  spdf_pepfar <- file_shp %>% read_sf()

  df_attr <- get_attributes(country = cntry)

  spdf_nga <- spdf_pepfar %>%
    dplyr::left_join(df_attr, by = c("uid" = "id"))

  # Country sub-units
  spdf_cntry <- spdf_nga %>% filter(label == "country")

  spdf_psnu <- spdf_nga %>% filter(label == "prioritization")

  spdf_psnu_edo <- spdf_psnu %>%
    filter(name == "Edo") %>%
    st_transform(crs = st_crs(3857))

  spdf_lga <- spdf_nga %>%
    filter(label == "community") %>%
    clean_column(colname = "name")

  spdf_lga_edo <- spdf_lga %>%
    st_transform(crs = st_crs(3857)) %>%
    st_centroid() %>%
    select(uid) %>%
    st_filter(spdf_psnu_edo, join = st_within) %>%
    st_drop_geometry() %>%
    mutate(psnu = "Edo") %>%
    left_join(spdf_lga, ., by = "uid") %>%
    filter(!is.na(psnu))

  # Site Coordinates
  df_site_locs <- extract_locations(country = cntry,
                                    level = fac_level,
                                    add_geom = TRUE) %>%
    extract_facilities() %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    select(uid = id, name, latitude, longitude)



  # NAT x SUBNAT ----
  df_nat <- file_subnat %>% read_msd()

  df_nat %>% glimpse()
  df_nat %>% distinct(fiscal_year)
  df_nat %>% distinct(indicator) %>% prinf()

  df_nat_pop <- df_nat %>%
    filter(country == cntry,
           fiscal_year == curr_fy -2 & indicator == "PLHIV" |
           fiscal_year == curr_fy & indicator == "POP_EST",
           standardizeddisaggregate == "Age/Sex")

  df_nat_pop <- df_nat_pop %>%
    group_by(psnuuid, psnu, indicator,
             standardizeddisaggregate,
             trendscoarse, sex) %>%
    summarise(across(targets, sum, na.rm = T))

  # MSD Site x IM ----

  df_sites <- file_site_im %>% read_msd()

  # MSD Site x IM
  df_sites %>% distinct(funding_agency)

  df_msd_sites <- df_sites %>%
    filter(fiscal_year == curr_fy,
           funding_agency != "Dedup")

  df_msd_sites <- df_msd_sites %>%
    filter(indicator %in% inds_pmtct) %>%
    clean_indicator()

  inds_pmtct <- df_msd_sites %>%
    filter(str_detect(indicator, "PMTCT_.*")) %>%
    distinct(indicator) %>%
    pull(indicator)

  inds_t_nd <- df_msd_sites %>%
    filter(str_detect(indicator, "PMTCT_.*"),
           str_detect(standardizeddisaggregate, "^Total")) %>%
    distinct(standardizeddisaggregate) %>%
    pull(standardizeddisaggregate)

  inds_disaggs <- df_msd_sites %>%
    filter(str_detect(indicator, "PMTCT_.*"),
           str_detect(standardizeddisaggregate, "^Total", negate = T)) %>%
    distinct(standardizeddisaggregate) %>%
    pull(standardizeddisaggregate)

  # MSD Site x IM - PMTCT Facilities

  df_pmtct_sites <- df_msd_sites %>%
    filter(is.na(targets)) %>%
    select(one_of(c("funding_agency", str_msd_sites$org_units))) %>%
    distinct() %>%
    clean_agency() %>%
    clean_column(colname = "community")

  df_pmtct_sites <- df_pmtct_sites %>%
    left_join(df_site_locs,
              by = c("orgunituid" = "uid", "sitename" = "name"))

  df_pmtct_sites %>%
    filter(is.na(latitude)) %>%
    group_by(psnu) %>%
    summarise(n = n_distinct(orgunituid), .groups = "drop")

# MUNGE ----

  # PMTCT Sites ----

  spdf_pmtct_sites <- df_pmtct_sites %>%
    filter(!is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))

  spdf_pmtct_sites <- spdf_cntry %>%
    st_make_valid() %>%
    st_filter(spdf_pmtct_sites, ., .predicate = st_intersects)

  # PMTCT - STAT @ Community ----

  df_msd_sites %>% distinct(indicator) %>% prinf()

  df_com_stat <- df_msd_sites %>%
    filter(psnu == "Edo",
           str_detect(indicator, "_STAT|T_ART"),
           standardizeddisaggregate %in% inds_t_nd,
           !is.na(cumulative)) %>%
    group_by(psnu, psnuuid, community, communityuid, indicator) %>%
    summarise(results = sum(cumulative, na.rm = T), .groups = "drop") %>%
    mutate(indicator = str_remove(indicator, "PMTCT_"))

  df_com_stat %>% distinct(indicator) %>% prinf()

  df_com_stat <- df_com_stat %>%
    mutate(indicator = factor(
      indicator,
      levels = c("STAT_D", "STAT", "STAT_POS", "ART_D", "ART"),
      ordered = TRUE)) %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = results) %>%
    clean_names() %>%
    rowwise() %>%
    mutate(stat_nn = stat_d - stat,                     # Non tested
           stat_nn = ifelse(stat_nn < 0, 0, stat_nn),
           stat_cov = stat / stat_d,
           stat_cov_breaks = case_when(
             stat_cov < .5 ~ 1,
             stat_cov >= .5 & stat_cov <.75 ~ 2,
             stat_cov >= .75 & stat_cov <.95 ~ 3,
             stat_cov > .95 ~ 4,
             TRUE ~ 99
           ),
           stat_yield = stat_pos / stat,
           stat_na = stat_pos - art,                    # Non linked
           stat_na = ifelse(stat_na < 0, 0, stat_na),
           art_cov = art / stat_pos) %>%
    ungroup() %>%
    clean_column(colname = "community")

  spdf_com_stat <- df_com_stat %>%
    pivot_longer(cols = starts_with(c("stat", "art")),
                 names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(
      metric,
      levels = c("stat_d", "stat", "stat_nn", "stat_cov", "stat_cov_breaks",
                 "stat_pos", "stat_yield", "stat_na", "art_d", "art", "art_cov"))) %>%
    left_join(x = spdf_lga_edo, y = ., by = c("uid" = "communityuid")) %>%
    filter(!is.na(value))

  # PMTCT - STAT @ PSNU ----

  df_psnu_stat <- df_msd_sites %>%
    filter(psnu == "Edo",
           str_detect(indicator, "_STAT|_STAT_POS|T_ART"),
           standardizeddisaggregate %in% inds_t_nd,
           !is.na(cumulative)) %>%
    group_by(psnu, psnuuid, indicator) %>%
    summarise(results = sum(cumulative, na.rm = T), .groups = "drop") %>%
    mutate(indicator = str_remove(indicator, "PMTCT_"))

  df_psnu_stat <- df_psnu_stat %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = results) %>%
    clean_names() %>%
    rowwise() %>%
    mutate(stat_nn = stat_d - stat,                    # Non tested
           stat_nn = ifelse(stat_nn < 0, 0, stat_nn),
           stat_cov = stat / stat_d,
           stat_np = stat - stat_pos,
           stat_yield = stat_pos / stat,
           stat_na = stat_pos - art,                   # Non linked
           stat_na = ifelse(stat_na < 0, 0, stat_na),
           art_cov = art / stat_pos) %>%
    ungroup()

  df_psnu_stat <- df_psnu_stat %>%
    pivot_longer(cols = -c(psnu, psnuuid),
                 names_to = "metric",
                 values_to = "value") %>% #distinct(metric)
    mutate(metric = factor(metric,
                           levels = c("stat_d", "stat",
                                      "stat_nn", "stat_cov",
                                      "stat_pos", "stat_np",
                                      "stat_yield", "stat_na",
                                      "art", "art_d", "art_cov"),
                           ordered = T)) %>%
    filter(!is.na(value))

  # PSNU Summary
  psnu_pmtct <- df_psnu_stat %>%
    group_by(metric) %>%
    summarise(across(value, sum, na.rm = T)) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    mutate(stat_nn = stat_d - stat,                    # Non tested
           stat_cov = stat / stat_d,
           stat_np = stat - stat_pos,
           stat_yield = stat_pos / stat,
           stat_na = stat_pos - art,                   # Non linked
           art_cov = art / stat_pos)


# VIZ ----

  # Maps ----

  # Map - Basemap
  basemap <- terrain_map(countries = cntry,
                         adm0 = spdf_cntry,
                         adm1 = spdf_psnu,
                         mask = T)

  basemap_edo <- terrain_map(#countries = cntry,
    countries = st_transform(spdf_psnu_edo, crs = st_crs(4326)),
    #adm0 = spdf_psnu_edo,
    adm0 = st_transform(spdf_psnu_edo, crs = st_crs(4326)),
    #adm1 = spdf_lga_edo,
    adm1 = st_transform(spdf_lga_edo, crs = st_crs(4326)),
    mask = T)

  # Map - Edo State Locator

  map_aoimap_aoi <- basemap +
    geom_sf(data = spdf_cntry, size = 1.5, fill = NA, color = grey10k) +
    geom_sf(data = st_transform(spdf_psnu_edo, crs = st_crs(4326)), size = 1, fill = NA, color = usaid_red) +
    geom_sf(data = filter(spdf_pmtct_sites, psnu != "Edo"),
            shape = 21, size = 3, fill = grey50k,
            color = grey10k, alpha = .6, show.legend = F) +
    geom_sf(data = filter(spdf_pmtct_sites, funding_agency == agency, psnu == "Edo"),
            aes(fill = funding_agency),
            shape = 21, size = 5, color = grey10k, show.legend = F) +
    geom_sf_text(data = filter(spdf_psnu, name != "Edo"), aes(label = name),
                 size = 4, color = usaid_black) +
    geom_sf_text(data = filter(spdf_psnu, name == "Edo"), aes(label = name),
                 size = 5, fontface = "bold", color = usaid_red) +
    theme_transparent()

  map_aoimap_aoi

  map_aoi <- ggplot() +
    geom_sf(data = spdf_psnu_edo, size = 2, fill = NA, color = grey10k) +
    geom_sf(data = spdf_lga_edo,
            size = 1, alpha = .7, fill = NA, color = grey30k) +
    geom_sf(data = spdf_psnu_edo, size = .5, fill = NA, color = grey90k) +
    geom_sf(data = filter(spdf_pmtct_sites, funding_agency == agency, psnu == "Edo"),
            aes(fill = funding_agency),
            shape = 21, size = 5, color = grey10k, show.legend = F) +
    geom_sf_text(data = spdf_lga_edo, aes(label = name),
                 size = 3, fontface = "bold", color = usaid_black) +
    # geom_sf_text(data = filter(spdf_pmtct_sites, funding_agency == agency, psnu == "Edo"),
    #              aes(label = sitename), size = 4, color = usaid_black) +
    labs(x = "", y = "") +
    si_style_map() +
    theme_transparent()

  map_aoi

  ggdraw(map_aoimap_aoi) +
    draw_plot(plot = map_aoi,
              x = .6, y = -.25,
              width = .25)

  # Plots ----

  # Edo State - PMTCT Sites ----
  df_msd_sites %>%
    filter(psnu == "Edo",
           str_detect(community, "Data reported above", negate = TRUE)) %>%
    distinct() %>%
    group_by(psnu, community) %>%
    summarise(sites = n_distinct(orgunituid), .groups = "drop") %>%
    clean_column(colname = 'community') %>%
    arrange(desc(sites), community) %>%
    rename_with(str_to_upper) %>%
    gt() %>%
    tab_header(
      title = "PMTCT FACILITIES",
      subtitle = md("As of **FY22 Q2**, Edo State had **26** Facilities in **13** Communities")) %>%
    gtsave(filename = file.path(
      dir_graphics,
      "Nigeria - Edo State - PMTCT Facilities table.png"))

  df_msd_sites %>%
    filter(psnu == "Edo",
           str_detect(community, "Data reported above", negate = TRUE)) %>%
    group_by(psnu, community) %>%
    summarise(sites = n_distinct(orgunituid), .groups = "drop") %>%
    clean_column(colname = 'community') %>%
    arrange(desc(sites), community) %>%
    ggplot(aes(reorder(community, sites), sites)) +
    geom_col(fill = old_rose_light) +
    geom_hline(yintercept = 1:4, color = "white") +
    scale_y_continuous(breaks = seq(1, 4, 1), position = "right") +
    coord_flip() +
    labs(x = "", y = "") +
    si_style_nolines()

  # Stat ----
  stat_dd <- 278519

  plot_psnu_stat_cov <- psnu_pmtct %>%
    ggplot() +
    geom_col(aes(x = 0, y = 1), width = 10, fill = trolley_grey_light) +
    geom_col(aes(x = 0, y = stat_cov), width = 10, fill = scooter_med) +
    geom_col(aes(x = 0, y = 1), width = 10, fill = NA, size = 1.5, color = scooter) +
    geom_hline(aes(yintercept = stat_cov),
               size = 1.5, lty = "dashed", color = scooter) +
    geom_richtext(aes(x = 2.5, y = .1),
                  label = paste0("<img src='", get_logo("female"), "' width='100' /> "),
                  fill = NA, color = NA) +
    geom_text(aes(x = 2.5, y = .2),
              label = paste0(percent(ou_pmtct$stat_cov, 1)),
              color = grey10k,
              fontface = "bold",
              size = 80,
              hjust = 0) +
    geom_text(aes(x = 0, y = .2),
              label = paste0("of women seen at ANC1 were tested for HIV\n[",
                             comma(ou_pmtct$stat), " out of ",
                             comma(ou_pmtct$stat_d), "]"),
              color = grey10k,
              fontface = "bold",
              size = 10,
              hjust = 0) +
    # geom_text(aes(x = 2.5, y = .3),
    #           label = paste0(percent(ou_pmtct$stat / stat_dd, 1)),
    #           color = grey10k,
    #           fontface = "bold",
    #           size = 80,
    #           hjust = 0) +
    geom_text(aes(x = -2, y = .2),
              label = paste0("Only ", percent(ou_pmtct$stat_d / stat_dd, 1),
                             " of the state pregnant women seen at ANC1\n[",
                             comma(ou_pmtct$stat_d), " out of ",
                             comma(stat_dd), "]"),
              color = grey10k,
              fontface = "bold",
              size = 10,
              hjust = 0) +
    coord_flip() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(axis.text = element_blank(),
          text = element_text())

  plot_psnu_stat_cov

  # Stat Pos ----
  plot_psnu_stat_pos <- psnu_pmtct %>%
    ggplot() +
    geom_col(aes(x = 0, y = 1), width = 10, fill = scooter_med) +
    geom_col(aes(x = 0, y = stat_yield), width = 10, fill = burnt_sienna_light) +
    geom_col(aes(x = 0, y = 1), width = 10, fill = NA, size = 1.5, color = burnt_sienna) +
    geom_hline(aes(yintercept = stat_yield),
               size = 1.5, lty = "dashed", color = burnt_sienna) +
    geom_richtext(aes(x = 2.5, y = .1),
                  label = paste0("<img src='", get_logo("female", fill_color = burnt_sienna_light, stroke_color = "white", prefix = "f_burnt_sienna_light_s_white"), "' width='100' />"),
                  fill = NA, color = NA) +
    geom_text(aes(x = 2.5, y = .2),
              label = paste0(percent(ou_pmtct$stat_yield, 1)),
              color = grey10k,
              fontface = "bold",
              size = 80,
              hjust = 0) +
    geom_text(aes(x = 0, y = .2),
              label = paste0("of pregnant women have tested HIV+\n[",
                             comma(ou_pmtct$stat_pos), " out of ",
                             comma(ou_pmtct$stat), "]"),
              color = "white",
              fontface = "bold",
              size = 10,
              hjust = 0) +
    coord_flip() +
    si_style_nolines() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(axis.text = element_blank())

  plot_psnu_stat_pos

  # Stat Pos on ART ----
  plot_psnu_art_cov <- psnu_pmtct %>%
    ggplot() +
    geom_col(aes(x = 0, y = 1), width = 10, fill = burnt_sienna_light) +
    geom_col(aes(x = 0, y = art_cov), width = 10, fill = genoa_light) +
    geom_col(aes(x = 0, y = 1), width = 10, fill = NA, size = 1.5, color = genoa) +
    geom_hline(aes(yintercept = art_cov),
               size = 1.5, lty = "dashed", color = genoa) +
    geom_richtext(aes(x = 2.5, y = .1),
                  label = paste0("<img src='", get_logo("female", fill_color = "white", stroke_color = "white", prefix = "f_white_s_genoa"), "' width='100' />"),
                  fill = NA, color = NA) +
    geom_text(aes(x = 2.5, y = .2),
              label = paste0(percent(ou_pmtct$art_cov, 1)),
              color = grey10k,
              fontface = "bold",
              size = 80,
              hjust = 0) +
    geom_text(aes(x = 0, y = .2),
              label = paste0("of HIV+ women receive ART\n[",
                             comma(ou_pmtct$art), " out of ",
                             comma(ou_pmtct$stat_pos), "]"),
              color = grey10k,
              fontface = "bold",
              size = 10,
              hjust = 0) +
    coord_flip() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(axis.text = element_blank())

  plot_psnu_art_cov



  # -----

  stat_breaks_bin <- 5000

  stat_breaks_end <- spdf_com_stat %>%
    filter(metric %in% c("stat_d", "stat")) %>%
    pull(value) %>%
    max(., na.rm = T) %>%
    plyr::round_any(., stat_breaks_bin)



  stat_labels <- c("# ELIGIBLE FOR TESTING", "# ACTUALLY TESTED")
  names(stat_labels) <- c("stat_d", "stat")

  stat_pos_labels <- c("# PREGNANT WOMEN HIV+")
  names(stat_pos_labels) <- c("stat_pos")

  # Map - Community Testing ----
  map_pmtct_stat <- basemap +
    geom_sf(data = spdf_com_stat %>%
              filter(metric %in% c("stat_d", "stat")),
            aes(fill = value),
            size = .2, color = grey10k) +
    geom_sf(data = spdf_cntry, size = 1.5, fill = NA, color = grey10k) +
    geom_sf(data = spdf_psnu, size = .5, fill = NA, color = grey40k) +
    geom_sf(data = spdf_cntry, size = .5, fill = NA, color = grey60k) +
    geom_sf_text(data = spdf_psnu, aes(label = name), size = 4, color = usaid_darkgrey) +
    scale_fill_si(palette = "scooters",
                  breaks = seq(0, max(spdf_com_stat$value, na.rm = T), 5000),
                  limits = c(0, max(spdf_com_stat$value, na.rm = T))) +
    labs(x = "", y = "") +
    facet_wrap(~metric, nrow = 1, labeller = labeller(metric = stat_labels)) +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  map_pmtct_stat +
    plot_annotation(
      #title = "PREGNANT WOMEN WITH KNOWN HIV STATUS AT ANTENATAL CARE (ANC)",
      subtitle = paste0(
        "As of ", curr_pd, ", **",
        percent(ou_pmtct$stat / ou_pmtct$stat_d, 1),
        "** of clients were tested with **",
        percent(ou_pmtct$stat_pos / ou_pmtct$stat, 1),
        "** of them testing HIV+"
      ),
      theme = theme(legend.position = "bottom",
                    plot.subtitle = element_markdown()))

  si_save(filename = file.path(dir_graphics, glue("NIGERIA - {curr_pd} PMTCT HIV Testing map.png")),
          plot = last_plot())

  # Map - Community Testing HIV+ ----

  map_pmtct_stat_pos <- basemap +
    geom_sf(data = spdf_com_stat %>%
              filter(metric %in% c("stat_pos")),
            aes(fill = value),
            size = .2, color = grey10k) +
    geom_sf(data = spdf_cntry, size = 1.5, fill = NA, color = grey10k) +
    geom_sf(data = spdf_psnu, size = .3, fill = NA, color = grey40k) +
    geom_sf(data = spdf_cntry, size = .5, fill = NA, color = grey60k) +
    geom_sf_text(data = spdf_psnu, aes(label = name), size = 4, color = usaid_darkgrey) +
    scale_fill_si(palette = "burnt_siennas",
                  breaks = seq(0, max(filter(spdf_com_stat, metric == "stat_pos") %>% pull(value), na.rm = T), 50),
                  limits = c(0, max(filter(spdf_com_stat, metric == "stat_pos") %>% pull(value), na.rm = T))) +
    labs(x = "", y = "") +
    facet_wrap(~metric, nrow = 1, labeller = labeller(metric = stat_pos_labels)) +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  map_pmtct_stat_pos +
    plot_annotation(
      #title = "PREGNANT WOMEN WITH KNOWN HIV STATUS AT ANTENATAL CARE (ANC)",
      subtitle = paste0(
        "As of ", curr_pd, ", **",
        percent(ou_pmtct$stat_yield, 1),
        "** of clients tested HIV+"
      ),
      theme = theme(legend.position = "bottom",
                    plot.subtitle = element_markdown()))

  si_save(filename = file.path(dir_graphics, "NIGERIA - FY21 PMTCT HIV Positive map.png"),
          plot = last_plot())


  # Map - Community HIV+ to ART ----

  map_pmtct_stat_linkage <- basemap +
    geom_sf(data = spdf_com_stat %>% filter(metric %in% c("art_cov"), value < .95),
            aes(fill = value),
            size = .2, color = grey10k) +
    geom_sf(data = spdf_cntry, size = 1.5, fill = NA, color = grey10k) +
    geom_sf(data = spdf_psnu, size = .3, fill = NA, color = grey40k) +
    geom_sf(data = spdf_cntry, size = .5, fill = NA, color = grey60k) +
    geom_sf_text(data = spdf_psnu, aes(label = name), size = 5, color = usaid_darkgrey) +
    scale_fill_si(palette = "genoas",
                  labels = percent,
                  breaks = c(seq(0, .75, .25), .95),
                  limits = c(0, 1)) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  map_pmtct_stat_linkage

  map_pmtct_stat_linkage +
    plot_annotation(
      #title = "HIV+ PREGNANT WOMEN LINKED TO TREATMENT",
      subtitle = paste0(
        "As of ", curr_pd, ", **",
        percent(ou_pmtct$art_cov, 1),
        "** HIV+ are on Treatment. **",
        (spdf_com_stat %>% filter(metric %in% c("art_cov"), value < .95) %>% nrow()),
        "** communities are below **95%** Coverage"
      ),
      theme = theme(legend.position = "bottom",
                    plot.subtitle = element_markdown(size = 14)))

  si_save(filename = file.path(dir_graphics, "NIGERIA - FY21 PMTCT ART Coverage map.png"),
          plot = last_plot())

  # Bar plot - HTS ----

  bars_pmtct_stat <- df_com_stat %>%
    ggplot(aes(reorder(community, stat_cov), stat_cov)) +
    geom_col(fill = scooter_med, show.legend = F, width = .9) +
    geom_hline(yintercept = c(.25, .50, .75), size = 1, color = grey10k) +
    geom_text(aes(label = percent(stat_cov, 1)),
              color = grey90k, size = 4, fontface = "bold",
              hjust = 1.1, vjust = .5) +
    #scale_fill_manual(values = c(scooter_light, grey20k)) +
    scale_y_continuous(expand = c(0, 0), labels = percent, position = "right") +
    coord_flip() +
    labs(x = "", y = "") +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(face = "bold"))

  bars_pmtct_stat

  bars_pmtct_stat +
    plot_annotation(
      title = "EDO - PMTCT TESTING AT COMMUNITY LEVEL",
      subtitle = paste0(
        "As of ", curr_pd, ", **",
        percent(psnu_pmtct$stat / psnu_pmtct$stat_d, 1),
        "** of the pregnant women have been tested at ANC1 in **EDO** State",
        "<br/>With only **", percent(psnu_pmtct$stat_d / stat_dd, 1), "** the state's pregnant women showing up at PEPFAR Supported Facilities"),
      theme = theme(plot.title = element_markdown(size = 20),
                    plot.subtitle = element_markdown(size = 16),
                    text = element_text(size = 4)))

  ggsave(filename = file.path(dir_graphics, glue("NIGERIA - {curr_pd} EDO STATE - PMTCT HIV Testing Bars Plot.png")),
          plot = last_plot())

  # Bar plot - HIV+ ----

  bars_pmtct_stat_pos <- df_com_stat %>%
    filter(!is.na(stat_yield)) %>%
    ggplot(aes(reorder(community, stat_yield), stat_yield)) +
    geom_col(fill = burnt_sienna_light, show.legend = F) +
    geom_hline(yintercept = seq(0, .06, .01), size = 1, color = "white") +
    geom_text(aes(label = percent(stat_yield, .1)),
              color = "white", size = 4, fontface = "bold",
              hjust = 1.2, vjust = .5) +
    scale_color_identity() +
    scale_y_continuous(expand = c(0, 0),
                       labels = percent,
                       position = "right",
                       breaks = seq(0, .06, .01)) +
    coord_flip(ylim = c(0, .1)) +
    labs(x = "", y = "") +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(face = "bold"))

  bars_pmtct_stat_pos

  bars_pmtct_stat_pos +
    plot_annotation(
      title = "EDO - PMTCT TESTING AT COMMUNITY LEVEL",
      subtitle = paste0(
        "As of ", curr_pd, ", **",
        percent(ou_pmtct$stat_yield, .1),
        "** of pregnant women tested were HIV+"
      ),
      theme = theme(plot.title = element_markdown(size = 20),
                    plot.subtitle = element_markdown(size = 16),
                    text = element_text(size = 4),
                    axis.text = element_markdown(size = 20, color = usaid_black)))

  ggsave(filename = file.path(dir_graphics, glue("NIGERIA - {curr_pd} EDO STATE - PMTCT HIV Positive Bars Plot.png")),
          plot = last_plot())


  # Bar plot - HIV+ to ART ----

  bars_pmtct_stat_linkage <- df_com_stat %>%
    filter(!is.na(art_cov)) %>%
    ggplot(aes(reorder(community, art_cov), art_cov)) +
    geom_col(fill = genoa_light, show.legend = F) +
    geom_hline(yintercept = seq(0, 1, .25), size = 1, color = grey10k) +
    geom_text(aes(label = percent(art_cov, .1)),
              color = grey10k, size = 3, fontface = "bold",
              hjust = 1.2, vjust = .5) +
    scale_y_continuous(expand = c(0, 0),
                       labels = percent,
                       position = "right",
                       breaks = seq(0, 1, .25)) +
    coord_flip(ylim = c(0, 1)) +
    labs(x = "", y = "") +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(face = "bold"))

  bars_pmtct_stat_linkage

  bars_pmtct_stat_linkage +
    plot_annotation(
      title = "EDO - PMTCT TESTING AT COMMUNITY LEVEL",
      subtitle = paste0(
        "As of ", curr_pd, ", **",
        percent(psnu_pmtct$art_cov, .1),
        "** of pregnant women testing HIV+ are linked to TX"
      ),
      theme = theme(plot.title = element_markdown(size = 20),
                    plot.subtitle = element_markdown(size = 16),
                    text = element_text(size = 4),
                    axis.text = element_markdown(size = 20, color = usaid_black)))

  ggsave(filename = file.path(dir_graphics, glue("NIGERIA - {curr_pd} EDO STATE - PMTCT HIV Positive Bars Plot.png")),
          plot = last_plot())
