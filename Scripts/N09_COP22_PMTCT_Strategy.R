##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: COP22 / FY23 Targets Allocation
##  LICENCE: MIT
##  DATE:    2022-03-28
##  UPDATED: 2022-07-08

## Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glamr)
  library(gisr)
  library(sf)
  library(janitor)
  library(glue)
  #library(gt)
  #library(gtExtras)
  library(ggtext)
  library(patchwork)
  library(ggchicklet)
  library(ggridges)

  library(fontawesome)
  library(emojifont)
  library(extrafont)

  font_import()

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

  ou_uid <- get_ouuid(cntry)

  fac_level <- get_ouorglevel(operatingunit = cntry, org_type = "facility")

  agency <- "USAID"

  curr_pd <- file_psnu_im %>% identify_pd()

  curr_fy <- curr_pd %>%
    str_sub(1, 4) %>%
    str_replace("FY", "20") %>%
    as.numeric()

  prev_fy <- curr_fy - 1

# FUNCTIONS

  #' @title Get LOGO
  #'
  #' @note
  #'
  #' @param file Prefix of filename. Use f for fill and s for stroke, eg: f_red_s_white
  #'
  get_logo <- function(name = "hospital",
                       fill_color = "white",
                       stroke_color = NULL,
                       stroke_width = NULL,
                       prefix = NULL,
                       path = NULL,
                       ...) {

    # Path
    if (is.null(path))
      path <- "./Images/logos"

    # Filename
    file <- paste0(name, ".png")

    if (!is.null(prefix))
      file <- paste0(prefix, "-", name, ".png")

    # Check if file exists
    img <- list.files(path, pattern = file, full.names = T)

    if (length(img) == 1)
      return(img)

    img <- file.path(path, file)

    # Save file
    fa_png(name = name,
           fill = fill_color,
           stroke = stroke_color,
           file = img,
           ...)

    return(img)
  }

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
  spdf_lga <- spdf_nga %>% filter(label == "community")

  # spdf_list <- spdf_pepfar %>%
  #   cntry_polygons(spdf = ., cntry = cntry)

  # Site Coordinates
  df_site_locs <- extract_locations(country = cntry,
                                    level = fac_level,
                                    add_geom = TRUE) %>%
    extract_facilities()

  df_site_locs <- df_site_locs %>%
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    select(uid = id, name, latitude, longitude)


  # MSD Site x IM ----

  df_sites <- file_site_im %>% read_msd()

  # Agency x State coverage
  df_agency_cov <- df_sites %>%
    filter(fiscal_year == curr_fy,
           funding_agency != "Dedup") %>%
    distinct(funding_agency, psnu) %>%
    clean_agency()

  usaid_psnus <- df_agency_cov %>%
    filter(funding_agency == "USAID",
           !psnu %in% c("Lagos", "_Military Nigeria")) %>%
    pull(psnu) %>%
    sort()

  cdc_psnus <- df_agency_cov %>%
    filter(funding_agency == "CDC",
           !psnu %in% c("Lagos", "_Military Nigeria")) %>%
    pull(psnu) %>%
    sort()

  # MSD Site x IM
  df_msd_sites <- df_sites %>%
    filter(fiscal_year == curr_fy, funding_agency != "Dedup")

  inds_pmtct <- df_msd_sites %>%
    filter(str_detect(indicator, "PMTCT_.*")) %>%
    distinct(indicator) %>%
    pull(indicator)

  inds_t_nd <- df_msd_sites %>%
    filter(str_detect(indicator, "PMTCT_.*"),
           str_detect(standardizeddisaggregate, "^Total")) %>%
    distinct(standardizeddisaggregate) %>%
    pull(standardizeddisaggregate)

  inds_t_nd <- c("Total Numerator", "Total Denominator")

  inds_disaggs <- df_msd_sites %>%
    filter(str_detect(indicator, "PMTCT_.*"),
           str_detect(standardizeddisaggregate, "^Total", negate = T)) %>%
    distinct(standardizeddisaggregate) %>%
    pull(standardizeddisaggregate)

  df_msd_sites <- df_msd_sites %>%
    filter(indicator %in% inds_pmtct) %>%
    clean_indicator()

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
    #group_by(psnu, community) %>%
    summarise(n = n_distinct(orgunituid), .groups = "drop")

  # MSD Site x IM - PMTCT Facilities Summary

  df_pmtct_coms <- df_pmtct_sites %>%
    filter(communityuid != "?") %>%
    group_by_at(.vars = vars(one_of("funding_agency", str_msd_sites$org_comms, str_msd_sites$org_psnus))) %>%
    summarise(pmtct_sites = n_distinct(orgunituid), .groups = "drop") %>%
    clean_column(colname = "community")

  df_pmtct_psnus <- df_pmtct_sites %>%
    filter(communityuid != "?") %>%
    group_by_at(.vars = vars(one_of(str_msd_sites$org_psnus))) %>%
    summarise(pmtct_sites = n_distinct(orgunituid), .groups = "drop")

  # MSD PSNU x IM ----

  # df_psnu <- file_psnu_im %>% read_msd()
  #
  # df_psnu %>%
  #   filter(fiscal_year == prev_fy,
  #          fundingagency != "Dedup",
  #          #fundingagency == agency,
  #          str_detect(indicator, "PMTCT_*")) %>%
  #   group_by(indicator, standardizeddisaggregate, otherdisaggregate,
  #            ageasentered, statushiv, hiv_treatment_status) %>%
  #   summarise(results = sum(cumulative, na.rm = T), .groups = "drop")

# MUNGE ----

  # PMTCT Sites ----

  spdf_pmtct_sites <- df_pmtct_sites %>%
    filter(!is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))

  spdf_pmtct_sites <- spdf_cntry %>%
    st_make_valid() %>%
    st_filter(spdf_pmtct_sites, ., .predicate = st_intersects)

  spdf_com_sites <- spdf_lga %>%
    left_join(df_pmtct_coms, by = c("uid" = "communityuid"))

  spdf_psnu_sites <- spdf_psnu %>%
    left_join(df_pmtct_psnus, by = c("uid" = "psnuuid")) %>%
    mutate(psnu_label_color = case_when(
      pmtct_sites > 200 ~ grey10k,
      TRUE ~ grey90k
    ))

  # PMTCT - STAT @ Community ----

  df_msd_sites %>% distinct(indicator) %>% prinf()

  df_com_stat <- df_msd_sites %>%
    filter(str_detect(indicator, "_STAT|T_ART"),
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
    ungroup()

  spdf_com_stat <- df_com_stat %>%
    pivot_longer(cols = starts_with(c("stat", "art")),
                 names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(
      metric,
      levels = c("stat_d", "stat", "stat_nn", "stat_cov", "stat_cov_breaks",
                 "stat_pos", "stat_yield", "stat_na", "art_d", "art", "art_cov"))) %>%
    left_join(x = spdf_lga, y = ., by = c("uid" = "communityuid")) %>%
    filter(!is.na(value))

  # PMTCT - STAT @ PSNU ----

  df_psnu_stat <- df_msd_sites %>%
    filter(str_detect(indicator, "_STAT|_STAT_POS|T_ART"),
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

  # OU Summary
  ou_pmtct <- df_psnu_stat %>%
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

  icons <- c("hospital", "female", "mars", "mars-double", "venus", "venus-double",
             "intersex", "transgender", "child", "user-md", "user-times",
             "user-plus", "bed", "wheelchair", "wheelchair-alt",
             "thumbs-o-up", "thumbs-o-down",
             "check-square", "window-close", "map-marker", "trophy",
             "arrow-up", "arrow-down", "arrows-h")

  walk(icons, function(.x){
    print(.x)
    fa_png(name = .x,
           fill = "white",
           file = paste0(dir_images, "/logos/", .x, ".png"))
  })

  # Plots ----

  # Stat ----
  plot_ou_stat_cov <- ou_pmtct %>%
    ggplot() +
    geom_col(aes(x = 0, y = 1), width = 10, fill = trolley_grey_light) +
    geom_col(aes(x = 0, y = stat_cov), width = 10, fill = scooter_med) +
    geom_col(aes(x = 0, y = 1), width = 10, fill = NA, size = 1.5, color = scooter) +
    geom_hline(aes(yintercept = stat_cov),
               size = 1.5, lty = "dashed", color = scooter) +
    geom_richtext(aes(x = 2.5, y = .1),
                  label = paste0("<img src='", get_logo("female"), "' width='100' /> "),
                  fill = NA, color = NA) +
                  #label = paste0("<img src='", get_logo("female"), "' width='30' /> ")) +
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
    coord_flip() +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(axis.text = element_blank(),
          text = element_text())

  plot_ou_stat_cov

  # Stat Pos ----
  plot_ou_stat_pos <- ou_pmtct %>%
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

  plot_ou_stat_pos

  # Stat Pos on ART ----
  plot_ou_art_cov <- ou_pmtct %>%
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

  plot_ou_art_cov


  # Viz plots ----
  (plot_ou_stat_cov / plot_ou_stat_pos / plot_ou_art_cov) +
    plot_layout(heights = c(3,3,3)) +
    theme(text = element_text(size = 6))

  dim_max <- get_max_dim(plot_ou_stat_cov, plot_ou_stat_pos,
                         plot_ou_art_cov, map_pmtct_stat)

  set_dim(plot_ou_stat_cov, dim_max)
  set_dim(map_pmtct_stat, dim_max)

  cowplot::plot_grid(plot_ou_stat_cov, map_pmtct_stat,
                     ncol = 1,
                     align = "hv",
                     axis = "bt",
                     rel_heights = c(1, 5))

  (plot_ou_stat_cov / (plot_ou_stat_pos + plot_ou_art_cov)) +
    plot_layout(heights = c(20, 20, 60))

  # Maps ----

  # Map - Basemap
  basemap <- terrain_map(countries = cntry,
                         adm0 = spdf_cntry,
                         adm1 = spdf_psnu,
                         mask = T)

  # Map - Site Locations ----

  map_sites <- basemap +
    geom_sf(data = spdf_cntry, size = 1.5, fill = NA, color = grey10k) +
    geom_sf(data = spdf_pmtct_sites,
            aes(fill = funding_agency),
                shape = 21, size = 2,
                color = grey10k, alpha = .3,
            show.legend = F) +
    geom_sf_text(data = spdf_psnu, aes(label = name),
                 size = 3, color = usaid_black) +
    geom_sf(data = spdf_cntry, size = .5, fill = NA, color = grey60k) +
    scale_fill_manual(values = c("USAID" = usaid_red, "CDC" = usaid_blue, "DOD" = usaid_lightgrey)) +
    labs(x = "", y = "",
         title = glue("<span style='color:{usaid_red};'>USAID</span> and <span style='color:{usaid_blue};'>CDC</span> PMTCT Facilities"),
         subtitle = glue("**Note**: Facilities missing location data are not displayed")) +
    si_style_map() +
    theme(plot.title = element_markdown(size = 14, hjust = .5),
          plot.subtitle = element_markdown(size = 12, hjust = .5),
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  map_edo_sites <- basemap +
    geom_sf(data = spdf_cntry, size = 1.5, fill = NA, color = grey10k) +
    geom_sf(data = spdf_pmtct_sites %>% filter(psnu == "Edo"),
            aes(fill = funding_agency),
            shape = 21, size = 2,
            color = grey10k, alpha = .3,
            show.legend = F) +
    geom_sf_text(data = spdf_psnu, aes(label = name),
                 size = 3, color = usaid_black) +
    geom_sf(data = spdf_cntry, size = .5, fill = NA, color = grey60k) +
    scale_fill_manual(values = c("USAID" = usaid_red, "CDC" = usaid_blue, "DOD" = usaid_lightgrey)) +
    labs(x = "", y = "",
         title = glue("<span style='color:{usaid_red};'>USAID</span> and <span style='color:{usaid_blue};'>CDC</span> PMTCT Facilities"),
         subtitle = glue("**Note**: Facilities missing location data are not displayed")) +
    si_style_map() +
    theme(plot.title = element_markdown(size = 14, hjust = .5),
          plot.subtitle = element_markdown(size = 12, hjust = .5),
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  map_sites %>%
    #si_save(
    ggsave(
      filename = file.path(
        dir_graphics, glue("NIGERIA - {curr_pd} PMTCT Facilities map.png")),
      plot = .,
      units = 'in',
      width = 10,
      height = 5.5)

  # Map - Site Summary by Community ----

  map_com_sites <- basemap +
    geom_sf(data = spdf_cntry, size = 1.5, fill = NA, color = grey10k) +
    geom_sf(data = spdf_com_sites,
            aes(fill = pmtct_sites),
            color = grey10k, size = .1,
            show.legend = T, na.rm = T) +
    geom_sf_text(data = spdf_psnu, aes(label = name), size = 3, color = usaid_black) +
    geom_sf(data = spdf_psnu, size = .2, fill = NA, color = grey40k) +
    geom_sf(data = spdf_cntry, size = .5, fill = NA, color = grey60k) +
    scale_fill_si(palette = "burnt_siennas", na.value = NA,
                  breaks = seq(0, max(spdf_com_sites$pmtct_sites, na.rm = T), 5),
                  limits = c(0, max(spdf_com_sites$pmtct_sites, na.rm = T))) +
    labs(x = "", y = "",
         title = glue("PMTCT Facilities by PSNU/Communities")
         #subtitle = glue("**Note**: DOD Facilities are not displayed")
         ) +
    si_style_map() +
    theme(plot.title = element_markdown(size = 14, hjust = 0.04),
          plot.subtitle = element_markdown(size = 12, hjust = .5),
          legend.title = element_blank(),
          legend.position = "top",
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  map_com_sites %>%
    ggsave(
      filename = file.path(
        dir_graphics, glue("NIGERIA - {curr_pd} PMTCT Facilities by Cummunity map.png")),
      plot = .,
      units = 'in',
      width = 10,
      height = 5.5)

  # Map - Site Summary by PSNU ----

  map_psnu_sites <- basemap +
    geom_sf(data = spdf_cntry, size = 1.5, fill = NA, color = grey10k) +
    geom_sf(data = spdf_psnu_sites,
            aes(fill = pmtct_sites),
            color = grey10k, size = .1,
            show.legend = T, na.rm = T) +
    geom_sf_text(data = spdf_psnu_sites,
                 aes(label = paste0(name, "\n(", pmtct_sites, ")"),
                     color = psnu_label_color),
                 size = 2) +
    geom_sf(data = spdf_psnu, size = .2, fill = NA, color = grey40k) +
    geom_sf(data = spdf_cntry, size = .5, fill = NA, color = grey60k) +
    scale_fill_si(palette = "burnt_siennas", na.value = NA,
                  breaks = seq(0, max(spdf_psnu_sites$pmtct_sites, na.rm = T), 50),
                  limits = c(0, max(spdf_psnu_sites$pmtct_sites, na.rm = T))) +
    scale_color_identity() +
    labs(x = "", y = "",
         title = glue("PMTCT Facilities by State")
         #subtitle = glue("**Note**: DOD Facilities are not displayed")
    ) +
    si_style_map() +
    theme(plot.title = element_markdown(size = 14, hjust = 0.04),
          plot.subtitle = element_markdown(size = 12, hjust = .5),
          legend.title = element_blank(),
          legend.position = "top",
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  map_psnu_sites %>%
    ggsave(
      filename = file.path(
        dir_graphics, glue("NIGERIA - {curr_pd} PMTCT Facilities by PSNU map.png")),
      plot = .,
      units = 'in',
      width = 10,
      height = 5.5)


  # -----

  stat_breaks_bin <- 5000

  stat_breaks_end <- spdf_com_stat %>%
    filter(metric %in% c("stat_d", "stat")) %>%
    pull(value) %>%
    max(., na.rm = T) %>%
    plyr::round_any(., stat_breaks_bin)

  #map_pmtct_stat <-
  # basemap +
  #   geom_sf(data = spdf_com_stat %>%
  #             filter(metric == "stat_cov_breaks") %>%
  #             mutate(
  #               value = factor(
  #                 value,
  #                 labels = c("NA", "<50%", "50-75%", "75-95%", "95+", "Other"))
  #             ) %>% dview(),
  #           aes(fill = value),
  #           size = .2, color = grey10k) +
  #   geom_sf(data = spdf_cntry, size = 1.5, fill = NA, color = grey10k) +
  #   geom_sf(data = spdf_psnu, size = .3, fill = NA, color = grey40k) +
  #   geom_sf(data = spdf_cntry, size = .5, fill = NA, color = grey60k) +
  #   geom_sf_text(data = spdf_psnu, aes(label = name), size = 3.5, color = grey10k, fontface = "bold") +
  #   geom_sf_text(data = spdf_psnu, aes(label = name), size = 3, color = usaid_black) +
  #   scale_fill_si(palette = "genoas", discrete = T) +
  #   labs(x = "", y = "",
  #        title = "NIGERIA - PMTCT",
  #        subtitle = "% PMTCT STAT") +
  #   facet_wrap(~metric, nrow = 1) +
  #   si_style_map() +
  #   theme(legend.title = element_blank(),
  #         legend.key.height = unit(.1, "in"),
  #         legend.key.width = unit(1, "in"))

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

  df_psnu_stat_viz <- df_psnu_stat %>%
    filter(metric %in% c("stat", "stat_nn")) %>%
    group_by(psnu) %>%
    mutate(total = sum(value, na.rm = T),
           stat = value[metric == "stat"],
           stat_nn = value[metric == "stat_nn"],
           prop = stat / total,
           label = paste0("<span style='color: ",
                          ifelse(psnu %in% usaid_psnus, usaid_red,
                                 ifelse(psnu %in% cdc_psnus, usaid_blue,
                                        usaid_darkgrey)),"';>",
                          psnu,
                          " (", comma(total),
                          ")</span>")) %>%
    ungroup()

  bars_pmtct_stat <- df_psnu_stat_viz %>%
    ggplot(aes(reorder(label, prop), value, fill = metric)) +
    geom_col(position = position_fill(reverse = T), show.legend = F, width = .9) +
    geom_hline(yintercept = c(.25, .50, .75), size = 1, color = grey10k) +
    geom_text(data = df_psnu_stat_viz %>% filter(metric == "stat"),
              aes(x = reorder(label, prop), y = prop,
                  #label = paste0(comma(stat), " - ", percent(prop, .1)))
                  label = percent(prop, 1)
                  ),
              color = grey90k, size = 4, fontface = "bold",
              hjust = 1.1, vjust = .5) +
    annotate(geom = "rect", xmin = .45, xmax = 6.41, ymin = 0, ymax = .1,
             fill = grey10k, alpha = .8) +
    annotate(geom = "text", x = 3.5, y = .05, label = "STATES\nwith\n<95%",
             color = "white", size = 10, fontface = "bold",) +
    scale_color_identity() +
    scale_fill_manual(values = c(scooter_light, grey20k)) +
    scale_y_continuous(expand = c(0, 0), labels = percent, position = "right") +
    coord_flip() +
    labs(x = "", y = "") +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(face = "bold"))

  bars_pmtct_stat

  bars_pmtct_stat +
    plot_annotation(
      #title = "HIV+ PREGNANT WOMEN LINKED TO TREATMENT",
      subtitle = paste0(
        "As of ", curr_pd, ", **",
        percent(ou_pmtct$stat / ou_pmtct$stat_d, 1),
        "** have been tested, with ",
        "<span style='color:", usaid_red, "'>USAID</span> & ",
        "<span style='color:", usaid_blue, "'>CDC</span> having states below the OU level"
      ),
      theme = theme(plot.subtitle = element_markdown(size = 20),
                    text = element_text(size = 4)))

  si_save(filename = file.path(dir_graphics, glue("NIGERIA - {curr_pd} PMTCT HIV Testing Bars Plot.png")),
          plot = last_plot())

  # Bar plot - HIV+ ----

  df_psnu_stat_pos_viz <- df_psnu_stat %>%
    filter(metric %in% c("stat_np", "stat_pos")) %>%
    group_by(psnu) %>%
    mutate(total = sum(value, na.rm = T),
           pos = value[metric == "stat_pos"],
           prop = pos / total,
           label = paste0("<span style='color: ",
                          ifelse(psnu %in% usaid_psnus, usaid_red,
                                 ifelse(psnu %in% cdc_psnus, usaid_blue,
                                        usaid_darkgrey)),"';>",
                          psnu,
                          " (", comma(pos),
                          ")</span>")) %>%
    ungroup()

  bars_pmtct_stat_pos <- df_psnu_stat_pos_viz %>%
    ggplot(aes(reorder(label, prop), value, fill = metric)) +
    geom_col(position = position_fill(reverse = T), show.legend = F) +
    geom_hline(yintercept = seq(0, .1, .02), size = 1, color = grey10k) +
    geom_text(data = df_psnu_stat_pos_viz %>% filter(metric == "stat_pos"),
              aes(x = reorder(label, prop), y = prop, label = percent(prop, .1)),
              color = grey10k, size = 3, fontface = "bold",
              hjust = 1.2, vjust = .5) +
    scale_color_identity() +
    scale_fill_manual(values = c(burnt_sienna, grey20k)) +
    scale_y_continuous(expand = c(0, 0),
                       labels = percent,
                       position = "right",
                       breaks = seq(0, .1, .01)) +
    coord_flip(ylim = c(0, .1)) +
    labs(x = "", y = "") +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(face = "bold"))

  bars_pmtct_stat_pos

  bars_pmtct_stat_pos +
    plot_annotation(
      #title = "HIV+ PREGNANT WOMEN",
      subtitle = paste0(
        "As of ", curr_pd, ", **",
        percent(ou_pmtct$stat_yield, .1),
        "** of pregnant women tested are HIV+<br/>",
        "<span style='color:", usaid_red, "'>USAID</span> & ",
        "<span style='color:", usaid_blue, "'>CDC</span> have multiple states with >2% HIV+"
      ),
      theme = theme(plot.subtitle = element_markdown(size = 20),
                    text = element_text(size = 4),
                    axis.text = element_markdown(size = 20, color = usaid_black)))

  si_save(filename = file.path(dir_graphics, glue("NIGERIA - {curr_pd} PMTCT HIV Positive Bars Plot.png")),
          plot = last_plot())


  # Bar plot - HIV+ to ART ----

  df_psnu_stat_linkage <- df_psnu_stat %>%
    filter(metric %in% c("stat_pos", "art", "art_cov")) %>%
    group_by(psnu) %>%
    mutate(total = value[metric == "art"],
           prop = value[metric == "art_cov"],
           pos = value[metric == "stat_pos"],
           label = paste0("<span style='color: ",
                          ifelse(psnu %in% usaid_psnus, usaid_red,
                                 ifelse(psnu %in% cdc_psnus, usaid_blue,
                                        usaid_darkgrey)),"';>",
                          psnu,
                          " (", comma(total),
                          ")</span>")) %>%
    ungroup() %>%
    filter(metric == "art_cov")

  bars_pmtct_stat_linkage <- df_psnu_stat_linkage %>%
    ggplot(aes(reorder(label, prop))) +
    #geom_col(aes(y = 1), fill = trolley_grey_light, show.legend = F) +
    geom_col(aes(y = prop), fill = scooter_med, show.legend = F) +
    geom_hline(yintercept = seq(0, 1, .25), size = 1, color = grey10k) +
    geom_text(data = df_psnu_stat_pos_viz %>% filter(metric == "stat_pos"),
              aes(x = reorder(label, prop), y = prop, label = percent(prop, .1)),
              color = grey10k, size = 3, fontface = "bold",
              hjust = 1.2, vjust = .5) +
    scale_color_identity() +
    scale_fill_manual(values = c(burnt_sienna, grey20k)) +
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
      #title = "HIV+ PREGNANT WOMEN",
      subtitle = paste0(
        "As of ", curr_pd, ", **",
        percent(ou_pmtct$stat_yield, .1),
        "** of pregnant women tested are HIV+<br/>",
        "<span style='color:", usaid_red, "'>USAID</span> & ",
        "<span style='color:", usaid_blue, "'>CDC</span> have multiple states with >2% HIV+"
      ),
      theme = theme(plot.subtitle = element_markdown(size = 20),
                    text = element_text(size = 4),
                    axis.text = element_markdown(size = 20, color = usaid_black)))

  si_save(filename = file.path(dir_graphics, glue("NIGERIA - {curr_pd} PMTCT HIV Positive Bars Plot.png")),
          plot = last_plot())
