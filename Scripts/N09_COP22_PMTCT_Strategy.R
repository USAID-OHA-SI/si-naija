##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: COP22 / FY23 Targets Allocation
##  LICENCE: MIT
##  DATE:    2022-03-28
##  UPDATED: 2022-03-28

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
  library(gtExtras)
  library(ggtext)
  library(patchwork)

  source("./Scripts/N00_Utilities.R")

## GLOBALS ----

# Dirs ----

  dir_merdata <- si_path("path_msd")
  dir_geodata <- si_path("path_vector")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"

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

  agency <- "USAID"

  curr_pd <- file_psnu_im %>% identify_pd()

  curr_fy <- curr_pd %>%
    str_sub(1, 4) %>%
    str_replace("FY", "20") %>%
    as.numeric()

  prev_fy <- curr_fy - 1

# DATA ----

  # Geodata ----

  ras <- get_raster()

  spdf_pepfar <- file_shp %>% read_sf()

  df_attr <- cntry %>% get_attributes()

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attr, by = c("uid" = "id"))

  spdf_cntry <- spdf_pepfar %>% filter(label == "country")
  spdf_psnu <- spdf_pepfar %>% filter(label == "prioritization")
  spdf_lga <- spdf_pepfar %>% filter(label == "community")

  spdf_lga %>% gview()

  # MSD Site x IM ----

  df_sites <- file_site_im %>% read_msd()

  # Agency states coverage
  df_agency_cov <- df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup") %>%
    distinct(fundingagency, psnu) %>%
    clean_agency()

  usaid_psnus <- df_agency_cov %>%
    filter(fundingagency == "USAID",
           !psnu %in% c("Lagos", "_Military Nigeria")) %>%
    pull(psnu) %>%
    sort()

  cdc_psnus <- df_agency_cov %>%
    filter(fundingagency == "CDC",
           !psnu %in% c("Lagos", "_Military Nigeria")) %>%
    pull(psnu) %>%
    sort()

  # MSD Site x IM
  df_msd_sites <- df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup")

  inds_pmtct <- df_msd_sites %>%
    filter(str_detect(indicator, "PMTCT_.*")) %>%
    distinct(indicator, standardizeddisaggregate) %>%
    pull(indicator) %>%
    unique()

  inds_t_nd <- c("Total Numerator", "Total Denominator")
  inds_disaggs <- c()

  df_msd_sites <- df_msd_sites %>%
    filter(indicator %in% inds_pmtct) %>%
    clean_indicator()

  # MSD PSNU x IM

  df_psnu <- file_psnu_im %>% read_msd()

  df_psnu %>%
    filter(fiscal_year == prev_fy,
           fundingagency != "Dedup",
           #fundingagency == agency,
           str_detect(indicator, "PMTCT_*")) %>%
    group_by(indicator, standardizeddisaggregate, otherdisaggregate,
             ageasentered, statushiv, hiv_treatment_status) %>%
    summarise(results = sum(cumulative, na.rm = T), .groups = "drop")

# MUNGE ----




  # PMTCT - STAT @ Community

  df_com_stat <- df_msd_sites %>%
    filter(str_detect(indicator, "_STAT|T_ART$"),
           standardizeddisaggregate %in% inds_t_nd,
           !is.na(cumulative)) %>%
    group_by(psnu, psnuuid, community, communityuid, indicator) %>%
    summarise(results = sum(cumulative, na.rm = T), .groups = "drop") %>%
    mutate(indicator = str_remove(indicator, "PMTCT_"))

  df_com_stat <- df_com_stat %>%
    mutate(indicator = factor(
      indicator,
      levels = c("STAT_D", "STAT", "STAT_POS", "ART"),
      ordered = TRUE)) %>%
    pivot_wider(names_from = indicator,
                names_sort = TRUE,
                values_from = results) %>%
    clean_names() %>%
    rowwise() %>%
    mutate(stat_nn = stat_d - stat,                    # Non tested
           stat_nn = ifelse(stat_nn < 0, 0, stat_nn),
           stat_cov = stat / stat_d,
           stat_yield = stat_pos / stat,
           stat_na = stat_pos - art,              # Non linked
           stat_na = ifelse(stat_na < 0, 0, stat_na),
           art_cov = art / stat_pos) %>%
    ungroup()

  spdf_com_stat <- df_com_stat %>%
    #select(-ends_with(c("cov", "yield"))) %>%
    pivot_longer(cols = starts_with(c("stat", "art")),
                 names_to = "metric",
                 values_to = "value") %>%
    mutate(metric = factor(metric, levels = c("stat_d", "stat", "stat_pos", "art"))) %>%
    left_join(x = spdf_lga, y = ., by = c("uid" = "communityuid")) %>%
    filter(!is.na(value))

  # PMTCT - STAT @ PSNU

  df_psnu_stat <- df_msd_sites %>%
    filter(str_detect(indicator, "_STAT|_STAT_POS|T_ART$"),
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
                           levels = c("stat_d", "stat", "stat_nn",
                                      "stat_pos", "stat_np", "stat_cov",
                                      "stat_yield", "art", "art_cov"),
                           ordered = T)) %>%
    filter(!is.na(value))


# VIZ ----

  # Maps

  basemap <- terrain_map(countries = cntry,
                         adm0 = spdf_cntry,
                         adm1 = spdf_psnu,
                         mask = T)

  #map_pmtct_stat <-
  basemap +
    geom_sf(data = spdf_com_stat %>% filter(metric %in% c("stat_d", "stat")),
            aes(fill = value),
            size = .2, color = grey10k) +
    geom_sf(data = spdf_psnu, size = .3, fill = NA, color = grey60k) +
    geom_sf_text(data = spdf_psnu, aes(label = name), size = 3.5, color = grey10k, fontface = "bold") +
    geom_sf_text(data = spdf_psnu, aes(label = name), size = 3, color = usaid_black) +
    scale_fill_si(palette = "genoas",
                  limits = c(0, max(spdf_com_stat$value))) +
    labs(x = "", y = "",
         title = "NIGERIA - PMTCT",
         subtitle = "% PMTCT STAT") +
    facet_wrap(~metric, nrow = 1) +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  stat_labels <- c("ELIGIBLE", "TESTED")
  names(stat_labels) <- c("stat_d", "stat")

  map_pmtct_stat <- basemap +
    geom_sf(data = spdf_com_stat %>%
              filter(metric %in% c("stat_d", "stat")),
            aes(fill = value),
            size = .2, color = grey10k) +
    geom_sf(data = spdf_psnu, size = .3, fill = NA, color = grey60k) +
    #geom_sf_text(data = spdf_psnu, aes(label = name), size = 3, color = grey10k, fontface = "bold") +
    geom_sf_text(data = spdf_psnu, aes(label = name), size = 2, color = usaid_darkgrey) +
    scale_fill_si(palette = "genoas",
                  limits = c(0, max(spdf_com_stat$value))) +
    labs(x = "", y = "") +
    facet_wrap(~metric, nrow = 1, labeller = labeller(metric = stat_labels)) +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  ou_pmtct <- df_psnu_stat %>%
    group_by(metric) %>%
    summarise(across(value, sum, na.rm = T)) %>%
    pivot_wider(names_from = metric, values_from = value)

  map_pmtct_stat +
    labs(#title = "NIGERIA - PMTCT HIV TESTING",
         subtitle = paste0(
           "As of FY21Q4, ",
           percent(ou_pmtct$stat / ou_pmtct$stat_d, 1),
           " of pregnant women were tested [",
           comma(ou_pmtct$stat),
           " out of ",
           comma(ou_pmtct$stat_d),
           "]"
         ))

  si_save(filename = file.path(dir_graphics, "NIGERIA - FY21 PMTCT HIV Testing map.png"),
          plot = last_plot())

  com_stat_pos_max <- spdf_com_stat %>%
    filter(metric %in% c("stat_pos")) %>% pull(value) %>% max()

  map_pmtct_stat_pos <- basemap +
    geom_sf(data = spdf_com_stat %>%
              filter(metric %in% c("stat_pos")),
            aes(fill = value),
            size = .2, color = grey10k) +
    geom_sf(data = spdf_psnu, size = .3, fill = NA, color = grey60k) +
    #geom_sf_text(data = spdf_psnu, aes(label = name), size = 3.5, color = grey10k, fontface = "bold") +
    geom_sf_text(data = spdf_psnu, aes(label = name), size = 2, color = usaid_black) +
    scale_fill_si(palette = "burnt_siennas", limits = c(0, com_stat_pos_max)) +
    labs(x = "", y = "") +
    #facet_wrap(~metric, nrow = 1) +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.height = unit(.1, "in"),
          legend.key.width = unit(1, "in"))

  si_save(filename = file.path(dir_graphics, "NIGERIA - FY21 PMTCT HIV Positive map.png"),
          plot = last_plot())

  df_psnu_stat_viz <- df_psnu_stat %>%
    filter(metric %in% c("stat", "stat_nn")) %>%
    group_by(psnu) %>%
    mutate(total = sum(value, na.rm = T),
           stat = value[metric == "stat"],
           prop = stat / total,
           label = paste0("<span style='color: ",
                          ifelse(psnu %in% usaid_psnus, usaid_blue,
                                 ifelse(psnu %in% cdc_psnus, usaid_lightblue,
                                        usaid_darkgrey)),"';>",
                          psnu,
                          " (", comma(total),
                          ")</span>")) %>%
    ungroup()

  bars_pmtct_stat <- df_psnu_stat_viz %>%
    ggplot(aes(reorder(label, prop), value, fill = metric)) +
    geom_col(position = position_fill(reverse = T), show.legend = F) +
    geom_text(data = df_psnu_stat_viz %>% filter(metric == "stat"),
              aes(x = reorder(label, prop), y = prop, label = percent(prop, 1)),
              color = grey10k, size = 2,
              hjust = 1.1, vjust = .5) +
    scale_color_identity() +
    scale_fill_manual(values = c(genoa, grey20k)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(x = "", y = "") +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(face = "bold"))

  si_save(filename = file.path(dir_graphics, "NIGERIA - FY21 PMTCT HIV Testing Bars Plot.png"),
          plot = last_plot())


  df_psnu_stat_pos_viz <- df_psnu_stat %>%
    filter(metric %in% c("stat_np", "stat_pos")) %>%
    group_by(psnu) %>%
    mutate(total = sum(value, na.rm = T),
           pos = value[metric == "stat_pos"],
           prop = pos / total,
           label = paste0("<span style='color: ",
                          ifelse(psnu %in% usaid_psnus, usaid_blue,
                                 ifelse(psnu %in% cdc_psnus, usaid_lightblue,
                                        usaid_darkgrey)),"';>",
                          psnu,
                          " (", comma(pos),
                          ")</span>")) %>%
    ungroup()

  bars_pmtct_stat_pos <- df_psnu_stat_pos_viz %>%
    ggplot(aes(reorder(label, prop), value, fill = metric)) +
    geom_col(position = position_fill(reverse = T), show.legend = F) +
    geom_text(data = df_psnu_stat_pos_viz %>% filter(metric == "stat_pos"),
              aes(x = reorder(label, prop), y = prop, label = percent(prop, 1)),
              color = usaid_black, size = 2.5, fontface = "bold",
              hjust = -1.1, vjust = .5) +
    scale_color_identity() +
    scale_fill_manual(values = c(burnt_sienna, grey20k)) +
    scale_y_continuous(expand = c(0, 0), labels = percent) +
    coord_flip() +
    labs(x = "", y = "") +
    si_style_xgrid() +
    theme(axis.text.y = element_markdown(face = "bold"))

  bars_pmtct_stat_pos

  si_save(filename = file.path(dir_graphics, "NIGERIA - FY21 PMTCT HIV Positive Bars Plot.png"),
          plot = last_plot())


  (map_pmtct_stat + map_pmtct_stat_pos) / bars_pmtct_stat


