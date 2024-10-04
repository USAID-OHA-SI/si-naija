# PROJECT: si-naija
# PURPOSE: Mapping HIV Prevalence and Testing Saturation
# AUTHOR: Baboyma Kagniniwa | USAID/GH - Office of HIV-AIDS
# LICENSE: MIT
# REF. ID: a14ec3e1
# CREATED: 2024-10-01
# UPDATED: 2024-10-01
# NOTES:

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(gagglr)
  library(grabr)
  library(gisr)
  library(sf)
  library(glitr)
  library(tidytext)
  library(ggtext)
  library(scales)
  library(patchwork)
  library(systemfonts)
  library(glue)
  library(janitor)

  source("./Scripts/N00_Util_VizBivariables.R")

# Set paths  ====

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_datim <- glamr::si_path("path_datim")

# Params

  ref_id <- "a14ec3e1"
  ou <-  "Nigeria"
  cntry <- ou
  agency <- "USAID"

# FILES

  file_nat <- dir_mer %>%
    return_latest(glue("NAT_SUBNAT_FY22.*.zip$"))

  meta <- get_metadata(file_nat)

# Functions  ====

  #' @title Remove Layer
  #'
  #'
  geom_remove <- function(.plot, geomClass = "GeomText"){

    if ("ggplot" %in% class(.plot) & !is.null(.plot["layers"])) {

      # Extract layers class
      .target_layers <- map_chr(.plot$layers, ~first(class(.x$geom)))

      # Identify target layers
      .target_layers <- which(.target_layers == geomClass)

      # delete target layers
      if (length(.target_layers) > 0) {
        usethis::ui_info("{geomClass} layer(s) found at index {.target_layers}")
        .plot$layers[.target_layers] <- NULL
      }
      else {
        usethis::ui_info("No {geomClass} layer found.")
      }
    }
    else {
      usethis::ui_stop("Invalid object - use a ggplot object.")
    }

    .plot
  }

# LOAD DATA ====

  ## MER - NatSUbnat

  df_nat <- file_nat %>% read_psd()

  cntry_uid <- df_nat %>%
    filter(operatingunit == cntry,
           fiscal_year == last(fiscal_year)) %>%
    distinct(operatingunit, operatingunituid) %>%
    pull(operatingunituid)

  ## Spatial datasets

  ras <- dir_ras %>%  get_raster()

  spdf_pepfar <- dir_shp %>% get_vcpolygons()

  ## Org hierarchy * Geodata

  df_psnus <- df_nat %>%
    filter(fiscal_year == min(fiscal_year),
           country == cntry) %>%
    distinct(psnuuid, psnu)

  spdf_cntry <- spdf_pepfar %>%
    filter(uid == cntry_uid)

  spdf_psnu <- spdf_pepfar %>%
    left_join(df_psnus, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(psnu))

# MUNGE ====

  df_nat %>%
    filter(fiscal_year == min(fiscal_year)) %>%
    distinct(indicator) %>%
    pull()

  ## Extract POP, PLHIV and ART data
  df_pop <- df_nat %>%
    filter(fiscal_year == min(fiscal_year),
           country == cntry,
           indicator %in% c("POP_EST", "PLHIV", "TX_CURR_SUBNAT"),
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/HIVStatus")
           ) %>%
    summarise(across(targets, ~sum(.x, na.rm=T)),
              .by = c(fiscal_year, country, psnuuid, psnu,
                      indicator, standardizeddisaggregate)) %>%
    group_by(fiscal_year, country, psnuuid, psnu, indicator) %>%
    arrange(standardizeddisaggregate) %>%
    filter(standardizeddisaggregate == first(standardizeddisaggregate)) %>%
    ungroup() %>%
    select(-standardizeddisaggregate) %>%
    pivot_wider(names_from = indicator, values_from = targets) %>%
    rename_with(str_to_lower)

  ## Compute prevalence and ART Saturation (set max saturation to 100%)

  df_tx <- df_pop %>%
    mutate(
      prev = plhiv / pop_est,
      sat = tx_curr_subnat / plhiv,
      sat = case_when(
        sat > 1 ~ 1,
        TRUE ~ sat
      )
    )

  q_lbls <- c("Low", "Mid", "High")
  q_lbls2 <- paste0("Q", 1:4)
  q_prev <- df_tx %>% var_quantiles(name = prev, nclass = 3)
  q_sat <- df_tx %>% var_quantiles(name = sat, nclass = 3)

  df_tx <- df_tx %>%
    mutate(
      prev_q = cut(prev,
                   breaks = q_prev,
                   labels = F,
                   include.lowest = T,
                   na.rm = T),
      sat_q = cut(sat,
                  breaks = q_sat,
                  labels = F,
                  include.lowest = T,
                  na.rm = T)
    ) %>%
    rowwise() %>%
    mutate(
      key = case_when(
        any(is.na(c(prev_q, sat_q))) ~ NA_character_,
        TRUE ~ paste0(prev_q, "-", sat_q)
      )
    )

  ## Compute colors

  pal_prev <- c("#ffd4ac", "#d56d4b", "#923417") # burnt_siennas
  pal_sat <- c("#89dacb", "#459688", "#004137") # genoas

  df_colors <- get_bivariate_colors(cols1 = pal_prev, cols2 = pal_sat)

  df_tx <- df_tx %>%
    left_join(df_colors, by = "key") %>%
    rename_with(~str_replace(.x, "1$", "_prev")) %>%
    rename_with(~str_replace(.x, "2$", "_sat"))

  spdf_tx <- spdf_psnu %>%
    left_join(df_tx, by = c("uid" = "psnuuid", "psnu"))

# VIZ ====

  viz_prev_distro <- df_tx %>%
    ggplot(aes(x = prev)) +
    geom_density(fill = burnt_sienna_light,
                 color = burnt_sienna,
                 linewidth = 1,
                 alpha = .6) +
    #geom_histogram(binwidth = .001, fill = grey10k, color = burnt_sienna) +
    geom_vline(xintercept = mean(df_tx$prev, na.rm=T), color = burnt_sienna, linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = median(df_tx$prev, na.rm=T), color = burnt_sienna, linewidth = 1) +
    scale_x_continuous(labels = percent) +
    labs(x = "", y = "") +
    si_style()

  viz_sat_distro <- df_tx %>%
    ggplot(aes(x = sat)) +
    geom_density(fill = genoa_light, color = genoa, linewidth = 1, alpha = .6) +
    #geom_histogram(binwidth = .02, fill = grey10k, color = genoa) +
    geom_vline(xintercept = mean(df_tx$sat, na.rm=T), color = genoa, linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = median(df_tx$sat, na.rm=T), color = genoa, linewidth = 1) +
    scale_x_continuous(labels = percent) +
    labs(x = "", y = "") +
    si_style()

  viz_prev_sat <- df_tx %>%
    ggplot(aes(prev, sat, size = plhiv, fill = sat)) +
    geom_smooth(
      formula = "y ~ x", method = "loess", orientation = "x",
      color = usaid_red, linewidth = 1, fill = grey20k, show.legend = F) +
    geom_point(shape = 21, stroke = .5, color = grey10k) +
    scale_size_continuous(range = c(3, 30)) +
    scale_fill_si(palette = "genoas", reverse = F, labels = percent) +
    scale_x_continuous(labels = percent,
                       breaks = seq(0, max(df_tx$prev, na.rm = T), .005),
                       limits = c(0, max(df_tx$prev, na.rm = T))) +
    scale_y_continuous(labels = percent,
                       breaks = seq(round(min(df_tx$sat, na.rm = T), 1) -.1, 1, .1)) +
    guides(size = "none") +
    coord_cartesian() +
    labs(x = "HIV Prevalence (Position)", y = "ART Saturation (Color)") +
    si_style() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(1, "in"))

  ((viz_prev_distro + viz_sat_distro) / viz_prev_sat )


  ## Bivariate

  legend_key <- df_colors %>% get_bivariate_legend()

  viz_b_prev_sat <- df_tx %>%
    ggplot(aes(prev, sat, size = plhiv, fill = value)) +
    geom_smooth(color = usaid_red, linewidth = 1, fill = grey20k, show.legend = F) +
    geom_point(shape = 21, stroke = .5, color = grey10k) +
    geom_text(data = df_tx %>% filter(prev > .02),
              aes(label = comma(plhiv)), size = 3, color = grey10k) +
    scale_size_continuous(range = c(3, 30)) +
    scale_fill_identity() +
    scale_x_continuous(labels = percent,
                       breaks = seq(0, max(df_tx$prev, na.rm = T), .005),
                       limits = c(0, max(df_tx$prev, na.rm = T))) +
    scale_y_continuous(labels = percent,
                       breaks = seq(round(min(df_tx$sat, na.rm = T), 1) -.1, 1, .1)) +
    guides(size = "none") +
    labs(x = "HIV Prevalence (Position)", y = "ART Saturation (Color)") +
    si_style() +
    theme(legend.key.width = unit(1, "in"))

  viz_b_prev_sat <- ggdraw(viz_b_prev_sat) +
    cowplot::draw_plot(
      plot = legend_key,
      x = .75, y = .05,
      width = .2, height = .2)

  viz_b_plot <- ((viz_prev_distro + viz_sat_distro) / viz_b_prev_sat)

  viz_b_plot

  si_save(plot = viz_b_plot,
          filename = file.path(dir_graphics, glue::glue("{cntry} - Prevelance based ART Saturation.png")),
          scale = 1.5,
          dpi = 350)


  ## Maps

  ## Basemap

  bmap <- terrain_map(countries = cntry,
                      adm0 = spdf_cntry,
                      adm1 = spdf_psnu,
                      terr = ras,
                      mask = T)

  ## Prevalence

  map_prev <-
    bmap +
    geom_sf(data = spdf_tx, aes(fill = prev), linewidth = .3, color = grey30k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = 1.5, color = grey90k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = .8, color = grey10k) +
    geom_sf_text(data = spdf_psnu, aes(label = psnu), color = grey50k) +
    scale_fill_si(palette = "burnt_siennas", na.value = "NA",
                  labels = percent, limits = c(0, max(spdf_tx$prev, na.rm = T))) +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(.06, "npc"))

  map_prev

  ## Remove labels from plot
  geom_remove(map_prev, "GeomText")

  map_prev_q <-
    bmap +
    geom_sf(data = spdf_tx %>%
              filter(!is.na(prev)) %>%
              mutate(prev_g = cut(prev, breaks = q_prev, labels = q_lbls,
                                  dig.lab = 2, include.lowest = T)),
            aes(fill = prev_g), linewidth = .3, color = grey30k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = 1.2, color = grey90k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = .8, color = grey10k) +
    geom_sf_text(data = spdf_psnu, aes(label = psnu), color = grey50k) +
    scale_fill_manual(
      values = pal_prev, na.value = NA,
      guide = guide_legend(
        theme = theme(
          legend.text.position = "bottom",
          legend.text = element_text(hjust = .5),
          legend.key.spacing = unit(0, "npc"),
          legend.key.width = unit(.06, "npc"))
      )
    ) +
    si_style_map() +
    theme(legend.title = element_blank())

  map_prev_q

  geom_remove(map_prev, "GeomText") + geom_remove(map_prev_q, "GeomText")

  si_save(filename = file.path(dir_graphics, glue::glue("{cntry} - Prevelance maps.png")),
          scale = 1.5,
          dpi = 350)

  ## ART Saturation
  map_sat <-
    bmap +
    geom_sf(data = spdf_tx, aes(fill = sat), linewidth = .3, color = grey10k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = 1.2, color = grey90k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = .8, color = grey10k) +
    geom_sf_text(data = spdf_psnu, aes(label = psnu), color = grey50k) +
    scale_fill_si(palette = "genoas", na.value = "NA",
                  labels = percent, limits = c(min(spdf_tx$sat, na.rm = T), 1)) +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(.06, "npc"))

  map_sat_q <-
    bmap +
    geom_sf(data = spdf_tx %>%
              filter(!is.na(sat)) %>%
              mutate(sat_g = cut(sat, breaks = q_sat, labels = q_lbls,
                                  dig.lab = 2, include.lowest = T)),
            aes(fill = sat_g), linewidth = .3, color = grey10k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = 1.2, color = grey90k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = .8, color = grey10k) +
    geom_sf_text(data = spdf_psnu, aes(label = psnu), color = grey50k) +
    scale_fill_manual(
      values = pal_sat, na.value = NA,
      guide = guide_legend(
        theme = theme(
          legend.text.position = "bottom",
          legend.text = element_text(hjust = .5),
          legend.key.spacing = unit(0, "in")))
    ) +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(.06, "npc"))

  map_sat_q

  geom_remove(map_sat, "GeomText") + geom_remove(map_sat_q, "GeomText")

  si_save(filename = file.path(dir_graphics, glue::glue("{cntry} - ART Saturation maps.png")),
          scale = 1.5,
          dpi = 350)

  ## Prev x Sat
  map_prev_sat <-
    bmap +
    geom_sf(data = spdf_tx, aes(fill = value), linewidth = .3, color = grey10k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = 1.2, color = grey90k) +
    geom_sf(data = spdf_cntry, fill = NA, linewidth = .8, color = grey10k) +
    geom_sf_text(data = spdf_psnu, aes(label = psnu), color = grey50k) +
    #scale_fill_si(palette = "genoas", na.value = "NA", labels = percent) +
    scale_fill_identity() +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(.2, "npc"))

  map_prev_sat <- ggdraw(map_prev_sat) +
    cowplot::draw_plot(
      plot = legend_key,
      x = .5, y = .02,
      width = .2, height = .2)

  si_save(plot = map_prev_sat,
          filename = file.path(dir_graphics, glue::glue("{cntry} - Prevalence & ART Saturation maps.png")),
          scale = 1.5,
          dpi = 350)

  ## Summary Map
  map_tx <- map_prev + map_sat + map_prev_sat
  map_tx2 <- (map_prev_q + map_sat_q) / map_prev_sat

# EXPORT ====

