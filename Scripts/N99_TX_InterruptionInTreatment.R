##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: TX_ML [% of patients transferring out]
##  LICENCE: MIT
##  UPDATE:  2021-06-29

# Dependancies----------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(ICPIutilities)
  library(sf)
  library(here)
  library(glamr)
  library(glitr)
  library(gisr)
  library(scales)
  library(tidytext)
  library(ggtext)
  library(RColorBrewer)
  library(patchwork)
  library(extrafont)
  library(glue)

  source("../lastmile/Scripts/00_Geo_Utilities.R")


# GLOBALS -------------------------------------------------------------

  ## Data & Output folders
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"
  dir_geodata <- si_path("path_vector")
  dir_merdata <- si_path("path_msd")
  dir_terr <- si_path("path_raster")

  ## Reporting Filters
  rep_agency = "USAID"
  rep_agencies <- c("USAID", "HHS/CDC")

  cntry <- "Nigeria"

  ml_colors <- c(genoa, moody_blue, golden_sand, old_rose)

  rep_fy <- 2021

  rep_qtr <- 2

  rep_fy2 <- rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_fys <- c(rep_fy - 1, rep_fy)

  rep_fys2 <- rep_fys %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_pd <- rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q", rep_qtr)

  rep_ref_pd <- rep_fys %>%
    first() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q4")

  rep_init_pd <- rep_fys %>%
    first() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q1")

  rep_pds <- c(rep_ref_pd, rep_pd)
  rep_pds2 <- c(rep_init_pd, rep_pd)

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_\\d{1}_N.*.zip$",
    recursive = FALSE
  )

  # MSD File version
  msd_version <- ifelse(str_detect(file_psnu_im, ".*_\\d{8}_v1_\\d"), "i", "c")

  # Shapefile path
  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons.*.shp",
    recursive = TRUE
  )

# FUNCTIONS -------------------------------------------------------------

  #' Extract TX_ML from MER PSNU Dataset
  #'
  #' @param fy fiscal year
  #' @param snu_prio snuprioritization
  #'
  extract_tx_ml <- function(.data,
                            fy = "2021",
                            agency = "USAID",
                            snu_prio = NULL,
                            mechs = NULL) {

    ## For ZAF Only
    if (!is.null(snu_prio)) {
      .data %>%
        filter(snuprioritization %in% snu_prio)
    }

    if (!is.null(mechs)) {
      .data %>%
        filter(mech_code %in% mechs)
    }

    ## Common Munging
    .data %>%
      filter(
        fiscal_year %in% {{fy}},
        indicator == "TX_ML",
        standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus",
        typemilitary == 'N',
        fundingagency == agency
      ) %>%
      mutate(
        otherdisaggregate = str_remove(otherdisaggregate, "No Contact Outcome - "),
        otherdisagg = ifelse(str_detect(otherdisaggregate, "Interruption"), "Interruption in Treatment", otherdisaggregate),
        otherdisagg = ifelse(str_detect(otherdisagg, "Refused"), "Refused or Stopped", otherdisagg),
        otherdisagg = factor(otherdisagg,
                             levels = c("Transferred Out", "Interruption in Treatment", "Refused or Stopped", "Died"),
                             labels = c("TO", "IIT", "Refused or Stopped", "Died"))
      ) %>%
      group_by(operatingunit, snu1, snu1uid,
               psnu, psnuuid, indicator, otherdisagg) %>%
      summarize_at(vars(cumulative), sum, na.rm = TRUE) %>%
      ungroup() %>%
      # fill in missing options for otherdisagg & and replace NA with zeros
      complete(psnuuid, nesting(indicator, otherdisagg)) %>%
      replace_na(list(cumulative = 0)) %>%
      # Fill in missing orgs info
      group_by(psnuuid) %>%
      mutate(across(c(operatingunit, snu1uid, snu1, psnu), ~ first(.x[!is.na(.x)]))) %>%
      ungroup() %>%
      group_by(operatingunit, snu1uid, snu1, psnuuid, psnu) %>%
      dplyr::mutate(
        ml_ttl = sum(cumulative, na.rm = T),
        prct = cumulative / ml_ttl
      ) %>%
      ungroup()
  }

  #' Extract TX_ML from MER PSNU Dataset
  #'
  #' @param fys      fiscal years
  #' @param agency   Funding Agency(ies)
  #' @param snu_prio snuprioritization
  #' @param mechs    Mech codes
  #'
  extract_tx_ml_trend <- function(df,
                                  fys = c(2020, 2021),
                                  agency = "USAID",
                                  snu_prio = NULL,
                                  mechs = NULL) {

    ## For ZAF Only
    if (!is.null(snu_prio)) {
      df <- df %>%
        filter(snuprioritization %in% snu_prio)
    }

    # filter mechs
    if (!is.null(mechs)) {
      df <- df %>%
        filter(mech_code %in% mechs)
    }

    # filter period, target and disaggs
    df <- df %>%
      filter(
        fiscal_year %in% {{fys}},
        indicator == "TX_ML",
        standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus",
        typemilitary == 'N')

    if (!is.null(agency)) {
      df <- df %>%
        filter(fundingagency %in% {{agency}})
    }

    # Rest of munging calculation
    df <- df %>%
      mutate(
        otherdisaggregate = str_remove(
          otherdisaggregate, "No Contact Outcome - "),
        otherdisagg = ifelse(str_detect(otherdisaggregate, "Interruption"),
                             "Interruption in Treatment", otherdisaggregate),
        otherdisagg = ifelse(str_detect(otherdisagg, "Refused"),
                             "Refused or Stopped", otherdisagg),
        otherdisagg = factor(otherdisagg,
                             levels = c("Transferred Out",
                                        "Interruption in Treatment",
                                        "Refused or Stopped",
                                        "Died"),
                             labels = c("TO", "IIT", "Refused or Stopped", "Died"))
      ) %>%
      reshape_msd(direction = "long", clean = TRUE) %>%
      filter(period_type == "results") %>%
      group_by(operatingunit, countryname, snu1, snu1uid,
               psnu, psnuuid, indicator, otherdisagg, period) %>%
      summarize_at(vars(value), sum, na.rm = TRUE) %>%
      ungroup() %>%
      group_by(operatingunit, countryname, snu1, snu1uid,
               psnu, psnuuid, indicator, period) %>%
      dplyr::mutate(
        ml_ttl = sum(value, na.rm = T),
        ml_prct = value / ml_ttl
      ) %>%
      ungroup()
  }

  #' Create a bar graph of % TO
  #'
  #' @param df Summarized country level TX_ML Data
  #' @param org_level snu1 or psnu
  #'
  plot_tx_ml <- function(df,
                         org_level = "psnu",
                         disagg = NULL,
                         fcolor = NULL,
                         add_subtitle = FALSE) {

    df <- df %>%
      group_by(operatingunit, snu1uid, snu1, psnuuid, psnu) %>%
      mutate(ml_grp = cumulative[otherdisagg == disagg],
             ml_grp_prct = ml_grp / ml_ttl) %>%
      ungroup() %>%
      mutate(label = paste0(psnu, " (", comma(ml_grp, 1), "/", comma(ml_ttl, 1), ")"))

    # Reorder by disagg
    viz <- df %>%
      arrange(label, ml_grp_prct) %>%
      ggplot(aes(x = reorder(label, ml_grp_prct),
                 y = prct,
                 fill = otherdisagg))

    # Create the viz
    viz <- viz +
      geom_col(position = position_fill(reverse = TRUE), alpha = .8) +
      geom_hline(yintercept = .25, color = grey10k, lwd = .3) +
      geom_hline(yintercept = .50, color = grey10k, lwd = .3) +
      geom_hline(yintercept = .75, color = grey10k, lwd = .3)

    # sepecifile color
    if (is.null(fcolor)) {
      viz <- viz +
        scale_fill_brewer(palette = "Set3", direction = -1)
    } else {
      viz <- viz +
        scale_fill_manual(values = fcolor)
    }

    viz <- viz  +
      scale_y_continuous(position = "right", labels = percent) +
      #scale_x_reordered() +
      coord_flip(expand = F, clip = "off")

    # add subtitle
    if (add_subtitle == TRUE) {
      viz <- viz +
        labs(x = "", y = "",
             subtitle = glue("{str_to_upper(org_level)} ({str_to_upper(disagg)} / TX_ML)"))
    }
    else {
      viz <- viz +
        labs(x = "", y = "")
    }

    # add theme
    viz <- viz  +
      si_style() +
      theme(
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8)
      )

    return(viz)
  }


  #' Map % TO
  #'
  #' @param df country dataset
  #' @param df_shp country geodata
  #' @param label_name colname to be used for labels
  #' @param uid_name colname foreign key from df
  #'
  map_tx_ml <- function(df, spdf, terr,
                        country = "Nigeria",
                        disagg = NULL,
                        label_name = "psnu",
                        uid_name = "psnuuid") {

    # OU
    print(str_to_upper(country))

    # Join and filter data
    spdf_tx_ml <- spdf %>%
      filter(countryname == {{country}}) %>%
      left_join(df, by = c("uid" = {{uid_name}}, "operatingunit")) %>%
      dplyr::filter(!is.na(otherdisagg)) %>%
      mutate(prct_color = if_else(prct <= .3, glitr::grey80k, glitr::grey10k))

    # print(spdf_tx_ml %>% glimpse())

    if (!is.null(disagg)) {
      spdf_tx_ml <- spdf_tx_ml %>%
        filter(otherdisagg == {{disagg}})
    }

    # Basemap
    admin0 <- spdf %>%
      filter(operatingunit == {{country}}, label == "country")

    admin1 <- spdf %>%
      filter(operatingunit == {{country}}, label == "snu1")

    basemap <- terrain_map(countries = admin0,
                           adm0 = admin0,
                           adm1 = admin1,
                           mask = TRUE,
                           terr = terr)

    # Viz
    gviz <- basemap +
      geom_sf(data = spdf_tx_ml, aes(fill = prct), color = grey10k, alpha = .7) +
      geom_sf(data = admin0, fill = NA, size = 1.5, color = grey10k) +
      geom_sf(data = admin0, fill = NA, size = .3, color = grey90k)

    # Add label
    if (!is.null(label_name)) {
      gviz <- gviz +
        geom_sf_text(data = spdf_tx_ml,
                     aes(label = paste0(psnu, "\n(", percent(prct, 1), ")"),
                         color = prct_color),
                     size = 2) +
        scale_color_identity()
    }

    # Apply fill color
    gviz <- gviz +
      scale_fill_si(palette = "burnt_siennas",
                    discrete = F,
                    limits = c(0, 1),
                    labels = percent,
                    na.value = NA,
                    alpha = .8)

    # facet if needed
    if (is.null(disagg)) {
      gviz <- gviz +
        facet_wrap(~otherdisagg, ncol = 2)
    }

    # Apply SI Style theme
    gviz <- gviz  +
      si_style_map() +
      theme(
        strip.text = element_text(family = "Source Sans Pro", face = "bold"),
        plot.title = element_text(family = "Source Sans Pro", face = "bold", color = grey80k),
        plot.subtitle = element_text(family = "Source Sans Pro", margin = unit(c(1,1,10,1), 'pt')),
        plot.caption = element_text(family = "Source Sans Pro", face = "italic")
      )

    return(gviz)
  }


  #' Combine Map + Graph
  #'
  #' @param cntry_plot bar chart
  #' @param cntry_map map
  #' @param title graphic title
  #' @param caption graphic footnote
  #'
  viz_tx_ml <- function(cntry_plot, cntry_map,
                        title = "<COUNTRY XYZ - Descriptive Title>",
                        caption = "SI/Core Analytic Cluster") {

    viz_output <- cntry_map + cntry_plot +
      plot_layout(ncol = 2, widths = c(2, 1)) +
      plot_annotation(
        title = str_to_upper(title),
        caption = paste0("OHA/SIEI - ", caption),
        theme = theme(plot.title = element_text(family = "Source Sans Pro", hjust = .5))
      )

    return(viz_output)
  }


# DATA ----

  ## PSNUxIMs
  df_psnu <- file_psnu_im %>% read_msd()

  df_psnu %>%
    filter(indicator == "TX_ML") %>%
    distinct(fiscal_year, standardizeddisaggregate, otherdisaggregate) %>%
    arrange(desc(fiscal_year), standardizeddisaggregate) %>%
    prinf()

  ## Raster data
  terr <- gisr::get_raster(terr_path = dir_terr)

  ## PEPFAR Boundaries
  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- get_attributes(country = cntry)

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(uid))

# MUNGE ----

  ## Munge TX_ML
  df_tx_ml_usaid <- df_psnu %>%
    extract_tx_ml(fy = rep_fy) %>%
    clean_psnu()

  df_tx_ml_cdc <- df_psnu %>%
    extract_tx_ml(fy = rep_fy, agency = "HHS/CDC") %>%
    clean_psnu()

  df_tx_ml_usaid_trend <- df_psnu %>%
    extract_tx_ml_trend(fys = rep_fys, agency = rep_agency) %>%
    clean_psnu()

  df_tx_ml_cdc_trend <- df_psnu %>%
    extract_tx_ml_trend(fys = rep_fys, agency = "HHS/CDC") %>%
    clean_psnu()


# VIZ ----

  caption <- glue("Source: {rep_pd}{msd_version} MSD, Produced by OHA/SIEI on {format(Sys.Date(), '%Y-%m-%d')}")

  ## USAID
  df_tx_ml_usaid %>%
    distinct(otherdisagg) %>%
    pull() %>%
    map(function(ml_disagg) {
      print(ml_disagg)

      ttl_ml_disagg <- df_tx_ml_usaid %>%
        filter(otherdisagg == {{ml_disagg}}) %>%
        pull(cumulative) %>%
        sum()

      ttl_ml <- df_tx_ml_usaid %>%
        pull(cumulative) %>%
        sum()

      plot = plot_tx_ml(df = df_tx_ml_usaid,
                        disagg = ml_disagg,
                        fcolor = ml_colors)

      viz <- plot +
        plot_annotation(
          title = str_to_upper(glue("USAID/{cntry} - ART PATIENTS WITH NO CLINICAL CONTACT")),
          subtitle = glue("PSNU (# of {ml_disagg} / TX_ML) - States are sorted by % {ml_disagg} of TX_ML Total\n{percent(ttl_ml_disagg / ttl_ml, 1)} {ml_disagg} ({comma(ttl_ml_disagg, 1)}) out of {comma(ttl_ml, 1)}"),
          caption = caption,
          theme = theme(plot.title = element_text(family = "Source Sans Pro",
                                                  color = color_title,
                                                  size = 14, hjust = 0.5),
                        plot.subtitle = element_text(family = "Source Sans Pro",
                                                     color = color_title,
                                                     size = 12, hjust = 0.5),
                        plot.caption = element_text(family = "Source Sans Pro",
                                                    color = color_title,
                                                    face = "italic",
                                                    size = 7, hjust = 1)))

      ggsave(here(dir_graphics, glue("{rep_pd} - {str_to_upper(cntry)}_USAID_TX_ML_{ml_disagg}_plot_{format(Sys.Date(), '%Y%m%d')}.png")),
             plot = viz,
             scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

    })

  map = map_tx_ml(df = df_tx_ml_usaid,
                  spdf = spdf_pepfar,
                  terr = terr,
                  country = cntry,
                  label_name = "psnu")

  viz <- map +
    plot_annotation(
      title = str_to_upper(glue("USAID/{cntry} - TX_ML (ALL)")),
      caption = caption,
      theme = theme(plot.title = element_text(family = "Source Sans Pro",
                                              color = color_title,
                                              size = 14, hjust = .5),
                    plot.caption = element_text(family = "Source Sans Pro",
                                                color = color_title,
                                                face = "italic",
                                                size = 7, hjust = 1))
    )

  ggsave(here(dir_graphics, glue("{rep_pd} - {str_to_upper(cntry)}_USAID_TX_ML_ALL_Map_{format(Sys.Date(), '%Y%m%d')}.png")),
         plot = viz,
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


  ## CDC
  df_tx_ml_cdc %>%
    distinct(otherdisagg) %>%
    pull() %>%
    map(function(ml_disagg) {
      print(ml_disagg)

      ttl_ml_disagg <- df_tx_ml_cdc %>%
        filter(otherdisagg == {{ml_disagg}}) %>%
        pull(cumulative) %>%
        sum()

      ttl_ml <- df_tx_ml_cdc %>%
        pull(cumulative) %>%
        sum()

      plot = plot_tx_ml(df = df_tx_ml_cdc,
                        disagg = ml_disagg,
                        fcolor = ml_colors)

      viz <- plot +
        plot_annotation(
          title = str_to_upper(glue("CDC/{cntry} - ART PATIENTS WITH NO CLINICAL CONTACT")),
          subtitle = glue("PSNU (# of {ml_disagg} / TX_ML) - States are sorted by % {ml_disagg} of TX_ML Total\n{percent(ttl_ml_disagg / ttl_ml, 1)} {ml_disagg} ({comma(ttl_ml_disagg, 1)}) out of {comma(ttl_ml, 1)}"),
          caption = caption,
          theme = theme(plot.title = element_text(family = "Source Sans Pro",
                                                  color = color_title,
                                                  size = 14, hjust = 0.5),
                        plot.subtitle = element_text(family = "Source Sans Pro",
                                                     color = color_title,
                                                     size = 12, hjust = 0.5),
                        plot.caption = element_text(family = "Source Sans Pro",
                                                    color = color_title,
                                                    face = "italic",
                                                    size = 7, hjust = 1)))

      ggsave(here(dir_graphics, glue("{rep_pd} - {str_to_upper(cntry)}_CDC_TX_ML_{ml_disagg}_plot_{format(Sys.Date(), '%Y%m%d')}.png")),
             plot = viz,
             scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

    })

  map = map_tx_ml(df = df_tx_ml_cdc,
                  spdf = spdf_pepfar,
                  terr = terr,
                  country = cntry,
                  label_name = "psnu")

  viz <- map +
    plot_annotation(
      title = str_to_upper(glue("CDC/{cntry} - TX_ML (ALL)")),
      caption = caption,
      theme = theme(plot.title = element_text(family = "Source Sans Pro",
                                              color = color_title,
                                              size = 14, hjust = .5),
                    plot.caption = element_text(family = "Source Sans Pro",
                                                color = color_title,
                                                face = "italic",
                                                size = 7, hjust = 1))
    )

  ggsave(here(dir_graphics, glue("{rep_pd} - {str_to_upper(cntry)}_CDC_TX_ML_ALL_Map_{format(Sys.Date(), '%Y%m%d')}.png")),
         plot = viz,
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")



## STOP HERE ----

  # USAID -
  ## Nigeria Specific data
  df_cntry <- df_tx_ml %>%
    filter(operatingunit == cntry)

  # PSNU level trends
  df_cntry_trend <- df_tx_ml_trend %>%
    filter(operatingunit == cntry) %>%
    mutate(label = paste0(psnu, " (", percent(ml_prct), ")"))

  # OU level trends
  df_ou_trend <- df_cntry_trend %>%
    group_by(countryname, otherdisagg, period) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    group_by(countryname, period) %>%
    mutate(ml_ttl = sum(value, na.rm = TRUE),
           ml_prct = value / ml_ttl) %>%
    ungroup()

  # Viz- OU AreaPlot
  viz_ou_ml <- df_ou_trend %>%
    ggplot(aes(x = period,
               y = ml_prct,
               group = otherdisagg,
               fill = otherdisagg,
               color = otherdisagg,
               label = comma(value, 1))
               ) +
    geom_area(show.legend = FALSE) +
    geom_line(size = 2, color = grey10k, show.legend = FALSE) +
    geom_line(size = 1, show.legend = FALSE) +
    geom_text(data = df_ou_trend %>% filter(period %in% rep_pds2),
              vjust = -.3, size = 3, color = usaid_darkgrey,
              position = position_nudge(y = 0.01)) +
    scale_fill_manual(values = ml_colors) +
    scale_color_manual(values = ml_colors) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    facet_wrap(~otherdisagg, nrow = 1) +
    labs(x = "", y = "",
         title = str_to_upper(glue("USAID TX_ML TREND, {rep_ref_pd} to {rep_pd}")),
         caption = glue("{str_to_upper(cntry)} - {caption}")) +
    si_style_ygrid() +
    theme(axis.text = element_text(size = 6),
          legend.title = element_blank(),
          strip.text = element_text(hjust = .5))

  ggsave(here(dir_graphics,
              glue("{rep_pd} - {str_to_upper(cntry)}_USAID_TX_ML_OU_Trend_AreaPlot_{format(Sys.Date(), '%Y%m%d')}.png")),
         plot = viz_ou_ml,
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

  # PSNU AreaPlot
  df_cntry_trend %>%
    distinct(countryname, psnu) %>%
    pull(psnu) %>%
    map(function(psnu){

      print(psnu)

      viz_trend <- df_cntry_trend %>%
        filter(psnu == {{psnu}}) %>%
        ggplot(aes(x = period,
                   y = ml_prct,
                   group = otherdisagg,
                   fill = otherdisagg,
                   color = otherdisagg,
                   label = comma(value, 1))
        ) +
        geom_area(show.legend = FALSE) +
        geom_line(size = 2, color = grey10k, show.legend = FALSE) +
        geom_line(size = 1, show.legend = FALSE) +
        geom_text(data = df_ou_trend %>% filter(period %in% rep_pds2),
                  vjust = -.3, size = 3, color = usaid_darkgrey,
                  position = position_nudge(y = 0.01)) +
        scale_fill_manual(values = ml_colors) +
        scale_color_manual(values = ml_colors) +
        scale_y_continuous(labels = percent, limits = c(0, 1)) +
        facet_wrap(~otherdisagg, nrow = 1) +
        labs(x = "", y = "",
             title = str_to_upper(glue("{psnu} - TX_ML TREND, {rep_ref_pd} to {rep_pd}")),
             caption = glue("{str_to_upper(cntry)} / {str_to_upper(psnu)} - {caption}")) +
        si_style_ygrid() +
        theme(axis.text = element_text(size = 6),
              legend.title = element_blank(),
              strip.text = element_text(hjust = .5))

      ggsave(here(dir_graphics,
                  glue("{rep_pd} - {str_to_upper(cntry)}_{str_to_upper(psnu)}_USAID_TX_ML_OU_Trend_AreaPlot_{format(Sys.Date(), '%Y%m%d')}.png")),
             plot = viz_trend,
             scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

      return(viz_trend)
    })

  # All
  ml_plot = plot_tx_ml(df = df_cntry, disagg = "to", fcolor = ml_colors)
  ml_map = map_tx_ml(df_cntry, spdf_pepfar, terr, cntry)

  viz_ml <- ml_map + ml_plot +
    plot_layout(ncol = 2, widths = c(2, 1)) +
    plot_annotation(
      title = str_to_upper(glue("{cntry} - TX_ML IIT INTERRUPTION IN TREATMENT (ALL)")),
      caption = caption,
      theme = theme(plot.title = element_text(family = "Source Sans Pro", hjust = .5))
    )

  ggsave(here(dir_graphics,
              glue("{rep_pd} - {str_to_upper(cntry)}_TX_ML_InterruptionInTreatment_{format(Sys.Date(), '%Y%m%d')}.png")),
         plot = viz_ml,
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

  #c("TO", "ITT", "Refused or Stopped", "Died")

  # TO Only
  to_plot = plot_tx_ml(df = df_cntry, disagg = "to", fcolor = ml_colors)
  to_map = map_tx_ml(df_cntry, spdf_pepfar, terr, cntry, disagg = "TO")

  viz_tx_ml <- to_map + to_plot +
    plot_layout(ncol = 2, widths = c(2, 1)) +
      plot_annotation(
        title = str_to_upper(glue("{cntry} - TX_ML IIT Transferred out")),
        caption = caption,
        theme = theme(plot.title = element_text(family = "Source Sans Pro", hjust = .5))
      )

  ggsave(here(dir_graphics,
              glue("{rep_pd} - {str_to_upper(cntry)}_TX_ML_TransferredOut_{format(Sys.Date(), '%Y%m%d')}.png")),
         plot = viz_tx_ml,
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


  # IIT Only
  iit_plot = plot_tx_ml(df = df_cntry, disagg = "iit", fcolor = ml_colors)
  iit_map = map_tx_ml(df_cntry, spdf_pepfar, terr, cntry, disagg = "IIT")

  viz_tx_ml <- iit_map + iit_plot +
    plot_layout(ncol = 2, widths = c(2, 1)) +
    plot_annotation(
      title = str_to_upper(glue("{cntry} - TX_ML INTERRUPTION IN TREATMENT (Only)")),
      caption = caption,
      theme = theme(plot.title = element_text(family = "Source Sans Pro", hjust = .5))
    )

  ggsave(here(dir_graphics,
              glue("{rep_pd} - {str_to_upper(cntry)}_TX_ML_IIT_lt3_and_3plus_{format(Sys.Date(), '%Y%m%d')}.png")),
         plot = viz_tx_ml,
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

  # Refused of Stopped Only
  rs_plot = plot_tx_ml(df = df_cntry, disagg = "rs", fcolor = ml_colors)
  rs_map = map_tx_ml(df_cntry, spdf_pepfar, terr, cntry, disagg = "Refused or Stopped")

  viz_tx_ml <- rs_map + rs_plot +
    plot_layout(ncol = 2, widths = c(2, 1)) +
    plot_annotation(
      title = str_to_upper(glue("{cntry} - TX_ML Refused or Stopped")),
      caption = caption,
      theme = theme(plot.title = element_text(family = "Source Sans Pro", hjust = .5))
    )

  ggsave(here(dir_graphics,
              glue("{rep_pd} - {str_to_upper(cntry)}_TX_ML_RefusedOrStopped_{format(Sys.Date(), '%Y%m%d')}.png")),
         plot = viz_tx_ml,
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

  # Died Only
  d_plot = plot_tx_ml(df = df_cntry, disagg = "d", fcolor = ml_colors)
  d_map = map_tx_ml(df_cntry, spdf_pepfar, terr, cntry, disagg = "Died")

  viz_tx_ml <- d_map + d_plot +
    plot_layout(ncol = 2, widths = c(2, 1)) +
    plot_annotation(
      title = str_to_upper(glue("{cntry} - TX_ML Died")),
      caption = caption,
      theme = theme(plot.title = element_text(family = "Source Sans Pro", hjust = .5))
    )

  ggsave(here(dir_graphics,
              glue("{rep_pd} - {str_to_upper(cntry)}_TX_ML_Died_{format(Sys.Date(), '%Y%m%d')}.png")),
         plot = viz_tx_ml,
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")





