##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: TX_CURR Results Trend by Gender
##  LICENCE: MIT
##  DATE:    2021-03-08


# DEPENDENCIES ----

library(tidyverse)
library(readxl)
library(janitor)
library(glitr)
library(glamr)
library(gisr)
library(extrafont)
library(scales)
library(ggtext)
library(sf)
library(ggrepel)
library(ggnewscale)
library(patchwork)
library(glue)
library(ICPIutilities)

# Load configs
source("../lastmile/Scripts/00_Geo_Utilities.R")

# SETUP ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_20210212_v1_1_N.*.zip$")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_20210212_v1_1_N.*.zip$")

  # NAT Data - get the latest NAT_SUBNAT file
  file_natsub <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_NAT_SUBNAT_.*_20210212_v1_1.zip$")

  # Shapefile path
  file_shp <- dir_geodata %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

# GLOBALS ----

  # Country name
  cntry <- "Nigeria"


# FUNCTIONS ----

  #' @title Get n% of a number
  #'
  #' @param num  Number
  #' @param prop Proportion, default 1
  #' @param ...  Other valid options of round
  #'
  get_proportion <- function(n, p = 1, ...) {
    base::sapply(n, function(x) {
      round((x * (p / 100)), ...)
    })
  }

  #' @title Locator map
  #'
  #' @param aoi
  #'
  locator_map <- function(spdf, terr,
                          country = "Nigeria",
                          aoi = NULL,
                          lbl_all = TRUE,
                          lbl_size = 3) {
    # Notification
    print(aoi)

    # Extract admin 0 and 1 for basemap
    admin0 <- spdf %>%
      filter(operatingunit == country,
             label == "country")

    admin1 <- spdf %>%
      filter(operatingunit == country,
             label == "snu1")

    # Identify oai
    locator <- spdf %>%
      filter(operatingunit == country,
             label == "prioritization",
             name == aoi)

    # Target layer for labels
    if ( lbl_all == TRUE) {
      lbl_layer <- admin1
    } else {
      lbl_layer <- locator
    }

    # Produce basemap
    basemap <- terrain_map(countries = admin0,
                           adm0 = admin0,
                           adm1 = admin1,
                           mask = TRUE,
                           terr = terr)

    # locator map
    map <- basemap +
      geom_sf(data = locator,
              fill = glitr::burnt_sienna,
              alpha = .8) +
      geom_sf_text(data = lbl_layer,
                   aes(label = name),
                   size = lbl_size,
                   color = usaid_darkgrey)

    return(map)
  }


#' @title ART Saturation Map
#' @param spdf_art Spatial data containing var to map
#' @param spdf     PEPFAR Polygons
#' @param terr     Terrain Raster
#' @param country  Ou / Country name
#'
art_saturation_map <-
  function(spdf_art, spdf,
           terr,
           state = NULL,
           lbl_size = 3) {

    print(country)

    # ART Sat
    spdf_art <- spdf_art %>%
      filter(operatingunit == country)

    print(spdf_art %>% nrow())

    # Extract admin 0 and 1 for basemap
    admin0 <- spdf %>%
      filter(operatingunit == country,
             label == "country")

    admin1 <- spdf %>%
      filter(operatingunit == country,
             label == "snu1")

    # Produce basemap
    basemap <- terrain_map(countries = admin0,
                           adm0 = admin0,
                           adm1 = admin1,
                           mask = TRUE,
                           terr = terr)

    # Produce thematic map
    map <- basemap +
      geom_sf(data = spdf_art,
              aes(fill = ART_SAT),
              lwd = .3,
              color = grey10k) +
      scale_fill_si(
        palette = "burnt_siennas",
        discrete = FALSE,
        alpha = .8,
        na.value = NA,
        breaks = seq(0, 1, .25),
        limits = c(0, 1),
        labels = percent
      ) +
      geom_sf(data = admin0,
              colour = grey10k,
              fill = NA,
              size = 1) +
      geom_sf(data = admin0,
              colour = grey90k,
              fill = NA,
              size = .3) +
      geom_sf_text(data = spdf_art,
                   aes(label = paste0(psnu, "\n", percent(ART_SAT, 1))),
                   size = lbl_size,
                   color = grey10k) +
      labs(
      #title = "ART SATUTATION IN USAID SUPPORTED PSNUs",
      #subtitle = "PSNUs with labelled",
      caption = paste0("ART Saturation = TX_CURR_SUBNAT / PLHIV (FY21)",
                       "\nUSAID's PSNUs are labelled with name + percent saturation",
                       "\nSource: FY21Q1i NAT_SUBNAT & PSNU x IM MSDs, Produced on ",
                       format(Sys.Date(), "%Y-%m-%d"))) +
      si_style_map()

    return(map)
  }


#' @title Trend of ART Saturation
#'
#' @param
#'
trend_art_saturation <- function(df) {

  # Get the gap
  min <- df %>%
    pull(TX_CURR) %>%
    min()

  gap_top <- min %>% get_proportion(., 20)

  gap_bottom <- min %>% get_proportion(., 10)

  # Produce the plot
  viz <- df %>%
    ggplot(aes(x = reorder(period2, pd_order), y = TX_CURR)) +
      geom_col(aes(fill = ART_SAT), show.legend = F) +
      geom_line(aes(y = TX_CURR + gap_top, group = 1), color = usaid_darkgrey) +
      geom_hline(yintercept = 0, color = usaid_darkgrey) +
      geom_label(aes(y = TX_CURR + gap_top,
                     label = percent(ART_SAT, 1),
                     color = tx_label),
                 size = 6,
                 color = grey90k) +
      geom_text(aes(y = gap_bottom,
                    label = comma(TX_CURR)),
                size = 6,
                color = "white") +
      scale_fill_si(
        palette = "genoas", #"burnt_siennas",
        discrete = FALSE,
        alpha = 0.7,
        breaks = seq(0, 1, .25),
        limits = c(0, 1),
        labels = percent
      ) +
      labs(x = "", y = "") +
      si_style_nolines() +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14)
      )

  return(viz)
}


# LOAD DATA ----

  # Data
    df_sites <- file_site_im %>% read_msd()

    #df_psnu <- file_psnu_im %>% read_msd()

    df_nat <- file_natsub %>% read_msd()

  # SPATIAL DATA
    terr <- gisr::get_raster(terr_path = dir_terr)

    spdf_pepfar <- file_shp %>% sf::read_sf()

    df_attrs <- get_attributes(country = cntry)

    spdf_pepfar <- spdf_pepfar %>%
      left_join(df_attrs, by = c("uid" = "id")) %>%
      filter(!is.na(name))

    spdf_pepfar %>%
      st_drop_geometry() %>%
      distinct(label)

    spdf_pepfar %>%
      filter(name == "Akwa Ibom",
             label == 'prioritization') %>%
      gview()




# MUNGE ----

  # PLHIV

  df_nat %>%
    filter(indicator %in% c("PLHIV")) %>%
    distinct(fiscal_year, standardizeddisaggregate) %>%
    arrange(fiscal_year, standardizeddisaggregate)

  df_nat %>%
    filter(indicator %in% c("PLHIV")) %>%
    distinct(fiscal_year, standardizeddisaggregate, statushiv) %>%
    arrange(fiscal_year, standardizeddisaggregate)

  df_plhiv <- df_nat %>%
    reshape_msd(clean = TRUE) %>%
    filter(operatingunit == cntry,
           indicator %in% c("PLHIV"),
           standardizeddisaggregate == "Total Numerator") %>%
    rename(countryname = countrynamename) %>%
    group_by(period, psnuuid, psnu, indicator) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = value)

  df_plhiv %>% glimpse()


  df_plhiv2 <- df_nat %>%
    reshape_msd(clean = TRUE) %>%
    filter(operatingunit == cntry,
           indicator %in% c("PLHIV"),
           standardizeddisaggregate == "Age/Sex") %>%
    group_by(period, psnuuid, psnu, indicator, trendscoarse, sex) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = value)

  df_plhiv2 %>% glimpse()
  df_plhiv2 %>% view()

  # Treatment

  df_sites %>%
    filter(indicator == 'TX_CURR') %>%
    distinct(fiscal_year, standardizeddisaggregate) %>%
    arrange(fiscal_year, standardizeddisaggregate) %>%
    prinf()

  # TX PSNU
  df_tx_locs <- df_sites %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == 2021) %>%
    clean_agency() %>%
    distinct(fundingagency, psnuuid, psnu)

  df_tx_locs %>% glimpse()
  df_tx_locs %>% view()

  df_tx_curr <- df_sites %>%
    reshape_msd(clean = TRUE) %>%
    clean_agency() %>%
    filter(indicator == "TX_CURR",
           period_type == "results",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(psnuuid, psnu, period, indicator) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(fiscal_year = str_sub(period, 1, 4)) %>%
    pivot_wider(names_from = indicator, values_from = value)

  df_tx_curr2 <- df_sites %>%
    reshape_msd(clean = TRUE) %>%
    clean_agency() %>%
    filter(indicator == "TX_CURR",
           period_type == "results",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(psnuuid, psnu, community, communityuid, period, indicator) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(fiscal_year = str_sub(period, 1, 4)) %>%
    pivot_wider(names_from = indicator, values_from = value)

  df_tx_curr2 %>% glimpse()


  df_tx <- df_tx_curr %>%
    left_join(df_plhiv %>% select(-psnu),
              by = c("psnuuid",
                     "fiscal_year" = "period"),
              keep = FALSE) %>%
    rowwise() %>%
    mutate(ART_SAT = TX_CURR / PLHIV) %>%
    ungroup() %>%
    left_join(df_tx_locs %>% select(-psnu), by = "psnuuid")

  df_tx_viz <- df_tx %>%
    mutate(
      label = paste0(psnu, " (", comma(PLHIV, 1), ")"),
        pd_order = as.integer(str_remove_all(period, "FY|Q")),
        period1 = paste0(str_replace(period, "Q", " Q"),
                         " (", percent(ART_SAT, 1),
                         ")"),
        period2 = case_when(
          endsWith(period, "1") ~ paste0("Dec `",
                                         as.integer(str_sub(period, 3, 4)) - 1),
          endsWith(period, "2") ~ str_replace(period, "FY", "Mar `"),
          endsWith(period, "3") ~ str_replace(period, "FY", "Jun `"),
          endsWith(period, "4") ~ str_replace(period, "FY", "Sep `")
        ),
        period2 = str_remove(period2, "Q.*"),
        period2 = fct_reorder(period2, pd_order))

  #df_tx_viz %>% glimpse()
  df_tx_viz %>% view()
  df_tx_viz %>% filter(psnu == 'Lagos') %>%  view()


  # Join to spatial file
  spdf_tx <- spdf_pepfar %>%
    left_join(df_tx_viz, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(ART_SAT))


# VIZ ----

  # Countries with valid data
  spdf_tx %>%
    st_set_geometry(NULL) %>%
    distinct(psnu) %>%
    pull()

  # Map
  locator_map <- locator_map(spdf = spdf_pepfar,
                         terr = terr,
                         aoi = "Akwa Ibom",
                         lbl_all = F)

  cntry_art_map <- art_saturation_map(spdf_art = spdf_tx,
                                spdf = spdf_pepfar,
                                terr = terr,
                                lbl_size = 2)

  # Bar chart
  bar <- df_tx_viz %>%
    filter(fundingagency == "USAID",
           psnu == "Akwa Ibom",
           period > "FY19Q3") %>%
    trend_art_saturation()

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - Akwa Ibom - Trend of ART Saturation - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = bar,
    width = 10,
    height = 6)

  df_tx_viz %>%
    filter(fundingagency == "USAID",
           period > "FY19Q3") %>%
    distinct(psnu) %>%
    pull(psnu) %>%
    sort() %>%
    map(function(aoi) {

      print(aoi)

      df <- df_tx_viz %>%
        filter(psnu == aoi,
               period > "FY19Q3")

      plot <- trend_art_saturation(df)

      # +
      #   labs(title = str_to_upper(aoi)) +
      #   theme(plot.title = element_text(family = "Source Sans Pro",
      #                                   size = 14,
      #                                   face = "bold",
      #                                   color = usaid_darkgrey,
      #                                   hjust = .5))

      # plot <- trend_art_saturation(df) +
      #   labs(title = str_to_upper(paste0(aoi,
      #                                   " (",
      #                                   df %>%
      #                                     pull(TX_CURR) %>%
      #                                     min(), ")")))

      si_save(
        filename = file.path(
          dir_graphics,
          paste0("NIGERIA - ",
                 str_to_upper(aoi),
                 " - Trend of ART Saturation - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = plot,
        width = 10,
        height = 6)

      return(plot)
    })


  df_tx_viz %>%
    filter(fundingagency == "USAID",
           psnu == "Lagos",
           period > "FY19Q3") %>%
    ggplot(aes(x = reorder(period2, pd_order), y = TX_CURR)) +
    geom_col(aes(fill = ART_SAT), show.legend = F) +
    geom_line(aes(y = TX_CURR + 15000, group = 1),
              color = usaid_darkgrey) +
    geom_hline(yintercept = 0, color = usaid_darkgrey) +
    geom_label(aes(y = TX_CURR + 15000,
                  label = percent(ART_SAT, 1),
                  color = tx_label),
              size = 5,
              color = grey90k) +
    geom_text(aes(y = 10000,
                  label = comma(TX_CURR),
                  color = tx_label),
              size = 5,
              color = "white") +
    scale_color_identity() +
    scale_fill_si(
      palette = "genoas",
      discrete = FALSE,
      alpha = 0.7,
      breaks = seq(0, 1, .25),
      limits = c(0, 1),
      labels = percent
    ) +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 14)
    )



  ### OLD ###
  nga_plot <- (nga_map + nga_bar)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("FY21Q1 - NIGERIA - ART Saturation 2 - ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_plot,
    width = 10,
    height = 5)

  # TX_CURR
  nga_tx_gender2 <- df_nga %>%
    mutate(
      fill_color = case_when(
        sex == "Female" ~ scooter,
        sex == "Male"~ old_rose,
        TRUE ~ usaid_lightgrey
      ),
      label = paste0(period, " (", comma(value), ")")
    ) %>%
    ggplot(aes(x = period, y = value, fill = fill_color)) +
    geom_col(position = "fill") +
    geom_text(aes(y = value, label = comma(value)),
              color = "white",
              position = position_fill(vjust = .5)) +
    scale_fill_identity() +
    scale_y_continuous(labels = percent) +
    si_style_ygrid() +
    labs(x = "", y = "")

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - TX_CURR by SEX (FY20 to Present) 2 ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_tx_gender2,
    width = 10,
    height = 7)


  nga_tx_gender3 <- df_nga %>%
    mutate(
      fill_color = case_when(
        sex == "Female" ~ scooter,
        sex == "Male"~ old_rose,
        TRUE ~ usaid_lightgrey
      ),
      label = paste0(period, " (", comma(value), ")")
    ) %>%
    ggplot(aes(x = period, y = value, fill = fill_color)) +
    geom_col(position = "stack") +
    geom_text(aes(y = value, label = comma(value)),
              color = "white",
              position = position_stack(vjust = .5)) +
    scale_fill_identity() +
    scale_y_continuous(labels = comma) +
    si_style_ygrid() +
    labs(x = "", y = "")

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - TX_CURR by SEX (FY20 to Present) 3 ",
             format(Sys.Date(), "%Y%m%d"),
             ".png")),
    plot = nga_tx_gender3,
    width = 10,
    height = 7)
