##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: ART Saturation Classification
##  LICENCE: MIT
##  DATE:    2021-12-02
##  UPDATED: 2022-04-01


# DEPENDENCIES ----

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glitr)
  library(glamr)
  library(gisr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(sf)
  library(ggrepel)
  library(ggnewscale)
  library(patchwork)
  library(glue)


# Load Configs
source("../lastmile/Scripts/00_Geo_Utilities.R")

# SETUP ----

  # DIRs ----
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")
  #dir_targets <- "../../PEPFAR/COUNTRIES/Nigeria/FY21 - APR/data"

  # Files ----

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "Site_IM_FY20.*_N.*.zip$")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "PSNU_IM_FY20.*_N.*.zip$")

  # NAT Data - get the latest NAT_SUBNAT file
  file_natsub <- dir_merdata %>%
    return_latest(pattern = "NAT_SUBNAT_.*.zip$")

  # Shapefile path
  file_shp <- dir_geodata %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

# GLOBALS ----

  # Country name
  cntry <- "Nigeria"
  country <- "Nigeria"

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

    print(state)

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


#' @title ART Saturation Map
#' @param spdf_art Spatial data containing var to map
#' @param spdf     PEPFAR Polygons
#' @param terr     Terrain Raster
#' @param country  Ou / Country name
#'
art_saturation_map2 <-
  function(spdf_art, spdf,
           terr, country,
           rep_pd = "FY21Q4",
           lbl_size = 3,
           full_label = FALSE,
           add_caption = TRUE) {

    # ART Sat
    # spdf_art <- spdf_art %>%
    #   filter(operatingunit == country)

    max <- spdf_art %>%
      pull(ART_SAT) %>%
      max()

    max <- ifelse(max < 1, 1, max)

    caption <- ifelse(country == "Nigeria",
                      paste0("ART Saturation = TX_CURR / PLHIV (", str_sub(rep_pd,1,4), ")",
                             "\nUSAID's PSNUs are labelled with name + percent saturation",
                             "\nSource: ", rep_pd, " PSNU x IM MSDs, Produced on ",
                             format(Sys.Date(), "%Y-%m-%d")),
                      paste0("ART Saturation = TX_CURR_SUBNAT / PLHIV (", str_sub(rep_pd,1,4), ")",
                             "\nUSAID's PSNUs are labelled with name + percent saturation",
                             "\nSource: ", rep_pd, " NAT_SUBNAT & PSNU x IM MSDs, Produced on ",
                             format(Sys.Date(), "%Y-%m-%d"))
    )

    print(paste0(country, ": ", (spdf_art %>% nrow())))

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
        alpha = 0.7,
        na.value = NA,
        breaks = seq(0, max, .25),
        limits = c(0, max),
        labels = percent
      ) +
      geom_sf(data = admin0,
              colour = grey10k,
              fill = NA,
              size = 1.5) +
      geom_sf(data = admin0,
              colour = grey90k,
              fill = NA,
              size = .3)

    # label control
    if (full_label == TRUE) {
      map <- map +
        geom_sf_text(data = spdf_art,
                     aes(label = paste0(psnu, "\n", percent(ART_SAT, 1))),
                     size = lbl_size,
                     color = grey10k)
    }
    else {
      map <- map +
        geom_sf_text(data = spdf_art %>%
                       filter(ART_SAT >= .9, fundingagency == "USAID"),
                     aes(label = percent(ART_SAT, 1)),
                     size = lbl_size,
                     color = grey10k)
    }

    # Add caption
    if (add_caption == TRUE) {

      map <- map +
        labs(
          #title = "ART SATUTATION IN USAID SUPPORTED PSNUs",
          #subtitle = "PSNUs with labelled",
          caption = caption)
    }

    # Add theme
    map <- map +
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

  # MSD Data----
  #df_sites <- file_site_im %>% read_msd()

  df_psnu_all <- file_psnu_im %>% read_msd()

  # Rep. Periods
  curr_pd <- df_psnu_all %>% identifypd(pd_type = "full")
  curr_fy <- df_psnu_all %>% identifypd(pd_type = "year")

  # Sub-Nat
  df_nat <- file_natsub %>%
    read_msd() %>%
    filter(countryname == cntry)

  # PCO Data - FY21 Results
  df_pco <- list.files(
    path = dir_data,
    pattern = "^SNU Level Analysis_Iframe",
    full.names = TRUE) %>%
    read_excel(sheet = 1) %>%
    clean_names()

  # SPATIAL DATA ----
  terr <- gisr::get_raster(path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # Orgunits ----
  df_attrs <- get_attributes(country = cntry)

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(name))

  spdf_pepfar %>%
    st_drop_geometry() %>%
    distinct(label)

  admin0 <- spdf_pepfar %>% filter(label == "country")
  admin1 <- spdf_pepfar %>% filter(label == "prioritization")
  admin2 <- spdf_pepfar %>% filter(label == "community")


# MUNGE ----

  # Custom KP Request ----

  kp_inds <- c("KP_PREV", "HTS_TST", "HTS_TST_POS", "TX_CURR", "PrEP_CURR")

  df_psnu_all %>%
    filter(fundingagency != "Dedup",
           fiscal_year == curr_fy,
           indicator %in% kp_inds) %>%
    distinct(indicator, standardizeddisaggregate)

  df_psnu_all %>%
    filter(fundingagency != "Dedup",
           fiscal_year == curr_fy,
           indicator %in% kp_inds,
           str_detect(standardizeddisaggregate, "KeyPop")) %>%
    distinct(indicator, standardizeddisaggregate)

  df_psnu_kp <- df_psnu_all %>%
    filter(fundingagency != "Dedup",
           fiscal_year == curr_fy,
           indicator %in% kp_inds,
           str_detect(standardizeddisaggregate, "KeyPop")) %>%
    clean_agency() %>%
    group_by(fundingagency, indicator, otherdisaggregate_sub) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),
              sum, na.rm = TRUE), .groups = "drop")

  df_psnu_kp %>%
    rename_with(.cols = everything(), .fn = ~str_to_upper(.)) %>%
    write_csv(file = paste0("./Dataout/Nigeria_KP_Indicators_by_disaggs_",
                            format(Sys.Date(), "%Y-%m-%d"),
                            ".csv", na = ""))

  df_psnu_kp %>%
    group_by(fundingagency, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum, na.rm = TRUE), .groups = "drop") %>%
    rename_with(.cols = everything(), .fn = ~str_to_upper(.)) %>%
    write_csv(file = paste0("./Dataout/Nigeria_KP_Indicators_",
                            format(Sys.Date(), "%Y-%m-%d"),
                            ".csv", na = ""))


  # TX_CURR ----
  df_psnu_all %>% glimpse()
  df_psnu_all %>% distinct(fundingagency)

  df_psnu <- df_psnu_all %>%
    filter(fundingagency != "Dedup",
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(fiscal_year, psnuuid, psnu, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd() %>%
    filter(period_type == "results",
           period == curr_pd,
           psnu %ni% c("Ebonyi", "Anabara")
    ) %>%
    select(-period_type)

  # PLHIV ----
  df_nat %>% glimpse()
  df_nat %>% distinct(indicator)

  df_nat_all <- df_nat %>%
    filter(indicator == "PLHIV",
           standardizeddisaggregate == "Total Numerator",
           !is.na(snu1)) %>%
    select(fiscal_year, psnuuid, psnu, indicator, targets) %>%
    rename(value = targets, period = fiscal_year) %>%
    pivot_wider(names_from = period, values_from = value)

  df_nat <- df_nat %>%
    filter(indicator == "PLHIV",
           standardizeddisaggregate == "Total Numerator",
           !is.na(snu1)) %>%
    select(fiscal_year, psnuuid, psnu, indicator, targets) %>%
    rename(value = targets, period = fiscal_year) %>%
    filter(period == curr_fy) %>%
    mutate(period = paste0("FY", str_sub(period, 3,4)))

  # ART Saturation ----
  df_art <- df_psnu %>%
    bind_rows(df_nat) %>%
    group_by(psnuuid, psnu) %>%
    summarise(value = value[indicator == "TX_CURR"] / value[indicator == "PLHIV"]) %>%
    ungroup() %>%
    mutate(period = curr_pd,
           indicator = "ART_SAT") %>%
    bind_rows(df_psnu, df_nat, .) %>%
    select(-period) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    mutate(
        benchmark = case_when(
          ART_SAT >= .9 ~ "Above",
          ART_SAT < .9 ~ "Below",
          TRUE ~ NA_character_
        )) %>%
    mutate(
      benchmark = case_when(
        is.na(ART_SAT) & psnu %in% c("Abia", "Taraba") ~ "Newly Added",
        is.na(ART_SAT) & psnu %in% c("Ebonyi", "Anambra") ~ "GF States",
        is.na(ART_SAT) & psnu == "_Military Nigeria" ~ "Military",
        TRUE ~ benchmark),
      fill_color = case_when(
        benchmark == "Above" ~ genoa,
        benchmark == "Below" ~ old_rose,
        benchmark == "Newly Added" ~ grey40k,
        benchmark == "GF States" ~ grey10k
      ),
      text_color = case_when(
        benchmark == "Above" ~ grey10k,
        benchmark == "Below" ~ grey10k,
        benchmark == "Newly Added" ~ grey10k,
        benchmark == "GF States" ~ usaid_black
      )) %>%
    clean_names()

  df_art %>% glimpse()

  # PCO data ----
  # df_pco <- df_pco %>%
  #   rename(
  #     psnu = states,
  #     TX_CURR = fy21_results,
  #     PLHIV = plhiv_t_1,
  #     ART_SAT = percent_art_coverage
  #   ) %>%
  #   mutate(
  #     benchmark = case_when(
  #       ART_SAT >= .9 ~ "Above",
  #       ART_SAT < .9 ~ "Below",
  #       TRUE ~ NA_character_
  #     )) %>%
  #   mutate(
  #     benchmark = case_when(
  #       is.na(ART_SAT) & psnu %in% c("Abia", "Taraba") ~ "Newly Added",
  #       is.na(ART_SAT) & psnu %in% c("Ebonyi", "Anambra") ~ "GF States",
  #       is.na(ART_SAT) & psnu == "_Military Nigeria" ~ "Military",
  #       TRUE ~ benchmark),
  #     fill_color = case_when(
  #       benchmark == "Above" ~ genoa,
  #       benchmark == "Below" ~ old_rose,
  #       benchmark == "Newly Added" ~ grey40k,
  #       benchmark == "GF States" ~ grey10k
  #     ),
  #     text_color = case_when(
  #       benchmark == "Above" ~ grey10k,
  #       benchmark == "Below" ~ grey10k,
  #       benchmark == "Newly Added" ~ grey10k,
  #       benchmark == "GF States" ~ usaid_black
  #     ))

  # spdf_art <- admin1 %>%
  #   left_join(df_art, by = c("uid" = "psnuuid"))

  spdf_art <- admin1 %>%
    left_join(df_art, by = c("name" = "psnu")) %>%
    rename(psnu = name) %>%
    mutate(
      benchmark = case_when(
        is.na(art_sat) & psnu %in% c("Abia", "Taraba") ~ "Newly Added",
        is.na(art_sat) & psnu %in% c("Ebonyi", "Anambra") ~ "GF States",
        is.na(art_sat) & psnu == "_Military Nigeria" ~ "Military",
        TRUE ~ benchmark),
      fill_color = case_when(
        benchmark == "Above" ~ genoa,
        benchmark == "Below" ~ old_rose,
        benchmark == "Newly Added" ~ grey40k,
        benchmark == "GF States" ~ grey10k
      ),
      text_color = case_when(
        benchmark == "Above" ~ grey10k,
        benchmark == "Below" ~ grey10k,
        benchmark == "Newly Added" ~ grey10k,
        benchmark == "GF States" ~ usaid_black
      ))


# VIZ ----

  # locator Map
  locator_map(spdf_pepfar, terr, aoi = "Akwa Ibom")

  # Produce basemap
  basemap <- terrain_map(countries = admin0,
                         adm0 = admin0,
                         adm1 = admin1,
                         mask = TRUE,
                         terr = terr)

  # Produce thematic map
  art_map <- basemap +
    geom_sf(data = spdf_art,
            aes(fill = fill_color),
            lwd = .3,
            color = grey10k) +
    geom_sf(data = admin0,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = admin0,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_sf_text(data = spdf_art,
                 aes(label = paste0(str_replace(psnu, " ", "\n"),
                                    "\n",
                                    ifelse(is.na(art_sat), "TBC",
                                           percent(art_sat, 1))),
                     color = text_color),
                 size = 2, fontface = "bold") +
    scale_fill_identity() +
    scale_color_identity()


  # Export Map
  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - ART Saturation map - ",
             curr_date("%Y%m%d"),
             ".png")),
    plot = art_map,
    width = 8,
    height = 7)

  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - ART Saturation map for ",
             curr_date("%Y%m%d"), ".svg")),
    plot = art_map,
    width = 8,
    height = 7)
