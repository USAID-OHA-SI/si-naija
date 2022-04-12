##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: ART Saturation Classification
##  LICENCE: MIT
##  DATE:    2021-12-02
##  UPDATED: 2022-03-03


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
source("./Scripts/N00_Utilities.R")

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

# LOAD DATA ----

  # MSD Data----
  #df_sites <- file_site_im %>% read_msd()
  df_psnu_all <- file_psnu_im %>% read_msd()

  # Rep. Periods
  curr_pd <- df_psnu_all %>% identifypd(pd_type = "full")
  lag2_pd <- lag_pd(pd = curr_pd, n = 2)

  curr_fy <- df_psnu_all %>% identifypd(pd_type = "year")

  # Sub-Nat
  df_nat_all <- file_natsub %>%
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
           psnu %ni% c("Ebonyi", "Anambra")
    ) %>%
    select(-period_type)

  df_psnu_tx <- df_psnu_all %>%
    clean_indicator() %>% #distinct(indicator) %>% arrange(indicator) %>%  prinf()
    filter(fundingagency != "Dedup",
           indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
           str_detect(standardizeddisaggregate, "Total.*tor")) %>%
    group_by(fiscal_year, psnuuid, psnu, indicator) %>%
    summarise(across(c(starts_with("qtr"), cumulative, targets),
                     sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd() %>%
    filter(period_type == "results") %>%
    select(-period_type)

  df_psnu_tx %>% distinct(indicator)

  df_psnu_tx <- df_psnu_tx %>%
    filter(indicator == "TX_CURR", period == lag2_pd) %>%
    mutate(indicator = "TX_CURR_LAG2", period = curr_pd) %>%
    bind_rows(df_psnu_tx, .) %>%
    filter(period == curr_pd)

  # PLHIV ----
  df_nat_all %>% glimpse()
  df_nat_all %>% distinct(indicator)

  df_nat <- df_nat_all %>%
    filter(indicator == "PLHIV",
           standardizeddisaggregate == "Total Numerator",
           !is.na(snu1)) %>%
    select(fiscal_year, psnuuid, psnu, indicator, targets) %>%
    rename(value = targets,
           period = fiscal_year) %>%
    pivot_wider(names_from = period, values_from = value)

  df_nat_pops <- df_nat_all %>%
    filter(indicator %in% c("PLHIV", "POP_EST"),
           standardizeddisaggregate == "Total Numerator") %>%
    select(fiscal_year, psnuuid, psnu, indicator, targets) %>%
    filter(fiscal_year == curr_fy) %>%
    rename(value = targets, period = fiscal_year) %>%
    mutate(period = curr_pd)


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

  # For new maps
  df_art_sds <- df_psnu_tx %>%
    bind_rows(df_nat_pops) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    clean_names() %>%
    relocate(tx_curr_lag2, .before = tx_curr) %>%
    mutate(art_sat = tx_curr / plhiv,
           vlc = tx_pvls_d / tx_curr_lag2,
           prop_plhiv = plhiv / sum(plhiv, na.rm = TRUE),
           prop_plhiv_pop = plhiv / pop_est)


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
        art_sat >= .9 ~ "Above",
        art_sat < .9 ~ "Below",
        TRUE ~ NA_character_
      ),
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

  spdf_art_sds <- admin1 %>%
    left_join(df_art_sds, by = c("name" = "psnu")) %>%
    rename(psnu = name) %>%
    mutate(
      benchmark = case_when(
        art_sat >= .81 ~ "Above",
        art_sat < .81 ~ "Below",
        TRUE ~ NA_character_
      ),
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
      ),
      art_color = case_when(
        is.na(art_sat) ~ grey10k,
        art_sat >= .3 ~ grey10k,
        TRUE ~ grey90k
      ),
      plhiv_vol_color = case_when(
        is.na(plhiv) ~ grey10k,
        plhiv > 50000 ~ grey10k,
        TRUE ~ grey90k
      ),
      plhiv_color = case_when(
        is.na(prop_plhiv) ~ grey10k,
        prop_plhiv > .02 ~ grey10k,
        TRUE ~ grey90k
      ),
      plhiv_color2 = case_when(
        is.na(prop_plhiv_pop) ~ grey10k,
        prop_plhiv_pop >= .006 ~ grey10k,
        TRUE ~ grey90k
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

  # Art Sat map
  art_map <- basemap +
    geom_sf(data = spdf_art_sds,
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
    geom_sf_text(data = spdf_art_sds,
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
      paste0("NIGERIA - ART Saturation above 81 map - ",
             curr_date("%Y%m%d"),
             ".png")),
    plot = art_map,
    width = 8,
    height = 7)

  # Art Sat map 2
  art_map2 <- basemap +
    geom_sf(data = spdf_art_sds,
            aes(fill = art_sat),
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
    geom_sf_text(data = spdf_art_sds,
                 aes(label = paste0(str_replace(psnu, " ", "\n"),
                                    "\n",
                                    ifelse(is.na(art_sat), "TBC",
                                           percent(art_sat, 1))),
                     color = art_color),
                 size = 2, fontface = "bold") +
    scale_fill_si(palette = "genoas", labels = percent,
                  #limits = c(0, max(spdf_art_sds$art_sat, na.rm = T))) +
                  limits = c(0, 2),
                  breaks = seq(0, 2, .5)) +
    scale_color_identity() +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(.3, "cm"))



  # Export Map
  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - Percent ART Saturation map - ",
             curr_date("%Y%m%d"),
             ".png")),
    plot = art_map2,
    width = 8,
    height = 7)

  # si_save(
  #   filename = file.path(
  #     dir_graphics,
  #     paste0("NIGERIA - ART Saturation map for ",
  #            curr_date("%Y%m%d"), ".svg")),
  #   plot = art_map,
  #   width = 8,
  #   height = 7)

  # PLHIV map --
  plhiv_map <- basemap +
    geom_sf(data = spdf_art_sds,
            aes(fill = plhiv),
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
    geom_sf_text(data = spdf_art_sds,
                 aes(label = paste0(str_replace(psnu, " ", "\n"),
                                    "\n",
                                    ifelse(is.na(plhiv), "TBC",
                                           comma(plhiv, 1))),
                     color = plhiv_vol_color),
                 size = 2, fontface = "bold") +
    scale_fill_si(palette = "burnt_siennas", labels = comma,
                  #limits = c(0, max(spdf_art_sds$plhiv, na.rm = T))) +
                  limits = c(0, 200000),
                  breaks = seq(0, 200000, 50000)) +
    scale_color_identity() +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(.3, "cm"))


  # Export Map
  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - PLHIV Population map - ",
             curr_date("%Y%m%d"),
             ".png")),
    plot = plhiv_map,
    width = 8,
    height = 7)

  # % Share of PLHIV map
  prop_plhiv_map <- basemap +
    geom_sf(data = spdf_art_sds,
            aes(fill = prop_plhiv),
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
    geom_sf_text(data = spdf_art_sds,
                 aes(label = paste0(str_replace(psnu, " ", "\n"),
                                    "\n",
                                    ifelse(is.na(prop_plhiv), "TBC",
                                           percent(prop_plhiv, 1))),
                     color = plhiv_color),
                 size = 2, fontface = "bold") +
    scale_fill_si(palette = "burnt_siennas", labels = percent,
                  #limits = c(0, max(spdf_art_sds$prop_plhiv, na.rm = T))) +
                  limits = c(0, .12),
                  breaks = seq(0, .12, .02)) +
    scale_color_identity() +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(.3, "cm"))


  # Export Map
  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - Prop of Total PLHIV Population map - ",
             curr_date("%Y%m%d"),
             ".png")),
    plot = prop_plhiv_map,
    width = 8,
    height = 7)


  # % Share of PLHIV / POP map
  prop_plhiv_map2 <- basemap +
    geom_sf(data = spdf_art_sds,
            aes(fill = prop_plhiv_pop),
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
    geom_sf_text(data = spdf_art_sds,
                 aes(label = paste0(str_replace(psnu, " ", "\n"),
                                    "\n",
                                    ifelse(is.na(prop_plhiv_pop), "TBC",
                                           percent(prop_plhiv_pop, .1))),
                     color = plhiv_color2),
                 size = 2, fontface = "bold") +
    scale_fill_si(palette = "burnt_siennas", labels = percent,
                  #limits = c(0, max(spdf_art_sds$prop_plhiv_pop, na.rm = T))) +
                  limits = c(0, .035),
                  breaks = seq(0, .035, .01)) +
    scale_color_identity() +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(.3, "cm"))


  # Export Map
  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - PLHIV Prop of Gen Population map - ",
             curr_date("%Y%m%d"),
             ".png")),
    plot = prop_plhiv_map2,
    width = 8,
    height = 7)


  # % VLC map ----
  vlc_map <- basemap +
    geom_sf(data = spdf_art_sds,
            aes(fill = vlc),
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
                                    ifelse(is.na(vlc), "TBC",
                                           percent(vlc, 1))),
                     color = text_color),
                 size = 2, fontface = "bold") +
    scale_fill_si(palette = "genoas", labels = percent,
                  #limits = c(0, max(spdf_art_sds$vlc, na.rm = T))) +
                  limits = c(.7, 1)) +
    scale_color_identity() +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank(),
          legend.key.width = unit(2, "cm"),
          legend.key.height = unit(.3, "cm"))


  # Export Map
  si_save(
    filename = file.path(
      dir_graphics,
      paste0("NIGERIA - VL Coverage map - ",
             curr_date("%Y%m%d"),
             ".png")),
    plot = vlc_map,
    width = 8,
    height = 7)
