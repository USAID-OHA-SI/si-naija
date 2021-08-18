##  PROJECT: LMA/Geospatial Distributions
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: MMD Distribution
##  LICENCE: MIT
##  DATE:    2021-03-02
##  UPDATED: 2021-06-30


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gisr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(sf)
  library(ggrepel)
  library(tidytext)
  library(patchwork)
  library(glue)
  library(ICPIutilities)
  library(rnaturalearth)

  source("./Scripts/00_Geo_Utilities.R")
  source("./Scripts/00_VL_Utilities.R")

# SETUP ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  ## Reporting Filters
  rep_agency = "USAID"
  rep_agencies <- c("USAID", "HHS/CDC")

  cntry <- "Nigeria"

  rep_fy = 2021

  rep_qtr = 2

  rep_fy2 = rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_fys = c(rep_fy - 1, rep_fy)

  rep_fys2 = rep_fys %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", .)

  rep_pd = rep_fy %>%
    as.character() %>%
    str_sub(3,4) %>%
    paste0("FY", ., "Q", rep_qtr)

  # Reference period: use last qtr of previous year
  ref_rep_pd = rep_fys2 %>%
    first() %>%
    paste0(., "Q4")

  rep_pds <- c(ref_rep_pd, rep_pd)

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_\\d{1}.zip$",
    recursive = FALSE
  )

  # NAT Data - get the latest NAT_SUBNAT file
  file_natsub <- return_latest(
    folderpath = dir_merdata,
    pattern = "^MER_.*_NAT_SUBNAT_.*_\\d{8}_v\\d{1}_\\d{1}.zip$",
    recursive = FALSE
  )

  # Shapefile path
  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons.*.shp",
    recursive = TRUE
  )

  # MSD File version
  msd_version <- ifelse(str_detect(file_psnu_im, ".*_\\d{8}_v1_\\d"), "i", "c")
  msd_version <- paste0(rep_pd, msd_version)

# FUNCTIONS ----

  #' @title MMD Map
  #'
  #' @param df      MMD Processed DataFrame
  #' @param basemap Basemap as ggplot plot
  #' @param len     MMD Duration
  #'
  mmd_map <- function(df, spdf, terr,
                      country = "Nigeria",
                      rep_pds = c("FY20Q4", "FY21Q2"),
                      len = "3+",
                      pal = "genoas",
                      add_name = FALSE) {

    print(glue("Country = {country}, Period = {rep_pd}, MMD = {len}"))

    # ART Sat
    spdf_mmd <- spdf %>%
      left_join(df, by = c("uid" = "psnuuid", "operatingunit", "countryname")) %>%
      mutate(
        countryname = case_when(
          countryname == "Democratic Republic of the Congo" ~ "DRC",
          TRUE ~ countryname
        )
      ) %>%
      filter(operatingunit == country,
             period %in% rep_pds,
             mmd_len == len,
             !is.na(mmd_share))

    max <- spdf_mmd %>%
      pull(mmd_share) %>%
      max()

    max <- ifelse(max < 1, 1, max)

    print(glue("Rows: {spdf_mmd %>% nrow()}, Max = {max}"))

    # Extract admin 0 and 1 for basemap
    admin0 <- spdf %>%
      filter(operatingunit == country, label == "country")

    admin1 <- spdf %>%
      filter(operatingunit == country, label == "snu1")

    # Produce basemap
    basemap <- terrain_map(countries = admin0,
                           adm0 = admin0,
                           adm1 = admin1,
                           mask = TRUE,
                           terr = terr)

    # Map
    map <- basemap +
      geom_sf(data = spdf_mmd,
              aes(fill = mmd_share),
              size = .3,
              color = grey10k,
              alpha = 0.5) +
      geom_sf(data = admin0,
              color = grey10k,
              fill = NA,
              size = 1.5) +
      geom_sf(data = admin0,
              color = grey80k,
              fill = NA,
              size = .3)

    # PSNU Labels
    if (add_name) {
      map <- map +
        geom_sf_text(data = spdf_mmd %>%
                       mutate(lbl_color = if_else(mmd_share > .3, grey20k, grey80k)),
                     aes(label = paste0(psnu, "\n", percent(mmd_share, 1)),
                         color = lbl_color),
                     size = 3)
    }
    else {
      map <- map +
        geom_sf_text(data = spdf_mmd %>%
                       mutate(lbl_color = if_else(mmd_share > .3, grey20k, grey80k)),
                     aes(label = percent(mmd_share, 1), color = lbl_color),
                     size = 3)
    }

    # Map style
    map <- map +
      scale_fill_si(
        palette = pal,
        discrete = FALSE,
        alpha = 0.7,
        na.value = NA,
        breaks = seq(0, max, .25),
        limits = c(0, 1),
        labels = percent
      ) +
      scale_color_identity() +
      facet_wrap(~period, nrow = 1) +
      labs(
        #title = glue("MMD{len} SCALING UP IN AFRICA",
        #subtitle = "% Treatment by MMD Duration",
        caption = glue("MMD{len} = TX_CURR[{len}] / TX_CURR",
                         "\nSource: {rep_pds %>% last()} PSNU x IM MSDs, Produced on ",
                         {format(Sys.Date(), "%Y-%m-%d")})) +
      si_style_map() +
      theme(plot.title = element_text(family = "Source Sans Pro", color = usaid_red),
            plot.subtitle = element_text(family = "Source Sans Pro", color = usaid_red),
            plot.caption = element_text(family = "Source Sans Pro"))
  }




# DATA ----

  # MSD
  df_psnu <- file_psnu_im %>% read_msd()

  # SPATIAL DATA

  ## PEPFAR Boundaries
  terr <- gisr::get_raster(terr_path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))


  ## NE Admins

  # Get country boundaries
  # africa <- ne_countries(continent = "africa", returnclass = "sf") %>%
  #   st_transform(crs = st_crs(4326)) %>%
  #   dplyr::select(iso3 = sov_a3, name, admin, sovereignt) %>%
  #   mutate(
  #     iso3 = case_when(
  #       iso3 == "SDS" ~ "SSD",
  #       TRUE ~ iso3
  #     )
  #   )

  # Append PEPFAR OUs
  # afr_countries <- africa %>%
  #   left_join(df_ous, by = c("iso3" = "countryname_iso")) %>%
  #   filter(!is.na(country_lvl))


# DATA MMD Distribution ----

  # Maps
  df_mmd <- df_psnu %>%
    filter(
      fiscal_year %in% rep_fys, # Needed for ref year/qtr
      fundingagency == rep_agency,
      indicator %in% c("TX_CURR"),
      standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus",
                                      "Total Numerator")
    ) %>%
    reshape_msd(clean = TRUE) %>%
    filter(period_type == "results") %>%
    mutate(
      otherdisaggregate = if_else(
        is.na(otherdisaggregate),
        NA_character_,
        str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
      ),
      otherdisaggregate = case_when(
        otherdisaggregate == "Less than 3 months" ~ "<3",
        otherdisaggregate == "3 to 5 months" ~ "3-5",
        otherdisaggregate == "6 or more months" ~ "6+",
        is.na(otherdisaggregate) ~ "tn",
        TRUE ~ otherdisaggregate
      )) %>%
    group_by(period, operatingunit, operatingunituid, countryname,
             psnu, psnuuid, otherdisaggregate) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(period %in% c(ref_rep_pd, rep_pd))

  df_mmd %>% glimpse()

  df_mmd %>% distinct(otherdisaggregate)

  # Track MMD 3+ for the last 2 Qtrs
  df_mmd_geq3 <- df_mmd %>%
    filter(otherdisaggregate %in% c("3-5", "6+")) %>%
    mutate(
      otherdisaggregate = case_when(
        otherdisaggregate == "3-5" ~ "3+",
        otherdisaggregate == "6+" ~ "3+",
        TRUE ~ otherdisaggregate
      )
    ) %>%
    group_by(period, operatingunit, operatingunituid, countryname,
             psnu, psnuuid, otherdisaggregate) %>%
    summarise_at(vars(value), sum, na.rm = TRUE) %>%
    ungroup()

  # Track MMD Not Reported
  df_mmd_notr <- df_mmd %>%
    mutate(otherdisaggregate = "nr") %>%
    group_by(period, operatingunit, operatingunituid, countryname,
             psnu, psnuuid, otherdisaggregate) %>%
    summarise(value = 0) %>%
    ungroup()

  # Track MMD all for the last 2 Qtrs
  df_mmd_share <- df_mmd %>%
    bind_rows(df_mmd_geq3) %>%
    bind_rows(df_mmd_notr) %>%
    group_by(period, operatingunit, operatingunituid,
             countryname, psnu, psnuuid) %>%
    mutate(
      value = case_when(
        otherdisaggregate == "nr" ~
          (value[otherdisaggregate == 'tn'] -
             sum(value[otherdisaggregate %in% c('<3', '3-5', '6+')])),
        TRUE ~ value
      ),
      value = if_else(value < 0, 0, value),
      mmd_total = value[otherdisaggregate == 'tn'],
      mmd_share = value / value[otherdisaggregate == 'tn']
    ) %>%
    ungroup() %>%
    rename(mmd_len = otherdisaggregate)

  df_mmd_share %>% glimpse()

  # Trends
  df_mmd_trend <- df_mmd %>%
    bind_rows(df_mmd_geq3) %>%
    bind_rows(df_mmd_notr) %>%
    group_by(period, operatingunit, operatingunituid,
             countryname, psnu, psnuuid) %>%
    mutate(
      value = case_when(
        otherdisaggregate == "nr" ~
          (value[otherdisaggregate == 'tn'] -
             sum(value[otherdisaggregate %in% c('<3', '3-5', '6+')])),
        TRUE ~ value
      ),
      value = if_else(value < 0, 0, value),
      mmd_total = value[otherdisaggregate == 'tn'],
      mmd_share = value / value[otherdisaggregate == 'tn']
    ) %>%
    ungroup() %>%
    dplyr::select(-c(value, mmd_total)) %>%
    rename(mmd_len = otherdisaggregate) %>%
    pivot_wider(names_from = period, values_from = c(mmd_total, mmd_share))

  df_mmd_trend2 <- df_mmd %>%
    bind_rows(df_mmd_geq3) %>%
    bind_rows(df_mmd_notr) %>%
    group_by(period, operatingunit, operatingunituid,
             countryname, psnu, psnuuid) %>%
    mutate(
      value = case_when(
        otherdisaggregate == "nr" ~
          (value[otherdisaggregate == 'tn'] -
             sum(value[otherdisaggregate %in% c('<3', '3-5', '6+')])),
        TRUE ~ value
      ),
      value = if_else(value < 0, 0, value)
    ) %>%
    ungroup() %>%
    rename(mmd_len = otherdisaggregate) %>%
    pivot_wider(names_from = period, values_from = value) %>%
    rename_at(vars(ends_with("Q4")), ~"prev_fy") %>%
    rename_at(vars(starts_with("FY")), ~"curr_pd") %>%
    group_by(operatingunit, operatingunituid,
             countryname, psnu, psnuuid) %>%
    mutate(prev_fy_total = prev_fy[mmd_len == 'tn'],
           prev_fy_share = prev_fy / prev_fy_total,
           curr_pd_total = curr_pd[mmd_len == 'tn'],
           curr_pd_share = curr_pd / curr_pd_total) %>%
    ungroup()

  #df_mmd_trend %>% glimpse()

  spdf_mmd <- spdf_pepfar %>%
    left_join(df_mmd_share, by = c("uid" = "psnuuid",
                                   "operatingunit", "countryname")) %>%
    filter(countryname != "South Africa", !is.na(mmd_len)) %>%
    mutate(
      countryname = case_when(
        countryname == "Democratic Republic of the Congo" ~ "DRC",
        TRUE ~ countryname
      )
    )

  spdf_mmd %>% glimpse()

# VIZ ----

  mmd_labeller <- as_labeller(
    c("<3" = "Less then 3 Months",
      "3-5" = "3 - 5 Months",
      "6+" = "6 or more months",
      "3+" = "3 or more months",
      "nr" = "Not reported",
      "tn" = "All PLHIV on ART")
    )

  # Bars
  df_mmd_share %>%
    filter(period == rep_pd,
           countryname == cntry,
           !mmd_len %in% c("nr", "tn")) %>%
    clean_psnu() %>%
    mutate(
      mmmd_len = factor(mmd_len,
                        levels = c("<3", "3-5", "6+", "3+", "nr", "tn"),
                        labels = c("Less then 3 Months", "3 - 5 Months",
                                   "6 or more months", "3 or more months",
                                   "Not reported", "All PLHIV on ART")),
      psnu = if_else(str_detect(psnu, "_Military"), "_Military", psnu),
      psnu = paste0(psnu, " (", comma(mmd_total, 1), ")"),
      psnu = reorder_within(psnu, mmd_share, mmd_len),
      #share_color = if_else(mmd_share < 0.03, grey70k, "white")) %>%
      share_color = grey70k) %>%
    ggplot(aes(x = reorder(psnu, mmd_share),
               y = mmd_share, label = percent(mmd_share, 1))) +
    geom_col(aes(y = 1.0), fill = grey10k) +
    geom_col(fill = usaid_blue) +
    geom_hline(yintercept = .25, color = grey20k) +
    geom_hline(yintercept = .50, color = grey20k) +
    geom_hline(yintercept = .75, color = grey20k) +
    geom_hline(yintercept = 1, color = grey20k) +
    geom_text(aes(color = share_color), size = 3, hjust = -0.1) +
    scale_x_reordered() +
    scale_y_continuous(labels = percent, position = "right") +
    scale_color_identity() +
    coord_flip(expand = F, clip = "off") +
    facet_wrap(~mmd_len, scales = "free_y", labeller = mmd_labeller) +
    labs(x = "", y = "",
         caption = paste0(glue("TX_CURR by Multi Months Dispensing ({rep_pd})"),
                          "\n", glue("Source: {rep_pd} PSNU x IM MSD, Produced on {format(Sys.Date(), '%Y-%m-%d')}"))) +
    si_style_xgrid() +
    theme(strip.placement = "outide")

  si_save(
    filename = file.path(
      dir_graphics,
      glue("{rep_pd} - {str_to_upper(cntry)} - MMD Dispensing plot - {format(Sys.Date(), '%Y%m%d')}.png")),
    plot = last_plot(),
    width = 9.54,
    height = 5,
    scale = 1.4)


  # Trend #1
  df_mmd_trend %>%
    filter(countryname == cntry,
           !mmd_len %in% c("nr", "tn")) %>%
    clean_psnu() %>%
    mutate(psnu = if_else(str_detect(psnu, "_Military"), "_Military", psnu),
           psnu = reorder_within(psnu, FY21Q2, mmd_len),
           FY20Q4 = if_else(is.na(FY20Q4), 0, FY20Q4),
           FY21Q2 = if_else(is.na(FY21Q2), 0, FY21Q2),
           trend = if_else(FY21Q2 > FY20Q4, usaid_blue, grey50k)) %>%
    ggplot(aes(x = reorder(psnu, FY21Q2))) +
    geom_segment(aes(xend = reorder(psnu, FY21Q2),
                     y = FY20Q4, yend = FY21Q2, color = trend), size = 1) +
    geom_point(aes(y = FY20Q4), shape = 21, size = 4, fill = grey50k, color = grey10k) +
    geom_point(aes(y = FY21Q2), shape = 21, size = 4, fill = usaid_blue, color = grey10k) +
    scale_y_continuous(labels = percent, position = "right") +
    scale_x_reordered() +
    scale_color_identity() +
    coord_flip() +
    facet_wrap(~mmd_len, scales = "free_y", labeller = mmd_labeller) +
    labs(x = "", y = "",
         title = "NIGERIA - FY20Q2 MMD Distribution",
         subtitle = "MMD3+ has increased between <span style='color:#939598;'>**FY20Q4**</span> and <span style='color:#002a6c'>**FY21Q2**</span> in most states<br/>States are sorted by FY21Q2 share of MMD ",
         caption = paste0(glue("TX_CURR by Multi Months Dispensing ({rep_pd})"),
                          "\n", glue("Source: {rep_pd} PSNU x IM MSD, Produced on {format(Sys.Date(), '%Y-%m-%d')}"))) +
    si_style() +
    theme(strip.placement = "outide",
          strip.text = element_text(size = 10),
          axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 6),
          plot.title = element_markdown(family = "Source Sans Pro"),
          plot.subtitle = element_markdown(family = "Source Sans Pro"))

  # Trend #2
  df_mmd_trend2 %>%
    filter(countryname == cntry,
           !mmd_len %in% c("nr", "tn")) %>%
    clean_psnu() %>%
    mutate(prev_fy_share = if_else(is.na(prev_fy_share), 0, prev_fy_share),
           curr_pd_share = if_else(is.na(curr_pd_share), 0, curr_pd_share),
           trend = if_else(curr_pd_share > prev_fy_share, genoa, grey50k),
           psnu = if_else(str_detect(psnu, "_Military"), "_Military", psnu),
           psnu = paste0(psnu, " (", comma(curr_pd_total, 1), ")"),
           psnu = reorder_within(psnu, curr_pd_share, mmd_len)) %>%
    ggplot(aes(x = reorder(psnu, curr_pd_share))) +
    geom_segment(aes(xend = reorder(psnu, curr_pd_share),
                     y = prev_fy_share,
                     yend = curr_pd_share,
                     color = trend),
                 size = 1) +
    geom_point(aes(y = prev_fy_share, size = prev_fy),
               shape = 21, fill = grey50k,
               color = grey10k, show.legend = FALSE) +
    geom_point(aes(y = curr_pd_share, size = curr_pd, fill = curr_pd_share),
               shape = 21, color = grey10k) +
    scale_y_continuous(labels = percent, position = "right") +
    scale_x_reordered() +
    scale_color_identity() +
    scale_fill_si(palette = "genoas", discrete = F,
                  limits = c(0, 1), labels = percent,
                  aesthetics = "fill",
                  name = "") +
    scale_size(range = c(3, 10), guide = FALSE) +
    coord_flip() +
    facet_wrap(~mmd_len, scales = "free_y", labeller = mmd_labeller) +
    labs(x = "", y = "",
         title = glue("NIGERIA - {rep_pd} MMD Distribution"),
         subtitle = glue("MMD3+ has increased between <span style='color:{grey50k};'>**{ref_rep_pd}**</span> and <span style='color:{genoa}'>**{rep_pd}**</span> in most states<br/>States are sorted by {rep_pd} share of MMD"),
         caption = paste0(glue("TX_CURR by Multi Months Dispensing ({rep_pd})"),
                          "\n", glue("Source: {rep_pd} PSNU x IM MSD, Produced on {format(Sys.Date(), '%Y-%m-%d')}"))) +
    si_style() +
    theme(strip.placement = "outide",
          strip.text = element_text(size = 10),
          #axis.text.x = element_text(size = 5),
          #axis.text.y = element_text(size = 6),
          plot.title = element_markdown(family = "Source Sans Pro"),
          plot.subtitle = element_markdown(family = "Source Sans Pro"),
          legend.key.width = ggplot2::unit(80, "pt"))


  si_save(
    filename = file.path(
      dir_graphics,
      glue("{rep_pd} - {str_to_upper(cntry)} - MMD Trend plot - {format(Sys.Date(), '%Y%m%d')}.png")),
    plot = last_plot(),
    width = 9.54,
    height = 5,
    scale = 1.4)


  # Batch this ----
  spdf_mmd %>%
    st_drop_geometry() %>%
    filter(!mmd_len %in% c("tn", "nr")) %>%
    distinct(mmd_len) %>%
    pull()

  c("6+") %>%
  #c("3+", "6+") %>%
    map(function(mmd_len) {

      map <- mmd_map(df = df_mmd_share,
                     spdf = spdf_pepfar,
                     terr = terr,
                     country = cntry,
                     rep_pds = rep_pd,
                     len = mmd_len,
                     pal = "genoas")

      print(map)

      # si_save(
      #   filename = file.path(
      #     dir_graphics,
      #     glue("{rep_pds %>% last()} - {str_to_upper(cntry)} - MMD{mmd_len} Scaling map - {format(Sys.Date(), '%Y%m%d')}.png")),
      #   plot = map,
      #   width = 9.54,
      #   height = 5,
      #   scale = 1.4)

      return(mmd_len)
    })
