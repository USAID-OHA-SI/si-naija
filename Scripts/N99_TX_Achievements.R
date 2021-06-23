##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: TX Achievements
##  LICENCE: MIT
##  DATE:    2021-01-19
##  UPDATED: 2021-06-10


# PACKAGES -------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(glitr)
  library(scales)
  library(glamr)
  library(sf)
  library(raster)
  library(gisr)
  library(janitor)
  library(ICPIutilities)
  library(extrafont)
  library(glue)

# GLOBAL --------------------------------------------------

  # Load configs
  source("./Scripts/N00_Config.R")
  source("../lastmile/Scripts/00_Geo_Utilities.R")

  # Country name
  cntry <- "Nigeria"

  rep_fy = 2021

  rep_qtr = 2

  rep_agency = "USAID"

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


  # Latest MSD PSNU x IM File
  file_msd <- return_latest(
      folderpath = dir_merdata,
      pattern = "MER_S.*_PSNU_IM_.*_\\d{8}_v.*_N.*.zip"
    )

  # Latest MSD Site x IM File
  file_msd_sites <- return_latest(
      folderpath = dir_merdata,
      pattern = "MER_S.*_Site_IM_.*_\\d{8}_v.*_N.*.zip"
    )

  # Geodata
  file_shp <- return_latest(
      folderpath = dir_geodata,
      pattern = "VcPepfarPolygons.shp$",
      recursive = TRUE
    )


# DATA ----

  # Geodata ----

  # PEPFAR Boundaries
  terr <- gisr::get_raster(terr_path = dir_terr)

  # PEPFAR Geodata
  spdf_pepfar <- file_shp %>% read_sf()

  df_attrs <- gisr::get_ouuids() %>%
    filter(!str_detect(operatingunit, " Region$")) %>%
    pull(operatingunit) %>%
    map_dfr(.x, .f = ~get_attributes(country = .x))

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id"))

  admin0 <- spdf_pepfar %>%
    filter(operatingunit == cntry,
           label == "country")

  admin1 <- spdf_pepfar %>%
    filter(operatingunit == cntry,
           label == "snu1")

  admin2 <- spdf_pepfar %>%
    filter(operatingunit == cntry,
           label == "prioritization")

  # MSD Data ----

  # Sites
  df_msd_sites <- file_msd_sites %>%
    read_msd()

  df_tx_sites <- df_msd_sites %>%
    filter(operatingunit == country,
           fiscal_year == rep_fy,
           indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup",
           communityuid != "?") %>%
    reshape_msd(clean = TRUE)

  # MSD Community TX
  df_tx_comm <- df_tx_sites %>%
    clean_agency() %>%
    filter(operatingunit == country,
           period == rep_fy2,
           indicator %in% c("TX_CURR", "TX_NEW"),
           period_type %in% c("targets", "cumulative")) %>%
    group_by(fundingagency, snu1, psnuuid, psnu,
             communityuid, community, indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    clean_column("community") %>%
    pivot_wider(names_from = period_type, values_from = value) %>%
    mutate(achieve = cumulative / targets)


  # MSD PSNU
  df_msd_psnu <- file_msd %>% read_msd()

  df_tx_psnu <- df_msd_psnu %>%
    filter(operatingunit == country,
           fiscal_year == rep_fy,
           indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup",
           psnuuid != "?") %>%
    reshape_msd(clean = TRUE)

  # MSD Community TX
  df_tx <- df_tx_psnu %>%
    clean_agency() %>%
    filter(operatingunit == country,
           period == rep_fy2,
           indicator %in% c("TX_CURR", "TX_NEW"),
           period_type %in% c("targets", "cumulative")) %>%
    group_by(fundingagency, snu1, psnuuid, psnu,
             indicator, period, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = period_type, values_from = value) %>%
    mutate(achieve = cumulative / targets,
           achieve_cup = if_else(achieve > 1, 1, achieve))

  df_tx_usaid <- df_tx %>%
    filter(fundingagency == rep_agency)

  # MSD Geodata
  spdf_tx <- spdf_pepfar %>%
    left_join(df_tx_usaid, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(achieve))


# VIZ ----

  # Basemap
  basemap <- terrain_map(countries = cntry,
                         adm0 = admin0,
                         adm1 = admin1,
                         mask = TRUE,
                         terr = terr)

  max <- spdf_tx %>%
    filter(indicator == "TX_CURR") %>%
    pull(achieve) %>%
    max()

  psnus <- spdf_tx %>%
    filter(indicator == "TX_CURR") %>%
    pull(psnu)

  # Thematic maps - achievements by agency and indicator
  basemap +
    geom_sf(data = spdf_tx %>% filter(indicator == "TX_CURR"),
            aes(fill = achieve_cup), color = grey20k, size = .2, alpha = .6) +
    geom_sf(data = admin0, fill = NA, color = grey10k, size = 1.5, alpha = 0.25) +
    geom_sf(data = admin0, fill = NA, color = grey80k) +
    geom_sf_text(data = admin1 %>% filter(!name %in% psnus),
                 aes(label = name), color = grey60k, size = 8/.pt) +
    geom_sf_text(data = spdf_tx %>% filter(indicator == "TX_CURR"),
                 aes(label = paste0(psnu, "\n(", percent(achieve, 1), ")")),
                     color = grey10k, size = 8/.pt) +
    scale_fill_si(palette = "genoas", discrete = F,
                  labels = scales::percent_format(accuracy = 1),
                  limits = c(0, 1), alpha = .8) +
    labs(title = glue("{str_to_upper(cntry)} - HIGH TX_CURR ACHIEVEMENTS FOR MOST STATES"),
         subtitle = "88% of USAID States have reached at least 80% of their targets by Q2.\nNote: map scale has been capped to 100%.",
         caption = glue("Data source: {rep_pd} MSD - Produced on {format(Sys.Date(), '%Y-%m-%d')}")) +
    si_style_map() +
    theme(
      plot.title = element_text(family = "Source Sans Pro", size = 10),
      plot.subtitle = element_text(family = "Source Sans Pro", size = 8)
    )

  # Save output
  si_save(
    filename = file.path(
      dir_graphics,
      glue("{rep_pd} - {str_to_upper(cntry)} - TX_CURR Achievements Map - {format(Sys.Date(), '%Y%m%d')}.png")),
    plot = last_plot(),
    width = 7,
    height = 7,
    scale = 1.4)



