##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: TX Achievements
##  LICENCE: MIT
##  DATE:    2021-01-19


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
library(extrafont)

# GLOBAL --------------------------------------------------

  # Load configs
  source("./Scripts/N00_Config.R")

  # Country name
  cntry <- "Nigeria"


  # Latest MSD PSNU x IM File
  file_msd <- list.files(
      path = merdata,
      pattern = "MER_S.*_PSNU_IM_.*_\\d{8}_v.*_N.*.zip",
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

  # Latest MSD Site x IM File
  file_msd_sites <- list.files(
      path = merdata,
      pattern = "MER_S.*_Site_IM_.*_\\d{8}_v.*_N.*.zip",
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

  # Geodata
  file_shp <- list.files(
      path = shpdata,
      pattern = "VcPepfarPolygons.shp$",
      recursive = TRUE,
      full.names = TRUE
    ) %>%
    sort() %>%
    last()


# DATA ----

  # Geodata ----

  # PEPFAR Geodata
    spdf_pepfar <- file_shp %>% read_sf()

  # OUs
    df_ous <- glamr::identify_ouuids(datim_user(), datim_pwd())

  # OU uid
    ou_uid <- df_ous %>%
      dplyr::filter(type == "OU", country == cntry) %>%
      pull(uid)

  # Levels
    df_lvls <- glamr::identify_levels(datim_user(), datim_pwd())

  # Comm level
    comm_lvl <- df_lvls %>%
      filter(operatingunit == cntry) %>% View()
      pull(community)

  # Orgs
    df_locs <- gisr::extract_locations(cntry, datim_user(), datim_pwd())

  # Community uids
    comm_uids <- df_locs %>%
      filter(level == comm_lvl) %>%
      pull(id)

  # Community boundaries
    spdf_nga_comm <- spdf_pepfar %>%
      filter(uid %in% ids)

  # NGA Boundaries
    spdf_nga_ou <- spdf_pepfar %>%
      filter(uid == ou_uid)

    spdf_nga_cntry <- get_admin0(cntry) %>%
      dplyr::select(admin)

    spdf_nga_states <- get_admin1(cntry) %>%
      dplyr::select(state = name_de) %>%
      dplyr::mutate(state = if_else(
        state == "Federal Capital Territory",
        "FCT",
        state))


  # MSD Data

  # Sites
    df_msd_sites <- file_msd_sites %>%
      read_msd() %>%
      reshape_msd(clean = TRUE)

  # MSD Community TX
    df_msd_comm <- df_msd_sites %>%
      clean_agency() %>%
      filter(operatingunit == country,
             period == "FY20",
             indicator %in% c("TX_CURR", "TX_NEW"),
             period_type %in% c("targets", "cumulative"),
             standardizeddisaggregate == "Total Numerator",
             fundingagency != "Dedup",
             communityuid != "?") %>%
      group_by(fundingagency, snu1, psnuuid, psnu,
               communityuid, community, indicator, period, period_type) %>%
      summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
      ungroup() %>%
      clean_column("community") %>%
      pivot_wider(names_from = period_type, values_from = val) %>%
      mutate(achieve = cumulative / targets)

    df_msd_comm %>% View()


  # MSD Geodata
    df_geo <- spdf_pepfar %>%
      left_join(df_msd_comm, by = c("uid" = "communityuid")) %>%
      filter(!is.na(achieve))

  # Round max achievements to 100%
    df_geo <- df_geo %>%
      mutate(achieve = if_else(achieve > 1, 1, achieve))

# VIZ ----

  # Basemap
  terr <- terrain_map(cntry, terr_path = rasdata, mask = TRUE)

  terr

  # Thematic maps - achievements by agency and indicator
  terr +
    geom_sf(data = df_geo, aes(fill = achieve), color = grey20k, size = .2) +
    geom_sf(data = spdf_nga_comm, fill = NA, color = grey20k, size = .2) +
    geom_sf(data = spdf_nga_states, fill = NA, linetype = "dotted") +
    geom_sf(data = spdf_nga_cntry, color = "white", fill = grey20k, size = 2, alpha = 0.25) +
    geom_sf(data = spdf_nga_cntry, fill = NA, color = grey80k) +
    geom_sf_text(data = spdf_nga_states, aes(label = state), color = grey70k, size = 2) +
    facet_wrap(fundingagency ~ indicator) +
    scale_fill_si(palette = "genoas", reverse = T, discrete = F,
                  labels = scales::percent_format(accuracy = 1),
                  limits = c(0, 1)) +
    labs(title = "NIGERIA - FY20 TX Achievements by LGAs",
         subtitle = "Achievements greater than 100% have been rounded back to 100%.",
         caption = paste0("Data source: MSD FY20Q4c\nOHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"))) +
    si_style_map()

  # Save output
  ggsave(file.path(graphics, paste0("Nigeria - FY20 TX Achievements by LGAs - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.3, dpi = 350,
         width = 10, height = 7, units = "in")

