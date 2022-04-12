##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: COP22 / FY23 Targets Allocation
##  LICENCE: MIT
##  DATE:    2022-04-10
##  UPDATED: 2022-04-10

## Libraries ----

  library(tidyverse)
  library(readxl)
  library(openxlsx)
  library(gophr)
  library(glamr)
  library(gisr)
  library(sf)
  library(tameDP)
  library(janitor)
  library(glue)
  library(gt)
  library(gtExtras)

  source("./Scripts/N00_Utilities.R")

## GLOBALS ----

  # Dirs ----

  dir_merdata <- si_path("path_msd")
  dir_geodata <- si_path("path_vector")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_cop21 <- "../../PEPFAR/COUNTRIES/Nigeria/COPs/COP21"
  dir_cop22 <- "../../PEPFAR/COUNTRIES/Nigeria/COPs/COP22"

  #dir_cop22 %>% open_path()

  # Files ----

  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY20.*_N")

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*_N")

  file_subnat <- dir_merdata %>%
    glamr::return_latest(pattern = "NAT_SUBNAT_FY15")

  file_pepfar_shp <- dir_geodata %>%
    return_latest(pattern = "VcPepfarPolygons")

  file_cop22_mechs <- dir_cop22 %>%
    file.path("Data") %>%
    return_latest("FY22 Mechanisms flags.xlsx")

  file_cop22_dp <- dir_cop22 %>%
    return_latest("Nigeria_datapack_\\d{8}.*.xlsx$")

  # Params ----

  cntry <- "Nigeria"

  #ou_uid <- get_ouuid(cntry)

  ou_uid <- datapackr::cop22_valid_PSNUs %>%
    filter(ou == cntry) %>%
    pull(country_uid) %>%
    first()

  agency <- "USAID"

  curr_pd <- file_psnu_im %>% identify_pd()

  curr_fy <- curr_pd %>%
    str_sub(1, 4) %>%
    str_replace("FY", "20") %>%
    as.numeric()

  # Previous Fiscal Year
  prev_fy <- curr_fy - 1

  # Current COP
  cop_year <- curr_fy
  cop_fy <- curr_fy + 1

  # Mechanisms

  # KeyPop
  kp_mechs <- c(81860, 81861)

  # Closing in FY22: Exclude from future planning
  tx_mechs <- c(100222, # Epic (FY22Q?)
                14505, # SIDHAS (FY21Q3)
                18655, # TO1 (FY22Q?)
                81856, # TO2 (FY22Q?)
                81857) # TO3 (FY22Q?)

  ovc_mechs <- c(18656, 18657, 81862, 81863)

  ace_mechs <- c(160521, 160522, 160523, 160524, 160525, 160527)

# DATA

  # Geodata
  ras <- get_raster()

  spdf_pepfar <- file_pepfar_shp %>% read_sf()

  df_attrs <- dir_geodata %>%
    file.path("OU-Attributes") %>%
    return_latest(pattern = "^orghierarchy - nigeria") %>%
    read_csv()

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(name))

  adm0 <- spdf_pepfar %>% filter(label == "country")
  adm1 <- spdf_pepfar %>% filter(label == "prioritization")

  # Mechs Transitions
  df_mechs_flags <- file_cop22_mechs %>%
    read_excel(sheet = 1) %>%
    filter(str_detect(fundingagency, "USAID"), valid == 1) %>%
    select(psnu, from = mech_code, to = mech_code_fy23) %>%
    mutate(source = from,
           change = if_else(from == to, 0, 1)) %>%
    pivot_longer(cols = c(from, to),
                 names_to = "transition",
                 values_to = "mech_code")

  # MSD - PSNU by IM
  df_psnu <- file_psnu_im %>% read_msd()

  df_msd_mechs <- df_psnu %>%
    filter(fiscal_year %in% c(prev_fy, curr_fy),
           fundingagency == agency) %>%
    select(mech_code, mech_name, primepartner) %>%
    distinct() %>%
    update_mechs() %>%
    filter(str_detect(mech_name, "Placeholder", negate = TRUE)) %>%
    clean_mechs() %>%
    clean_partners()

  df_mechs <- df_mechs_flags %>%
    left_join(df_msd_mechs, by = "mech_code")

  # Colors
  df_mechs_colors <- df_mechs %>%
    group_by(source) %>%
    summarise(n = n_distinct(mech_code[source != mech_code]) ) %>%
    ungroup() %>%
    filter(n > 0)

  # Join Mechs details to shp
  spdf_mechs <- adm1 %>%
    left_join(df_mechs, by = c("name" = "psnu")) %>%
    filter(!is.na(transition)) %>%
    mutate(label = paste0(name, "\n", mech_code, "\n", mech_name))

  spdf_mechs %>% dview

# VIZ

  ## Basemap
  basemap <- terrain_map(countries = cntry,
                         adm0 = adm0,
                         adm1 = adm1,
                         mask = TRUE)

  basemap

  # TX Partners
  basemap +
    geom_sf(data = spdf_mechs %>% filter(source == "100222"),
            aes(fill = mech_code),
            size = .2, color = grey10k) +
    geom_sf_text(data = spdf_mechs %>% filter(source == "100222"),
              aes(label = label), color = usaid_black, size = 3) +
    scale_fill_si(palette = "category10", discrete = T) +
    facet_wrap(~transition)

  df_mechs_colors %>%
    pull(source) %>%
    walk(function(.x) {
      mech_map <- basemap +
        geom_sf(data = spdf_mechs %>% filter(source == .x),
                aes(fill = mech_code),
                size = .2, color = grey10k,
                show.legend = FALSE) +
        geom_sf_text(data = spdf_mechs %>% filter(source == .x),
                     aes(label = label),
                     color = usaid_black, size = 2,
                     show.legend = FALSE) +
        scale_fill_si(palette = "category10", discrete = T) +
        facet_wrap(~transition)

      #print(mech_map)

      si_save(plot = mech_map,
              filename = file.path(dir_graphics,
                                   paste0(cntry, " - FY23 ",
                                          first(df_mechs[df_mechs$mech_code == .x, "mech_name"]$mech_name),
                                          " - ", .x,
                                          " Transition.png")))
    })




