# PURPOSE: SI-Naija
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Program Coverage
# REF ID:  d269a16d
# LICENSE: MIT
# DATE:    2023-10-09
# UPDATE:  2023-10-09
# NOTES:   For Incoming MD Presentation

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(grabr)
  library(glitr)
  library(glamr)
  library(sf)
  library(gisr)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)

  source("./Scripts/N00_Utilities.R")

# LOCALS & SETUP ====

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_cntry <- file.path("../../PEPFAR/COUNTRIES/Nigeria")

  # Files

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY22.*_Nig")
  #file_site <- si_path() %>% return_latest("Site_IM_FY21.*_Nig")
  file_tb <- dir_data %>% return_latest("Map.*New TB Activity")

  # Shapefile path
  file_shp <- dir_shp %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

  meta <- get_metadata(file_psnu)

  # Set Params

  ref_id <- "d269a16d"
  agency <- "USAID"
  cntry <- "Nigeria"

# Functions  =====

# LOAD DATA =====

  df_msd_psnu <- file_psnu %>% read_psd()

  file_tb %>% excel_sheets()

  df_tb_states <- read_excel(path = file_tb, sheet = 1) %>%
    janitor::clean_names()

  df_tb_states %>% glimpse()

  # SPATIAL DATA

  terr <- gisr::get_raster(folderpath = dir_ras)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # Org Levels & Attributes

  df_levels <- get_levels(
      username = datim_user(),
      password = datim_pwd()
    ) %>%
    pivot_longer(
      cols = where(is.numeric),
      names_to = "orgunit_label",
      values_to = "orgunit_level"
    )

  # Org units attributes

  df_attrs <- grabr::datim_orgunits(
    cntry = cntry,
    username = datim_user(),
    password = datim_pwd()
  )

  df_attrs <- df_levels %>%
    filter(countryname == cntry) %>%
    select(countryname, orgunit_label, orgunit_level) %>%
    mutate(orgunit_level = as.character(orgunit_level)) %>%
    left_join(df_attrs, .,
              by = c("regionorcountry_name" = "countryname", "orgunit_level"),
              relationship = "many-to-many") %>%
    rename_with(.cols = contains("_internal_id"),
                .fn = ~str_replace(.x, "internal_id", "uid")) %>%
    relocate(orgunit_label, .after = orgunit_level) %>%
    select(-contains(c("_parent", "_code", "moh"))) %>%
    filter(str_detect(orgunit_name, "_Mil", negate = T))

# MUNGE =====

  # Geography

  spdf_pepfar %>% glimpse()

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = join_by("uid" == "orgunit_uid")) %>%
    filter(!is.na(orgunit_label))

  # Admin Boundaries

  spdf_cntry <- spdf_pepfar %>%
    filter(orgunit_label == "country")

  spdf_psnu <- spdf_pepfar %>%
    filter(orgunit_label == "prioritization")

  # TB - New Activity Coverage ----

  spdf_tb <- spdf_psnu %>%
    left_join(df_tb_states, by = c("orgunit_name" = "states")) %>%
    filter(!is.na(regions))

  # OVC ----

  df_ovc <- df_msd_psnu %>%
    filter(fiscal_year == meta$curr_fy,
           operatingunit == cntry,
           funding_agency == agency,
           str_detect(indicator, "OVC_")) %>%
    clean_agency() %>%
    clean_indicator() %>%
    clean_mechs() %>%
    clean_partners()

  df_ovc_serv <- df_ovc %>%
    filter(standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(cumulative, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, country, psnuuid, psnu, indicator))

  df_ovc_cov <- df_ovc %>%
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(cumulative, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, country, psnuuid, psnu, mech_name, prime_partner_name))

  spdf_ovc_cov <- spdf_psnu %>%
    left_join(df_ovc_cov, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(mech_name))

  ## Add FY25 IMs

  spdf_ovc_cov <- spdf_ovc_cov %>%
    mutate(
      mech_name_new = case_when(
        psnu %in% c("Lagos", "Edo", "Cross River", "Akwa Ibom", "Bayelsa") ~ "THRIVE SOUTH",
        psnu %in% c('Borno', 'Yobe', 'Bauchi', 'Jigawa', 'Taraba', 'Adamawa') ~ "THRIVE NORTH 1",
        psnu %in% c('Kano', 'Niger', 'Kebbi', 'Sokoto', 'Zamfara') ~ "THRIVE NORTH 2",
        TRUE ~ "Not Covered"
      )
    )

  # KP ----

  trans_states <- c("Kano", "Edo", "Taraba")
  gf_states <- c("Gombe", "Kwara", "Anambra", "Ebonyi")

  df_kp <- df_msd_psnu %>%
    filter(fiscal_year == meta$curr_fy,
           operatingunit == cntry,
           (funding_agency == agency &
           str_detect(mech_name, "KP CARE")) |
           psnu %in% c(trans_states, gf_states)) %>%
    mutate(mech_name = case_when(
      psnu %in% trans_states ~ "USAID Transition",
      psnu %in% gf_states ~ "GF Transition",
      TRUE ~ mech_name
    ))

  df_kp %>% distinct(mech_name)

  df_kp_cov <- df_kp %>%
    distinct(fiscal_year, funding_agency, country, psnuuid, psnu, mech_name) %>%
    clean_mechs() %>%
    add_row(psnu = "Anambra", mech_name = "GF Transition") %>%
    add_row(psnu = "Ebonyi", mech_name = "GF Transition")

  spdf_kp_cov <- spdf_psnu %>%
    left_join(df_kp_cov, by = c("orgunit_name" = "psnu")) %>%
    filter(!is.na(mech_name))

  # TX ----

  df_tx <- df_msd_psnu %>%
    filter(fiscal_year == meta$curr_fy,
           operatingunit == cntry,
           funding_agency == agency,
           str_detect(indicator, "TX_"),
           str_detect(standardizeddisaggregate, "KeyPop", negate = T),
           str_detect(mech_name, "KP CARE", negate = T))

  df_tx %>% distinct(mech_name)

  df_tx_cov <- df_tx %>%
    distinct(fiscal_year, funding_agency, country, psnuuid, psnu, mech_name) %>%
    clean_mechs() %>%
    filter(str_detect(psnu, "_Mil", negate = T)) %>%
    filter(!(mech_name == "RISE" & psnu != "Taraba"))

  spdf_tx_cov <- spdf_psnu %>%
    left_join(df_tx_cov, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(mech_name))

# VIZ =====

  # Base map

  bmap <- terrain_map(
    countries = spdf_cntry,
    adm0 = spdf_cntry,
    adm1 = spdf_psnu,
    mask = TRUE,
    terr = terr
  )

  bmap %>% print()

  # TB New Activities coverage

  tb_map <- bmap +
    geom_sf(data = spdf_tb, aes(fill = regions), size = .3, color = grey30k) +
    geom_sf(data = spdf_cntry,
            colour = grey10k, fill = NA, size = 1.5) +
    geom_sf(data = spdf_cntry,
            colour = grey90k, fill = NA, size = .3) +
    geom_sf_text(data = spdf_psnu,
                 aes(label = orgunit_name),
                 size = 3,
                 color = usaid_black) +
    scale_fill_manual(
      values = c("Region 1" = scooter, "Region 2" = old_rose)) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank())

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(str_to_upper(cntry),
             " - TB New Activities Coverage.png")),
    plot = tb_map,
    width = 10,
    height = 7,
    dpi = 320,
    scale = 1.1)

  # OVC Partners Coverage ----

  ovc_map <- bmap +
    geom_sf(data = spdf_ovc_cov %>%
              filter(orgunit_name %ni% c("Kwara")),
            aes(fill = mech_name),
            size = .3,
            color = grey30k) +
    geom_sf(data = spdf_cntry,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_cntry,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_sf_text(data = spdf_psnu %>%
                   filter(orgunit_name %in% spdf_ovc_cov$psnu),
                 aes(label = str_replace(orgunit_name, " ", "\n")),
                 size = 4, color = grey10k) +
    geom_sf_text(data = spdf_psnu %>%
                   filter(orgunit_name %ni% spdf_ovc_cov$psnu),
                 aes(label = orgunit_name),
                 size = 3, color = grey90k) +
    scale_fill_manual(
      values = c(
        "ICHSSA 1" = scooter,
        "ICHSSA 2" = moody_blue,
        "ICHSSA 3" = burnt_sienna,
        "ICHSSA 4" = genoa
      )) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank())

  ovc_map

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(meta$curr_pd, " - ",
             str_to_upper(cntry),
             " - OVC PROGRAM COVERAGE",
             ".png")),
    plot = ovc_map,
    width = 10,
    height = 7,
    dpi = 320,
    scale = 1.2)

  ovc_map2 <- bmap +
    geom_sf(data = spdf_ovc_cov %>%
              filter(orgunit_name %ni% c("Kwara")),
            aes(fill = mech_name_new),
            size = .3, color = grey30k) +
    geom_sf(data = spdf_cntry,
            colour = grey10k,
            fill = NA, size = 1.5) +
    geom_sf(data = spdf_cntry,
            colour = grey90k,
            fill = NA, size = .3) +
    geom_sf_text(data = spdf_psnu %>%
                   filter(orgunit_name %in% spdf_ovc_cov$psnu),
                 aes(label = str_replace(orgunit_name, " ", "\n")),
                 size = 4, color = grey10k) +
    geom_sf_text(data = spdf_psnu %>%
                   filter(orgunit_name %ni% spdf_ovc_cov$psnu),
                 aes(label = orgunit_name),
                 size = 3, color = grey90k) +
    scale_fill_manual(
      values = c(
        "THRIVE SOUTH" = scooter,
        "THRIVE NORTH 1" = burnt_sienna,
        "THRIVE NORTH 2" = genoa
      )) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank())

  ovc_map2

  ovc_maps <- (ovc_map + ovc_map2)

  ovc_maps

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(meta$curr_pd, " to FY25 - ",
             str_to_upper(cntry),
             " - OVC PROGRAM COVERAGE Transition",
             ".png")),
    plot = ovc_maps,
    width = 10,
    height = 5,
    dpi = 320,
    scale = 1.2)


  # KP Partners (including FY24 transition) Coverage ----

  kp_map <- bmap +
    geom_sf(data = spdf_kp_cov,
            aes(fill = mech_name),
            size = .3,
            color = grey30k) +
    geom_sf(data = spdf_cntry,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_cntry,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_sf_text(data = spdf_psnu,
                 aes(label = orgunit_name),
                 size = 2,
                 color = grey90k) +
    scale_fill_manual(
      values = c(
        "KP CARE 1" = scooter,
        "KP CARE 2" = moody_blue,
        "USAID Transition" = burnt_sienna,
        "GF Transition" = golden_sand
      )) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank())

  kp_map

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(meta$curr_pd, " - ",
             str_to_upper(cntry),
             " - KP PROGRAM COVERAGE",
             ".png")),
    plot = kp_map,
    width = 10,
    height = 7,
    dpi = 320,
    scale = 1.2)

  # TX Partners

  tx_map <- bmap +
    geom_sf(data = spdf_tx_cov,
            aes(fill = mech_name),
            size = .3,
            color = grey30k) +
    geom_sf(data = spdf_cntry,
            colour = grey10k,
            fill = NA,
            size = 1.5) +
    geom_sf(data = spdf_cntry,
            colour = grey90k,
            fill = NA,
            size = .3) +
    geom_sf_text(data = spdf_psnu,
                 aes(label = orgunit_name),
                 size = 2,
                 color = grey90k) +
    scale_fill_manual(
      values = c(
        "ACE 1" = scooter,
        "ACE 2" = denim,
        "ACE 3" = moody_blue,
        "ACE 4" = burnt_sienna,
        "ACE 5" = golden_sand,
        "ACE 6" = genoa,
        "RISE" = old_rose
      )) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.title = element_blank())

  tx_map

  si_save(
    filename = file.path(
      dir_graphics,
      paste0(meta$curr_pd, " - ",
             str_to_upper(cntry),
             " - TX PROGRAM COVERAGE",
             ".png")),
    plot = tx_map,
    width = 10,
    height = 7,
    dpi = 320,
    scale = 1.2)


