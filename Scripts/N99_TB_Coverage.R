##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: TB Achievements
##  LICENCE: MIT
##  DATE:    2021-06-14


# PACKAGES -------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(sf)
  library(raster)
  library(gisr)
  library(janitor)
  library(scales)
  library(ggtext)
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
  # file_msd_sites <- return_latest(
  #     folderpath = dir_merdata,
  #     pattern = "MER_S.*_Site_IM_.*_\\d{8}_v.*_N.*.zip"
  #   )

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

  # MSD PSNU
  df_msd_psnu <- file_msd %>% read_msd()

  df_msd_psnu %>%
    filter(str_detect(indicator, "^TB_|TX_TB|TX_CURR")) %>%
    distinct(indicator, standardizeddisaggregate) %>%
    arrange(indicator) %>%
    prinf()

  df_tb <- df_msd_psnu %>%
    filter(operatingunit == country,
           fiscal_year == rep_fy,
           indicator %in% c("TX_TB", "TB_STAT", "TB_STAT_POS", "TB_ART", "TX_CURR"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
           fundingagency != "Dedup",
           psnuuid != "?") %>%
    reshape_msd(clean = TRUE)

  # TB Coverage
  df_tb_cov <- df_tb %>%
    clean_agency() %>%
    filter(period == rep_pd, period_type == "results") %>%
    mutate(indicator = paste0(indicator, "_", numeratordenom)) %>%
    group_by(fundingagency, psnu, psnuuid, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    clean_names() %>%
    rowwise() %>%
    mutate(tb_stat_cov = tb_stat_n / tb_stat_d,
           tb_art_cov = tb_art_n/tb_stat_pos_n,
           tx_tb_screen = tx_tb_d / tx_curr_n) %>%
    ungroup()

  tb_usaid_psnus <- df_tb_cov %>%
    filter(fundingagency == rep_agency,
           str_detect(psnu, "Military", negate = TRUE)) %>%
    pull(psnu)

  # TB Achivements

  # MSD Geodata
  spdf_tb_cov <- spdf_pepfar %>%
    left_join(df_tb_cov, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(psnu))

  spdf_tb_cov_usaid <- spdf_tb_cov %>%
    filter(fundingagency == rep_agency)

  admin1 <- admin1 %>%
    mutate(tb_state = ifelse(name %in% tb_usaid_psnus, grey10k, grey80k))


# VIZ ----

  # Basemap
  basemap <- terrain_map(countries = cntry,
                         adm0 = admin0,
                         adm1 = admin1,
                         mask = TRUE,
                         terr = terr)

  # TB States
  viz_tb_states <- basemap +
    geom_sf(data = spdf_tb_cov_usaid,
            fill = old_rose, color = grey20k, size = .2, alpha = .3) +
    geom_sf(data = admin0, fill = NA, color = grey10k, size = 1.5, alpha = 0.25) +
    geom_sf(data = admin0, fill = NA, color = grey80k) +
    geom_sf_text(data = admin1,
                 aes(label = name, color = tb_state),
                 size = 10/.pt) +
    scale_color_identity()

  # Save tb output
  si_save(
    filename = file.path(
      dir_graphics,
      glue("{rep_pd} - {str_to_upper(cntry)} - {rep_agency} TB States - {format(Sys.Date(), '%Y%m%d')}.png")),
    plot = viz_tb_states,
    width = 7,
    height = 7,
    scale = 1.4)


  viz_tx_states <- basemap +
    geom_sf(data = spdf_tb_cov_usaid,
            fill = genoa, color = grey20k, size = .2, alpha = .3) +
    geom_sf(data = admin0, fill = NA, color = grey10k, size = 1.5, alpha = 0.25) +
    geom_sf(data = admin0, fill = NA, color = grey80k) +
    geom_sf_text(data = admin1,
                 aes(label = name, color = tb_state),
                 size = 10/.pt) +
    scale_color_identity()

  # Save tx output
  si_save(
    filename = file.path(
      dir_graphics,
      glue("{rep_pd} - {str_to_upper(cntry)} - {rep_agency} TX States - {format(Sys.Date(), '%Y%m%d')}.png")),
    plot = viz_tx_states,
    width = 7,
    height = 7,
    scale = 1.4)


  # TB Stat Coverage
  viz_tb_stat_cov <- basemap +
    geom_sf(data = spdf_tb_cov_usaid,
            aes(fill = tb_stat_cov), color = grey20k, size = .3, alpha = .4) +
    geom_sf(data = admin0, fill = NA, color = grey10k, size = 1.5, alpha = 0.25) +
    geom_sf(data = admin0, fill = NA, color = grey80k) +
    geom_sf_text(data = spdf_tb_cov_usaid,
                 aes(label = paste(name, "\n", percent(tb_stat_cov, 1))),
                 color = grey10k, size = 10/.pt) +
    geom_sf_text(data = admin1 %>% filter(!name %in% tb_usaid_psnus),
                 aes(label = name, color = tb_state), size = 10/.pt) +
    scale_fill_si(palette = "old_roses", discrete = FALSE,
                  limits = c(0, max(spdf_tb_cov_usaid$tb_stat_cov)),
                  labels = percent, alpha = .7) +
    scale_color_identity() +
    si_style_map()

  si_save(
    filename = file.path(
      dir_graphics,
      glue("{rep_pd} - {str_to_upper(cntry)} - {rep_agency} TB STAT Coverage - {format(Sys.Date(), '%Y%m%d')}.png")),
    plot = viz_tb_stat_cov,
    width = 7,
    height = 7,
    scale = 1.4)


  # TB ART Coverage
  viz_tb_art_cov <- basemap +
    geom_sf(data = spdf_tb_cov_usaid,
            aes(fill = tb_art_cov), color = grey10k, size = .3, alpha = .4) +
    geom_sf(data = admin0, fill = NA, color = grey10k, size = 1.5, alpha = 0.25) +
    geom_sf(data = admin0, fill = NA, color = grey80k) +
    geom_sf_text(data = spdf_tb_cov_usaid,
                 aes(label = paste(name, "\n", percent(tb_art_cov, 1))),
                 color = grey10k, size = 10/.pt) +
    geom_sf_text(data = admin1 %>% filter(!name %in% tb_usaid_psnus),
                 aes(label = name, color = tb_state), size = 10/.pt) +
    scale_fill_si(palette = "old_roses", discrete = FALSE,
                  limits = c(0, max(spdf_tb_cov_usaid$tb_art_cov)),
                  labels = percent, alpha = .7) +
    scale_color_identity() +
    si_style_map()

  si_save(
    filename = file.path(
      dir_graphics,
      glue("{rep_pd} - {str_to_upper(cntry)} - {rep_agency} TB ART Coverage - {format(Sys.Date(), '%Y%m%d')}.png")),
    plot = viz_tb_art_cov,
    width = 7,
    height = 7,
    scale = 1.4)



