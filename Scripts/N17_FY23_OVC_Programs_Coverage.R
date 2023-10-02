# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam | SI
# Ref. ID: 49396ce0
# LICENSE: MIT
# DATE: 2023-08-21
# NOTES:

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(janitor)
    library(glue)
    library(here)
    library(gt)

  # Dependencies

    source("./Scripts/N00_Utilities.R")
    source("./Scripts/N00_Viz_Utilities.R")

  # Set paths

    dir_data   <- "Data"
    dir_dataout <- "Dataout"
    dir_images  <- "Images"
    dir_graphs  <- "Graphics"

    dir_mer <- glamr::si_path("path_msd")
    dir_ras <- glamr::si_path("path_raster")
    dir_shp <- glamr::si_path("path_vector")

  # Load files

    file_vcp <- dir_shp %>%
      return_latest(pattern = "^VcPepfarPolygons.*.shp$",
                    recursive = TRUE)

    file_nat <- dir_mer %>%
      return_latest(
        folderpath = .,
        pattern = "MER_.*_NAT_SUBNAT_.*"
      )

    file_psnu_im <- dir_mer %>%
      return_latest(
        folderpath = .,
        pattern = "MER_.*_PSNU_IM_FY21.*_Nigeria"
      )

  # Parameters

    ref_id <- Sys.time() |> digest::sha1() |> substr(start = 1, stop = 8)

    agency <- "USAID"

    cnty <- "Nigeria"

    cntry_uid <- get_ouuid(operatingunit = cntry)

    get_metadata(path = file_psnu_im, type = "PSNU_IM")

    meta <- metadata # this is loaded as a global var

    walk(names(meta), \(x) print(glue::glue("{x}: {meta[x]}")))

    caption = glue("{meta$caption} | USAID/OHA/SIEI | Ref id: {ref_id}")

  # Functions


# LOAD DATA ============================================================================

  sfdf_pepfar <- get_vcpolygons(path = dir_shp)

  df_msd <- file_psnu_im %>% read_psd()

  df_nat <- file_nat %>% read_psd()


# MUNGE ============================================================================

  # Boundaries

  df_psnus <- df_msd %>%
    filter(fiscal_year == meta$curr_fy,
           str_detect(psnu, "_mil", negate = T)) %>%
    distinct(psnuuid, psnu)

  sfdf_cntry <- sfdf_pepfar %>%
    filter(uid == cntry_uid)

  sfdf_psnu <- sfdf_pepfar %>%
    left_join(df_psnus, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(psnu))

  # CLHIV
  df_nat %>% glimpse()

  df_hiv <- df_nat %>%
    filter(operatingunit == cntry,
           psnu %in% df_cov$psnu,
           fiscal_year == meta$curr_fy,
           indicator == "PLHIV",
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    mutate(
      agecoarse = case_when(
        ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "<15",
        ageasentered == "15-19" ~ "15-19",
        TRUE ~ "20+"
      )
    ) %>%
    summarise(across(targets, \(x) sum(x, na.rm = T)),
              .by = c(psnuuid, psnu, agecoarse)) %>%
    rename(plhiv = targets)

  df_hiv <- df_hiv %>%
    filter(agecoarse != "20+") %>%
    summarise(across(plhiv, \(x) sum(x, na.rm = T)),
              .by = c(psnuuid, psnu)) %>%
    mutate(agecoarse = "<20") %>%
    bind_rows(df_hiv, .) %>%
    filter(agecoarse != "20+") %>%
    mutate(agecoarse = str_replace(agecoarse, "<", "u"),
           agecoarse = str_replace(agecoarse, "[+]", "plus"),
           agecoarse = paste0("plhiv_", agecoarse)) %>%
    pivot_wider(
      names_from = agecoarse,
      values_from = plhiv
    ) %>%
    clean_names()

  # MSD - Extracts only PSNU with OVC Indicators

  df_msd %>% glimpse()

  # TX CURR
  df_tx <- df_msd %>%
    filter(operatingunit == cntry,
           psnu %in% df_cov$psnu,
           fiscal_year == meta$curr_fy,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus"
    ) %>%
    mutate(
      agecoarse = case_when(
        ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "<15",
        ageasentered == "15-19" ~ "15-19",
        TRUE ~ "20+"
      )
    ) %>%
    summarise(across(qtr2, \(x) sum(x, na.rm = T)),
              .by = c(psnuuid, psnu, agecoarse)) %>%
    rename(tx_curr = qtr2)

  df_tx <- df_tx %>%
    filter(agecoarse != "20+") %>%
    summarise(across(tx_curr, \(x) sum(x, na.rm = T)),
              .by = c(psnuuid, psnu)) %>%
    mutate(agecoarse = "<20") %>%
    bind_rows(df_tx, .) %>%
    filter(agecoarse != "20+") %>%
    mutate(agecoarse = str_replace(agecoarse, "<", "u"),
           agecoarse = str_replace(agecoarse, "[+]", "plus"),
           agecoarse = paste0("tx_curr_qtr2_", agecoarse)) %>%
    pivot_wider(
      names_from = agecoarse,
      values_from = tx_curr
    ) %>%
    clean_names()


  df_art <- df_hiv %>%
    left_join(df_tx, by = c("psnuuid", "psnu")) %>%
    mutate(art_coverage_u15 = tx_curr_qtr2_u15 / plhiv_u15,
           art_coverage_u20 = tx_curr_qtr2_u20 / plhiv_u20) %>%
    arrange(desc(art_coverage_u20))

  # OVC

  df_ovc <- df_msd %>%
    filter(funding_agency == agency,
           operatingunit == cntry,
           fiscal_year == meta$curr_fy,
           str_detect(indicator, "OVC"))

  df_ovc %>% distinct(indicator)

  df_ovc %>%
    distinct(indicator, standardizeddisaggregate,
             otherdisaggregate, statushiv, source_name) %>%
    arrange(indicator, standardizeddisaggregate)

  # OVC HIVSTAT

  df_ovc %>%
    filter(
      indicator == "OVC_HIVSTAT",
      standardizeddisaggregate == "Age/Sex/ReportedStatus",
      statushiv == "Positive"
    )

  df_ovcstat <- df_ovc %>%
    filter(
      indicator == "OVC_HIVSTAT",
      standardizeddisaggregate == "Age/Sex/ReportedStatus",
      statushiv == "Positive"
      #indicator == "OVC_HIVSTAT_POS",
      #standardizeddisaggregate == "Total Numerator",
    ) %>%
    mutate(agecoarse = case_when(
      ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "<15",
      ageasentered == "15-17" ~ "15-17",
      ageasentered == "Unknown Age" ~ "No-Age",
      TRUE ~ NA_character_
    )) %>%
    summarise(across(qtr2, \(x) sum(x, na.rm = T)),
              .by = c(psnuuid, psnu, agecoarse)) %>%
    rename(ovc_hivstat = qtr2)

  df_ovcstat <- df_ovcstat %>%
    summarise(across(ovc_hivstat, \(x) sum(x, na.rm = T)),
              .by = c(psnuuid, psnu)) %>%
    mutate(agecoarse = "<18") %>%
    bind_rows(df_ovcstat, .) %>%
    pivot_wider(names_from = agecoarse,
                values_from = ovc_hivstat,
                names_prefix = "ovc_hivstat_") %>%
    rename_with(.fn = ~str_replace(., "<", "u")) %>%
    clean_names() %>%
    relocate(ovc_hivstat_u15, .after = psnu)

  # OVC SERV

  df_ovcserv <- df_ovc %>%
    filter(
      indicator %in% c("OVC_SERV_UNDER_18", "OVC_SERV_OVER_18"),
      standardizeddisaggregate == "Total Numerator"
    ) %>%
    summarise(across(cumulative, \(x) sum(x, na.rm = T)),
              .by = c(psnuuid, psnu, indicator)) %>%
    rename(ovc_serv = cumulative)

  df_ovcserv <- df_ovcserv %>%
    summarise(across(ovc_serv, \(x) sum(x, na.rm = T)),
              .by = c(psnuuid, psnu)) %>%
    mutate(indicator = "OVC_SERV_ALL") %>%
    bind_rows(df_ovcserv, .)

  df_ovcserv <- df_ovcserv %>%
    pivot_wider(names_from = indicator,
                values_from = ovc_serv) %>%
    rename_with(.fn = ~str_to_lower(.)) %>%
    clean_names()


  # OVC IP Coverage
  df_cov <- df_ovc %>%
    distinct(psnuuid, psnu, mech_code, mech_name, prime_partner_name) %>%
    clean_mechs() %>%
    left_join(df_art, by = c("psnuuid", "psnu")) %>%
    left_join(df_ovcstat, by = c("psnuuid", "psnu")) %>%
    left_join(df_ovcserv, by = c("psnuuid", "psnu")) %>%
    mutate(
      ovc_coverage_u15 = ovc_hivstat_u15 / tx_curr_qtr2_u15,
      ovc_coverage_u18 = ovc_hivstat_u18 / tx_curr_qtr2_u20
    )

  sfdf_ovc <- sfdf_psnu %>%
    left_join(df_cov, by = c("uid" = "psnuuid", "psnu" = "psnu")) %>%
    filter(!is.na(mech_code)) %>%
    mutate(
      mech_color = case_when(
        mech_name == "ICHSSA 1" ~ denim,
        mech_name == "ICHSSA 2" ~ moody_blue,
        mech_name == "ICHSSA 3" ~ scooter,
        mech_name == "ICHSSA 4" ~ genoa,
        TRUE ~ trolley_grey_light
      ),
      label_ovc_serv = case_when(
        ovc_serv_all >= 80000 ~ grey10k,
        TRUE ~ grey90k
      ),
      label_ovc_cov = case_when(
        ovc_coverage_u15 >= .8 ~ grey10k,
        TRUE ~ grey90k
      ))

  # Customise psnu label colors
  sfdf_psnu <- sfdf_psnu %>%
    mutate(label_color = case_when(
      psnu %in% df_cov$psnu ~ grey10k,
      TRUE ~ grey90k
    ))



# VIZ ============================================================================

  # Review boundaries
  sfdf_cntry %>% gview()
  sfdf_psnu %>% gview()

  # Basemap
  terr <- get_terrain(countries = sfdf_cntry, mask = TRUE)

  basemap <- terrain_map(countries = sfdf_cntry,
                         adm0 = sfdf_cntry,
                         adm1 = sfdf_psnu,
                         mask = TRUE,
                         terr = dir_ras)

  basemap


  # IP Coverage by State
  map_ovc_ip <- basemap +
    geom_sf(data = sfdf_ovc, lwd = .3, fill = moody_blue_light, color = trolley_grey_light) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = 1, color = grey90k) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = .3, color = grey10k) +
    geom_sf_text(data = sfdf_psnu, aes(label = psnu), color = grey80k, size = 2.5) +
    facet_wrap(~mech_name, ncol = 2)

  map_ovc_ip

  ggsave(filename = here(dir_graphs, "Negeria - FY23 OVC IPs Coverage by State.png"),
         plot = map_ovc_ip,
         scale = 1.2, dpi = 310,
         width = 10, height = 7,
         units = "in")

  # IP Coverage by State 2
  map_ovc_ips <- basemap +
    geom_sf(data = sfdf_ovc, aes(fill = mech_name), lwd = .3, color = trolley_grey_light) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = 1, color = grey90k) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = .3, color = grey10k) +
    geom_sf_text(data = sfdf_psnu,
                 aes(label = psnu, color = label_color),
                 size = 3, fontface = "bold") +
    scale_fill_manual(values = c("ICHSSA 1" = denim,
                             "ICHSSA 2" = moody_blue,
                             "ICHSSA 3" = scooter,
                             "ICHSSA 4" = genoa)) +
    scale_color_identity() +
    labs(x = "", y = "", caption = caption) +
    si_style_map() +
    theme(legend.position = "top", legend.title = element_blank())

  map_ovc_ips

  ggsave(filename = here(dir_graphs, "Negeria - FY23 OVC IPs Coverage by state 2.png"),
         plot = map_ovc_ips,
         scale = 1.2, dpi = 310,
         width = 10, height = 7,
         units = "in")

  # OVC SERV
  map_ovc_serv <- basemap +
    geom_sf(data = filter(sfdf_ovc, psnu != "Kwara"),
            aes(fill = ovc_serv_all),
            lwd = .3, color = trolley_grey_light) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = 1, color = grey90k) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = .3, color = grey10k) +
    geom_sf_text(data = filter(sfdf_ovc, psnu != "Kwara"),
                 aes(label = paste(psnu, "\n", comma(ovc_serv_all)),
                     color = label_ovc_serv),
                 size = 3, fontface = "bold") +
    scale_fill_si(labels = comma,
                  limits = c(0, max(sfdf_ovc$ovc_serv_all)),
                  breaks = c(1000, 10000, 25000, 50000, 75000, 100000, 125000)) +
    scale_color_identity() +
    labs(x = "", y = "", caption = caption) +
    si_style_map() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.key.width = ggplot2::unit(3.5, "cm"),
          legend.key.height = ggplot2::unit(.5, "cm"))

  map_ovc_serv

  ggsave(filename = here(dir_graphs, "Negeria - FY23 OVC SERV Distribution by state.png"),
         plot = map_ovc_serv,
         scale = 1.2, dpi = 310,
         width = 7, height = 7,
         units = "in")

  df_cov %>%
    filter(psnu != "Kwara") %>%
    select(mech_name, psnu, starts_with("ovc_serv")) %>%
    arrange(mech_name, desc(ovc_serv_all)) %>%
    rename_with(.cols = starts_with("ovc_serv"),
                .fn = ~str_remove(., "ovc_serv_")) %>%
    rename_with(.cols = everything(),
                .fn = ~str_to_upper(.)) %>%
    group_by(MECH_NAME) %>%
    gt() %>%
    sub_missing(
      columns = 3:last_col(),
      missing_text = "-"
    ) %>%
    fmt_integer(columns = 3:last_col(), rows = everything())


  # OVC STAT / COV

  # Under 15
  map_ovc_cov_u15 <- basemap +
    geom_sf(data = sfdf_ovc,
            aes(fill = ovc_coverage_u15),
            lwd = .3, color = trolley_grey_light) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = 1, color = grey90k) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = .3, color = grey10k) +
    geom_sf_text(data = sfdf_ovc,
                 aes(label = paste(psnu, "\n", percent(ovc_coverage_u15)),
                     color = label_ovc_cov),
                 size = 3, fontface = "bold") +
    scale_fill_si(palette = "burnt_siennas", labels = percent,
                  limits = c(0, 1.5),
                  breaks = c(.1, .25, .50, .75, 1, 1.25, 1.5)) +
    scale_color_identity() +
    labs(x = "", y = "", caption = caption) +
    si_style_map() +
    theme(legend.position = "top", legend.title = element_blank(),
          legend.key.width = ggplot2::unit(3.5, "cm"),
          legend.key.height = ggplot2::unit(.5, "cm"))

  map_ovc_cov_u15

  ggsave(filename = here(dir_graphs, "Negeria - FY23 OVC Coverage u15 by state.png"),
         plot = map_ovc_cov_u15,
         scale = 1.2, dpi = 310,
         width = 7, height = 7,
         units = "in")

  # Under 18

  map_ovc_cov_u18 <- basemap +
    geom_sf(data = sfdf_ovc,
            aes(fill = ovc_coverage_u18),
            lwd = .3, color = trolley_grey_light) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = 1, color = grey90k) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = .3, color = grey10k) +
    geom_sf_text(data = sfdf_ovc,
                 aes(label = paste(psnu, "\n", percent(ovc_coverage_u18)),
                     color = label_ovc_cov),
                 size = 3, fontface = "bold") +
    scale_fill_si(palette = "burnt_siennas", labels = percent,
                  limits = c(0, 1.1),
                  breaks = c(.1, .25, .50, .75, 1, 1.1)) +
    scale_color_identity() +
    labs(x = "", y = "", caption = caption) +
    si_style_map() +
    theme(legend.position = "top", legend.title = element_blank(),
          legend.key.width = ggplot2::unit(3.5, "cm"),
          legend.key.height = ggplot2::unit(.5, "cm"))

  map_ovc_cov_u18

  ggsave(filename = here(dir_graphs, "Negeria - FY23 OVC Coverage u18 by state.png"),
         plot = map_ovc_cov_u18,
         scale = 1.2, dpi = 310,
         width = 7, height = 7,
         units = "in")

# SPINDOWN ============================================================================


  ## Outputs

  df_cov %>%
    rename(state = psnu, state_uid = psnuuid) %>%
    rename_with(.fn = str_to_upper) %>%
    write_csv(file = here(dir_dataout, "Negeria - FY23 OVC Programs Coverage.csv"))

  df_ovcserv %>%
    write_csv(file = here(dir_dataout, "Negeria - FY23 Cumulative OVC SERV.csv"))

