# PURPOSE: Si Support Nigeria
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: TX_CURR: 8+1+1
# REF ID:  1d0ac5a6
# LICENSE: MIT
# DATE:    2023-08-29
# UPDATE:  2023-08-29
# NOTES:   <special not for tomorrow's self>

# LOCALS & SETUP ============================================================================

  # Libraries

  library(tidyverse)
  library(readxl)
  library(glitr)
  library(glamr)
  library(gisr)
  library(gophr)
  library(grabr)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(janitor)
  library(glue)

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
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21.*_Nigeria")
  file_site <- si_path() %>% return_latest("Site_IM_FY21.*_Nigeria")

  file_bio1 <- dir_cntry %>%
    file.path("NDR") %>%
    return_latest(pattern = "Biometric Validated TX_CURR.*.xlsx", recursive = T)

  file_bio2 <- dir_cntry %>%
    file.path("NDR") %>%
    return_latest(pattern = "Treatment_Q\\d_sum.*.xlsx", recursive = T)

  file_vcp <- dir_shp %>%
    return_latest(pattern = "^VcPepfarPolygons.*.shp$",
                  recursive = TRUE)

  # Metadata

  file_psnu %>% get_metadata()

  meta <- metadata

  # Set Params

  ref_id <- "1d0ac5a6"
  agency <- "USAID"
  cntry <- "Nigeria"

  cntry_uid <- get_ouuid(operatingunit = cntry)

  # Pop & PLHIV Indicators
  inds_hiv <- c("POP_EST", "PLHIV", "TX_CURR_SUBNAT")
  disag_hiv <- c("Age/Sex", "Age/Sex/HIVStatus")

# Functions  =====

# LOAD DATA =====

  # Shapefiles
  sfdf_pepfar <- get_vcpolygons(path = dir_shp)

  ## PLHIV

  df_msd_nat <- file_nat %>%
    read_psd() %>%
    filter(operatingunit == cntry,
           indicator %in% inds_hiv,
           standardizeddisaggregate %in% disag_hiv)

  ## Program Results

  df_msd_psnu <- file_psnu %>%
    read_psd() %>%
    filter(operatingunit == cntry,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus")


  ## Bio validated TX_CURR

  ## FY23 Q1
  file_bio1 %>% excel_sheets()

  df_bio1 <- file_bio1 %>%
    read_excel(sheet = "Sheet1") %>%
    clean_names()

  ## FY23 Q3
  file_bio2 %>% excel_sheets()

  df_bio2 <- file_bio2 %>%
    read_excel(sheet = "Dedup") %>%
    select(1:8) %>%
    rename_with(~str_remove(., "...\\d")) %>%
    rename(TX_CURR = Active)

# MUNGE ====

  # Boundaries

  df_psnus <- df_pops %>%
    filter(str_detect(psnu, "_Mil", negate = T)) %>%
    distinct(psnuuid, psnu)

  sfdf_cntry <- sfdf_pepfar %>%
    filter(uid == cntry_uid)

  sfdf_psnu <- sfdf_pepfar %>%
    left_join(df_psnus, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(psnu))

  # POPs

  df_pops <- df_msd_nat %>%
    summarise(across(targets, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnuuid, psnu, indicator)) %>%
    pivot_longer(cols = targets,
                 names_to = "period",
                 values_to = "value") %>%
    mutate(period_type = "Context",
           period = paste0("FY", str_sub(fiscal_year, 3, 4))) %>%
    select(period, period_type, psnuuid, psnu, indicator, value) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    clean_names() %>%
    filter(period != "FY24")

  # TX Results / Targets

  df_msd_psnu %>% glimpse()

  df_agencies <- df_msd_psnu %>%
    clean_agency() %>%
    distinct(fiscal_year, funding_agency, psnuuid, psnu)

  usaid_states <- df_agencies %>%
    filter(fiscal_year == meta$curr_fy,
           funding_agency == agency,
           psnu %ni% c("Lagos", "_Military Nigeria")) %>%
    pull(psnu)

  df_tx_curr <- df_msd_psnu %>%
    reshape_msd() %>%
    summarise(across(value, \(x) sum(x, na.rm = T)),
              .by = c(period, period_type, psnuuid, psnu, indicator))

  df_tx <- df_tx_curr %>%
    filter(period_type %in% c("targets", "cumulative")) %>%
    pivot_wider(names_from = period_type,
                values_from = value,
                names_prefix = "tx_curr_") %>%
    filter(period != "FY24") %>%
    left_join(df_pops, ., by = c("period", "psnuuid", "psnu"))



  # Bio TX

  df_bio1 %>% glimpse()

  df_bio <- df_bio1 %>%
    mutate(indicator = "TX_CURR", period = "FY23Q1", period_type = "Validation") %>%
    rename(value = tx_curr, state = state_name) %>%
    select(period, period_type, state, indicator, sex, age = age_group, value)

  df_bio <- df_bio %>%
    summarise(value = sum(value, na.rm = T),
              .by = c(period, period_type, state, indicator))

  df_bio2 %>% glimpse()

  df_bio <- df_bio2 %>%
    pivot_longer(cols = where(is.double),
                 names_to = "indicator",
                 values_to = "value") %>%
    mutate(period = "FY23Q3", period_type = "Validation") %>%
    summarise(value = sum(value, na.rm = T),
              .by = c(period, period_type, state_name, indicator)) %>%
    select(period, period_type, state = state_name, indicator, value) %>%
    bind_rows(df_bio, .) %>%
    #filter(indicator == "TX_CURR", state != "_Military") %>%
    group_by(state, indicator) %>%
    mutate(
      value_start = value[period == first(period)],
      value_end = value[period == last(period)]) %>%
    ungroup()


  # Mix Pop, TX and Validations results

  df_tx <- df_bio %>%
    filter(period == "FY23Q1", indicator == "TX_CURR") %>%
    mutate(period = "FY23") %>%
    select(period, state, tx_curr_vqtr1 = value_start, tx_curr_vqtr3 = value_end) %>%
    left_join(df_tx, ., by = c("period", "psnu" = "state"))



# VIZ =====================================================================

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



  # Gaps
  df_tx_gap_viz <- df_tx %>%
    filter(period == meta$curr_fy_lab,  psnu != "_Military") %>%
    group_by(period, psnuuid, psnu) %>%
    mutate(
      psnu_tx_label = paste0(psnu, "\n(", comma(tx_curr_cumulative), ")"),
      psnu_usaid_label = case_when(
        psnu == "Lagos" ~ glue("<span style='color:{usaid_blue}'>**{psnu}**</span>"),
        psnu %in% usaid_states ~ glue("<span style='color:{usaid_blue}'>{psnu}</span>"),
        TRUE ~ glue("<span style='color:{usaid_darkgrey}'>{psnu}</span>")
      ),
      tx_gap = tx_curr_cumulative - plhiv,
      tx_gap2 = tx_curr_vqtr3 - plhiv,
      color_gap = case_when(
        plhiv >= tx_curr_cumulative ~ trolley_grey,
        plhiv < tx_curr_cumulative ~ genoa,
        TRUE ~ trolley_grey_light
      ),
      color_gap2 = case_when(
        tx_gap > 0 ~ trolley_grey,
        tx_gap < 0 ~ usaid_red,
        TRUE ~ trolley_grey
      ),
      color_gap3 = case_when(
        tx_gap2 > 0 ~ trolley_grey,
        tx_gap2 < 0 ~ usaid_red,
        TRUE ~ trolley_grey
      ),
      shape_gap = case_when(
        plhiv >= tx_curr_cumulative ~ 21,
        plhiv < tx_curr_cumulative ~ 22,
        TRUE ~ 0
      ),
      shape_gap_color = case_when(
        plhiv > tx_curr_cumulative ~ usaid_red,
        plhiv <= tx_curr_cumulative ~ trolley_grey,
        TRUE ~ trolley_grey
      ),
      shape_gap_color2 = case_when(
        plhiv > tx_curr_cumulative ~ trolley_grey,
        plhiv <= tx_curr_cumulative ~ usaid_red,
        TRUE ~ trolley_grey
      ),
      shape_gap_color3 = case_when(
        plhiv > tx_curr_vqtr3 ~ usaid_red,
        plhiv <= tx_curr_vqtr3 ~ trolley_grey,
        TRUE ~ trolley_grey
      )
    ) %>%
    ungroup()

  # Maps
  sfdf_tx_gap <- sfdf_psnu %>%
    left_join(df_tx_gap_viz, by = c("uid" = "psnuuid", "psnu"))

  # Viz - Map Gaps

  map_tx_gap1 <- basemap +
    geom_sf(data = sfdf_tx_gap,
            aes(fill = tx_gap),
            lwd = .3, color = trolley_grey_light) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = 1, color = grey90k) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = .3, color = grey10k) +
    geom_sf_text(data = sfdf_tx_gap,
                 aes(label = paste(psnu, "\n", comma(tx_gap))),
                 size = 2.0, fontface = "bold", hjust = .5) +
    scale_fill_si(palette = "carto_div_temps", reverse = T,
                  label = label_number(scale_cut = cut_short_scale()),
                  limits = c(-30000, 65000),
                  breaks = seq(-30000, 65000, 15000)) +
    scale_color_identity() +
    labs(x = "", y = "",
         caption = paste0(meta$caption, " - Viz updated on ", curr_date(), " - Ref. ID: ", ref_id),
         title = "NIGERIA - FY23 TREATMENT GAP AT Q3",
         subtitle = glue("How far are states <span style='color:{genoa};'>**TX_CURR**</span> from their estimated <span style='color:{usaid_red}'>**PLHIV**</span>?")) +
    si_style_map() +
    theme(plot.subtitle = element_markdown(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.key.width = ggplot2::unit(2, "cm"),
          legend.key.height = ggplot2::unit(.5, "cm"))

  map_tx_gap1

  si_save(filename = file.path(dir_graphics, "Nigeria FY23Q3 - Treatment Gap Map v1 8-1-1.png"),
          plot = map_tx_gap1, scale = 1.2, width = 8, height = 6)

  map_tx_gap2 <- basemap +
    geom_sf(data = sfdf_tx_gap,
            aes(fill = tx_gap2),
            lwd = .3, color = trolley_grey_light) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = 1, color = grey90k) +
    geom_sf(data = sfdf_cntry, fill = NA, lwd = .3, color = grey10k) +
    geom_sf_text(data = sfdf_tx_gap,
                 aes(label = paste(psnu, "\n", comma(tx_gap2))),
                 size = 2, fontface = "bold", hjust = .5) +
    scale_fill_si(palette = "carto_div_temps", reverse = T,
                  label = label_number(scale_cut = cut_short_scale()),
                  limits = c(-30000, 65000),
                  breaks = seq(-30000, 65000, 10000)) +
    scale_color_identity() +
    labs(x = "", y = "",
         caption = paste0(meta$caption, " - Viz updated on ", curr_date(), " - Ref. ID: ", ref_id),
         title = "NIGERIA - FY23 TREATMENT GAP AT Q3",
         subtitle = glue("How far are states <span style='color:{genoa};'>**TX_CURR**</span> (biometric validated) from their estimated <span style='color:{usaid_red}'>**PLHIV**</span>?")) +
    si_style_map() +
    theme(plot.subtitle = element_markdown(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.key.width = ggplot2::unit(2, "cm"),
          legend.key.height = ggplot2::unit(.5, "cm"))

  map_tx_gap2

  si_save(filename = file.path(dir_graphics, "Nigeria FY23Q3 - Treatment Gap Map v2 8-1-1.png"),
          plot = map_tx_gap2, scale = 1.2, width = 8, height = 6)

  map_tx_gap <- map_tx_gap1 +
    theme(legend.key.width = ggplot2::unit(3, "cm")) +
    (map_tx_gap2 +
       theme(plot.title = element_blank(),
             plot.subtitle = element_blank(),
             plot.caption = element_blank(),
             legend.position = "None"))

  map_tx_gap

  si_save(filename = file.path(dir_graphics, "Nigeria FY23Q3 - Treatment Gap Map v3 8-1-1.png"),
          plot = map_tx_gap, scale = 1.2, width = 8, height = 6)


  # Viz - Gap
  viz_tx_gap <- df_tx_gap_viz %>%
    ggplot() +
    geom_vline(xintercept = seq(0, 250000, 50000), color = trolley_grey_light) +
    geom_segment(aes(x = plhiv, y = reorder(psnu_usaid_label, tx_curr_cumulative),
                     xend = tx_curr_cumulative, yend = reorder(psnu_usaid_label, tx_curr_cumulative),
                     color = color_gap2),
                 size = 1) +
    geom_point(aes(x = plhiv, y = psnu_usaid_label, fill = shape_gap_color),
               shape = 22, size = 3, color = grey10k, alpha = 1, show.legend = "None") +
    geom_point(aes(x = tx_curr_cumulative, y = psnu_usaid_label),
               shape = 21, size = 3, color = grey10k, fill = genoa, alpha = 1, show.legend = "None") +
    geom_point(aes(x = tx_curr_vqtr3, y = psnu_usaid_label),
               shape = 21, size = 3, color = grey10k, fill = moody_blue, alpha = 1, show.legend = "None") +
    geom_text(data = df_tx_gap_viz %>% filter(tx_gap < 0),
              aes(x = plhiv, y = psnu_usaid_label, label = paste0("(", comma(abs(tx_gap)), ")")),
              hjust = -.3) +
    scale_size_continuous(range = c(3, 10)) +
    scale_x_continuous(position = "top", label = comma) +
    #scale_y_discrete(labels = df_tx_gap_viz %>% arrange(tx_curr_cumulative, psnu) %>% pull(psnu_usaid_label)) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_shape_identity() +
    labs(
      x = "", y = "",
      caption = paste0(meta$caption, " - Viz updated on ", curr_date(), " - Ref. ID: ", ref_id),
      title = "NIGERIA - FY23 TREATMENT GAP AT Q3",
      subtitle = glue("How far are states <span style='color:{genoa};'>**TX_CURR**</span> from their estimated <span style='color:{usaid_red}'>**PLHIV**</span>?<br>Note: <span style='color:{moody_blue}'>**TX_CURR**</span> (biometric validated)")
    ) +
    si_style() +
    theme(plot.subtitle = element_markdown(),
          axis.text.y = element_markdown())

  viz_tx_gap

  si_save(filename = file.path(dir_graphics, "Nigeria FY23Q3 - Treatment Gap 8-1-1.png"),
          plot = viz_tx_gap, scale = 1.2, width = 10, height = 7)

  viz_tx_gap2 <- df_tx_gap_viz %>%
    ggplot() +
    geom_vline(xintercept = seq(-30000, 60000, 10000), color = trolley_grey_light) +
    geom_segment(aes(x = 0, y = reorder(psnu_usaid_label, tx_gap),
                     xend = tx_gap, yend = reorder(psnu_usaid_label, tx_gap),
                     color = color_gap2),
                 size = 1) +
    geom_point(aes(x = 0, y = psnu_usaid_label, fill = shape_gap_color),
               shape = 22, color = grey10k, size = 3, alpha = 1, show.legend = "None") +
    geom_point(aes(x = tx_gap, y = psnu_usaid_label),
               shape = 21, color = grey10k, size = 3, fill = genoa, alpha = 1, show.legend = "None") +
    geom_point(aes(x = tx_gap2, y = psnu_usaid_label),
               shape = 21, color = grey10k, size = 3, fill = moody_blue, alpha = 1, show.legend = "None") +
    geom_text(data = df_tx_gap_viz %>% filter(tx_gap < 0),
              aes(x = 0, y = psnu_usaid_label, label = paste0("(", comma(abs(tx_gap)), ")")),
              hjust = -.3) +
    geom_text(data = df_tx_gap_viz %>% filter(tx_gap >= 0),
              aes(x = 0, y = psnu_usaid_label, label = comma(tx_gap)),
              hjust = 1.3) +
    scale_size_continuous(range = c(3, 10)) +
    scale_x_continuous(position = "top", label = comma) +
    #scale_y_discrete(labels = df_tx_gap_viz %>% arrange(tx_curr_cumulative, psnu) %>% pull(psnu_usaid_label)) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_shape_identity() +
    labs(
      x = "", y = "",
      caption = paste0(meta$caption, " - Viz updated on ", curr_date(), " - Ref. ID: ", ref_id),
      title = "NIGERIA - FY23 TREATMENT GAP AT Q3",
      subtitle = glue("How far are states <span style='color:{genoa};'>**TX_CURR**</span> from their estimated <span style='color:{usaid_red}'>**PLHIV**</span>? Over-reporting or under-estimation of PLHIV?<br>Note: <span style='color:{moody_blue}'>**TX_CURR**</span> (biometric validated)")
    ) +
    si_style() +
    theme(plot.subtitle = element_markdown(),
          axis.text.y = element_markdown())

  viz_tx_gap2

  si_save(filename = file.path(dir_graphics, "Nigeria FY23Q3 - Treatment Gap v2 8-1-1.png"),
          plot = viz_tx_gap2, scale = 1.2, width = 10, height = 7)


  viz_tx_gap3 <- df_tx_gap_viz %>%
    ggplot() +
    geom_vline(xintercept = seq(-30000, 60000, 10000), color = trolley_grey_light) +
    geom_segment(aes(x = 0, y = reorder(psnu_usaid_label, tx_gap2),
                     xend = tx_gap2, yend = reorder(psnu_usaid_label, tx_gap2),
                     color = color_gap3),
                 size = 1) +
    geom_point(aes(x = 0, y = psnu_usaid_label, fill = shape_gap_color3),
               shape = 22, color = grey10k, size = 3, alpha = 1, show.legend = "None") +
    geom_point(aes(x = tx_gap2, y = psnu_usaid_label),
               shape = 21, color = grey10k, size = 3, fill = genoa, alpha = 1, show.legend = "None") +
    geom_text(data = df_tx_gap_viz %>% filter(tx_gap2 < 0),
              aes(x = 0, y = psnu_usaid_label, label = paste0("(", comma(abs(tx_gap2)), ")")),
              hjust = -.3) +
    geom_text(data = df_tx_gap_viz %>% filter(tx_gap2 >= 0),
              aes(x = 0, y = psnu_usaid_label, label = comma(tx_gap2)),
              hjust = 1.3) +
    scale_size_continuous(range = c(3, 10)) +
    scale_x_continuous(position = "top", label = comma) +
    #scale_y_discrete(labels = df_tx_gap_viz %>% arrange(tx_gap2, psnu) %>% pull(psnu_usaid_lebel)) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_shape_identity() +
    labs(
      x = "", y = "",
      caption = paste0(meta$caption, " - Viz updated on ", curr_date(), " - Ref. ID: ", ref_id),
      title = "NIGERIA - FY23 TREATMENT GAP AT Q3",
      subtitle = glue("How far are states <span style='color:{genoa};'>**TX_CURR**</span> (Biometric validated) from their estimated <span style='color:{usaid_red}'>**PLHIV**</span>? Over-reporting or under-estimation of PLHIV?")
    ) +
    si_style() +
    theme(plot.subtitle = element_markdown(),
          axis.text.y = element_markdown())

  viz_tx_gap3

  si_save(filename = file.path(dir_graphics, "Nigeria FY23Q3 - Treatment Gap v3 8-1-1.png"),
          plot = viz_tx_gap3, scale = 1.2, width = 10, height = 7)


  # Bio changes

  df_bio %>%
    filter(indicator == "TX_CURR", state != "_Military") %>%
    ggplot() +
    geom_segment(aes(x = value_start, y = reorder(state, value_start),
                     xend = value_end, yend = state),
                 size = 1) +
    geom_point(aes(x = value, y = state,
                   size = value, fill = period),
               shape = 21, color = grey10k, alpha = 1) +
    scale_size_continuous(range = c(4, 10))

# OUTPUTS =================================================================

