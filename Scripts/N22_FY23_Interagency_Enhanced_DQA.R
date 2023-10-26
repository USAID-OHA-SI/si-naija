# PURPOSE: si-naija
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: DQA Inquiries
# REF ID:  3c1bb538
# LICENSE: MIT
# DATE:    2023-10-15
# UPDATE:  2023-10-23
# NOTES:   HQ FO Inquiries into USAID/Nigeria Enhanced DQA

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(glamr)
  library(gophr)
  library(glitr)
  library(gisr)
  library(sf)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(janitor)
  library(gt)

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

  file_nat1 <- si_path() %>% return_latest("NAT_SUBNAT_FY15.*")
  file_nat2 <- si_path() %>% return_latest("NAT_SUBNAT_FY21.*")

  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21.*_Nig")

  file_sites1 <- si_path() %>% return_latest("Site_IM_FY15.*_Nig")
  file_sites2 <- si_path() %>% return_latest("Site_IM_FY21.*_Nig")

  file_dqa_sites <- dir_data %>%
    file.path("DQA") %>%
    return_latest("list of DQA.*.xlsx")

  # Meta data

  get_metadata(file_psnu)

  meta <- metadata

  # Set Params

  ref_id <- "3c1bb538"
  agency <- "USAID"
  cntry <- "Nigeria"

  meta$source <- meta$source %>% paste("- Ref. ID =", ref_id)


# Functions  =====

# LOAD DATA =====

  df_nat <- file_nat1 %>%
    c(file_nat2) %>%
    map_dfr(read_psd) %>%
    filter(operatingunit == cntry)

  df_sites <- file_sites1 %>%
    c(file_sites2) %>%
    map_dfr(read_psd)

  df_sites %>% glimpse()

  df_sites %>% distinct(sitetype)

# MUNGE =====

  # HIV

  df_hiv <- df_nat %>%
    filter(operatingunit == cntry,
           indicator == "PLHIV",
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(targets, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnu, indicator)) %>%
    rename(value = targets)

  df_hiv_ou <- df_nat %>%
    filter(operatingunit == cntry, indicator == "PLHIV",
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(targets, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, indicator)) %>%
    rename(value = targets)

  # Agency Sites

  df_agencies <- df_sites %>%
    filter(fiscal_year == meta$curr_fy) %>%
    distinct(funding_agency, psnuuid, psnu, orgunituid, sitename) %>%
    clean_agency()

  # DQA Sites

  df_ss <- file_dqa_sites %>%
    read_excel(sheet = 1) %>%
    clean_names() %>%
    select(site, satellite = x3)

  df_ss <- df_ss %>%
    fill(site, .direction = "down") %>%
    distinct_all() %>%
    mutate(
      site = case_when(
        site == "Mushin OSS" ~ "Mushin KP One Stop Shop",
        site == "Federal Medical Centre Makurdi" ~ "Federal Medical Center - Makurdi",
        str_detect(site, "Rivers\\)") ~ "KPIF Obio-Akpor KP OSS",
        TRUE ~ str_remove(site, ",$")
      )
    ) %>%
    arrange(site, satellite)

  df_ss <- df_ss %>%
    left_join(df_agencies, by = c("site" = "sitename")) %>%
    relocate(funding_agency, psnuuid, psnu, orgunituid, .before = 1) %>%
    arrange(funding_agency, psnu, site, satellite)

  df_ss_locs <- df_ss %>%
    select(-satellite) %>%
    distinct_all()

  # TX

  df_tx <- df_sites %>%
    filter(operatingunit == cntry, str_detect(indicator, "TX_"))

  df_tx_site <- df_tx %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           sitetype %in% c("Facility", "Community")) %>%
    summarise(across(c(cumulative), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnuuid, psnu, orgunituid, sitename, indicator))

  df_tx_psnu <- df_tx %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnu, indicator))

  df_tx_ou <- df_tx %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, indicator))


  df_tx_ou <- df_tx_ou %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW")) %>%
    select(-targets) %>%
    rename(value = cumulative) %>%
    mutate(value = case_when(
      value == 0 ~ NA_integer_,
      TRUE ~ value
    )) %>%
    bind_rows(df_hiv_ou)

  # TX Top10 Sites

  df_tx_site_t10 <- df_tx_site %>%
    filter(indicator %in% c("TX_CURR"),
           fiscal_year == meta$curr_fy) %>%
    arrange(desc(cumulative)) %>%
    filter(row_number() <= 10)

  df_tx_site_hvol <- df_tx_site %>%
    filter(indicator %in% c("TX_CURR"),
           fiscal_year == meta$curr_fy) %>%
    arrange(desc(cumulative)) %>%
    filter(cumulative >= 1000)

  df_tx_site_hvol %>% count(psnu)

  # TX Top10 USAID Sites

  df_tx_usaid_site_t10 <- df_tx %>%
    filter(fiscal_year == meta$curr_fy,
           funding_agency == agency,
           indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           sitetype %in% c("Facility", "Community")) %>%
    summarise(across(c(cumulative), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnuuid, psnu, orgunituid, sitename, indicator)) %>%
    arrange(desc(cumulative)) %>%
    filter(row_number() <= 10)

  # TX Top 10 Historical Trend

  df_tx_usaid_site_t10 %>%
    pull(orgunituid) %>%
    filter(.data = df_tx,
           fiscal_year == meta$curr_fy,
           funding_agency == agency,
           indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           orgunituid %in% .)

  df_tx_usaid_site_t10_hist <- df_tx %>%
    filter(fiscal_year == meta$curr_fy,
           funding_agency == agency,
           indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           orgunituid %in% df_tx_usaid_site_t10$orgunituid) %>%
    summarise(across(c(cumulative), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnuuid, psnu, orgunituid, sitename, indicator)) %>%
    arrange(fiscal_year, desc(cumulative))

  df_tx_usaid_site_t10_hist <- df_tx_usaid_site_t10 %>%
    left_join(df_ss, by = c("psnuuid", "psnu", "orgunituid")) %>%
    arrange(desc(cumulative)) %>%
    mutate(id = row_number()) %>%
    select(id, uid = orgunituid, site) %>%
    #filter(row_number() == 1) %>%
    pmap_dfr(\(id, uid, site) {

      print(uid)

      df_site <- df_tx %>%
        filter(funding_agency == agency,
               indicator == "TX_CURR",
               standardizeddisaggregate == "Total Numerator",
               orgunituid == uid) %>%
        summarise(across(c(cumulative), \(x) sum(x, na.rm = T)),
                  .by = c(fiscal_year, psnuuid, psnu, orgunituid, sitename, indicator)) %>%
        mutate(id = id,
               dqa = case_when(
                 is.na(site) ~ "No",
                 TRUE ~ "Yes"
               ))

      return(df_site)

    }) %>%
    arrange(fiscal_year, id, desc(cumulative))


# VIZ =====

  # Sites

  df_ss %>%
    select(-ends_with("uid")) %>%
    group_by(funding_agency) %>%
    mutate(id = row_number()) %>%
    relocate(id, .before = 1) %>%
    gt() %>%
    sub_missing(missing_text = "-") %>%
    tab_style(locations = cells_body(column = site, rows = psnu == "Akwa Ibom"),
              style = list(cell_text(weight = "bold"))) %>%
    opt_all_caps() %>%
    tab_header(title = "SELECTED SITES FOR FY23 ENHANCED DQA") %>%
    gtsave(filename = file.path(dir_graphics, "DQA Selected Sites.png"))

  # High Volume Sites

  df_ss %>%
    select(-c(funding_agency, satellite)) %>%
    distinct_all() %>%
    left_join(df_tx_site_t10, ., by = c("orgunituid", "psnuuid", "psnu")) %>%
    left_join(df_agencies, by = c("psnuuid", "psnu", "sitename", "orgunituid")) %>%
    select(funding_agency, psnu, sitename, site, tx_curr = cumulative) %>%
    mutate(
      site = case_when(
      is.na(site) ~ "No",
      TRUE ~ "Yes"
    )) %>%
    arrange(desc(tx_curr)) %>%
    mutate(id = row_number()) %>%
    relocate(id, .before = 1) %>%
    gt() %>%
    sub_missing(missing_text = "-") %>%
    fmt_number(columns = tx_curr, sep_mark = ",", drop_trailing_zeros = T) %>%
    tab_style(locations = list(
      cells_body(column = tx_curr),
      cells_body(column = site, row = site == "Yes")),
              style = list(cell_text(weight = "bold"))) %>%
    tab_style(
      locations = list(
        cells_body(column = tx_curr, row = funding_agency == "USAID"),
        cells_body(column = site, row = funding_agency == "USAID")
      ),
      style = list(cell_text(color = usaid_red))) %>%
    opt_all_caps() %>%
    tab_header(title = "HIGH VOLUME SITE SELECTED FOR FY23 ENHANCED DQA") %>%
    gtsave(filename = file.path(dir_graphics, "High Volume sites DQA Selected Sites.png"))

  # USAID - High Volume Sites

  df_ss %>%
    select(-c(funding_agency, satellite)) %>%
    distinct_all() %>%
    left_join(df_tx_usaid_site_t10, ., by = c("orgunituid", "psnuuid", "psnu")) %>%
    left_join(df_agencies, by = c("psnuuid", "psnu", "sitename", "orgunituid")) %>%
    select(funding_agency, psnu, sitename, site, tx_curr = cumulative) %>%
    mutate(
      site = case_when(
        is.na(site) ~ "No",
        TRUE ~ "Yes"
      )) %>%
    arrange(desc(tx_curr)) %>%
    mutate(id = row_number()) %>%
    relocate(id, .before = 1) %>%
    gt() %>%
    sub_missing(missing_text = "-") %>%
    fmt_number(columns = tx_curr, sep_mark = ",", drop_trailing_zeros = T) %>%
    tab_style(locations = list(
      cells_body(column = tx_curr),
      cells_body(column = site, row = site == "Yes")),
      style = list(cell_text(weight = "bold"))) %>%
    tab_style(
      locations = list(
        cells_body(column = site, row = site == "Yes")
      ),
      style = list(cell_text(color = usaid_red))) %>%
    opt_all_caps() %>%
    tab_header(title = "USAID HIGH VOLUME SITE SELECTED FOR FY23 ENHANCED DQA") %>%
    gtsave(filename = file.path(dir_graphics, "USAID - High Volume sites DQA Selected Sites.png"))

  # TX

  tx_range <- range(df_tx_ou$value, na.rm = T)
  tx_pds <- range(df_tx_ou$fiscal_year, na.rm = T)

  plot_tx_trend <- df_tx_ou %>%
    ggplot(aes(x = fiscal_year, y = value)) +
    geom_rect(xmin = 2019, xmax = 2022, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .05) +
    geom_vline(xintercept = c(2019, 2022), color = usaid_darkgrey, size = .6, linetype = "dashed") +
    geom_line(aes(color = indicator, group = indicator), linewidth = 1.5) +
    geom_point(aes(fill = indicator, group = indicator), shape = 21, size = 5, color = grey10k) +
    geom_text(data = df_tx_ou %>% filter(fiscal_year %in% c(2016, 2019, 2022) |
                                         fiscal_year == 2023 & indicator != "PLHIV" |
                                         fiscal_year == tx_pds[2] & indicator == "PLHIV" |
                                         indicator == "PLHIV" & value == tx_range[2]),
              aes(x = fiscal_year, y = value, label = comma(value)),
              vjust = -1.2, hjust = "inward", size = 5, color = usaid_black,
              check_overlap = T) +
    annotate(geom = "text", x = 2020.5, y = 3100000, label = "SURGE", size = 10) +
    scale_color_manual(values = c("PLHIV" = old_rose, "TX_CURR" = scooter, "TX_NEW" = genoa)) +
    scale_fill_manual(values = c("PLHIV" = old_rose, "TX_CURR" = scooter, "TX_NEW" = genoa)) +
    scale_x_continuous(limits = c(tx_pds[1], tx_pds[2]), breaks = tx_pds[1]:tx_pds[2]) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       limits = c(0, tx_range[2] + 1000),
                       breaks = seq(0, tx_range[2] + 1000, 500000)) +
    xlim(2016, max(tx_pds)) +
    coord_cartesian(clip = "off") +
    labs(x = "", y = "",
         caption = glue("{meta$source} - Updated on {curr_date()}"),
         title = "NIGERIA - HIV PROGRAM GROWTH",
         subtitle = glue("# of HIV+ on ARV grew from **700K** to 2M since 2016")
         ) +
    si_style() +
    theme(legend.title = element_blank())

  plot_tx_trend

  si_save(filename = file.path(dir_graphics, "Nigeria - Historical Treatment Growth.png"),
          plot = plot_tx_trend,
          dpi = 320,
          scale = 1.5,
          width = 10,
          height = 5)

  # TX - High Volume sites

  df_tx_usaid_site_t10_hist <- df_tx_usaid_site_t10_hist %>%
    mutate(site_label = paste0(id, " - ", sitename, " (", psnu, ")", ifelse(dqa =="Yes", "**", "")))

  site_order <- df_tx_usaid_site_t10_hist %>%
    filter(fiscal_year == meta$curr_fy) %>%
    arrange(id) %>%
    pull(site_label)

  plot_tx_t10_trend <- df_tx_usaid_site_t10_hist %>%
    ggplot(aes(x = fiscal_year, y = cumulative)) +
    geom_rect(xmin = 2019, xmax = 2022, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .2) +
    geom_vline(xintercept = c(2019, 2022), color = usaid_darkgrey, size = .6, linetype = "dashed") +
    geom_line(aes(group = id), color = scooter, linewidth = 1.5) +
    geom_point(aes(group = id), fill = scooter, shape = 21, size = 4, color = grey10k) +
    geom_text(data = df_tx_usaid_site_t10_hist %>% filter(fiscal_year == meta$curr_fy),
              aes(x = fiscal_year, y = cumulative, label = comma(cumulative)),
              vjust = -.50, hjust = "inward", size = 5, color = usaid_black,
              check_overlap = T) +
    scale_x_continuous(limits = c(tx_pds[1], tx_pds[2] - 1),
                       breaks = tx_pds[1]:(tx_pds[2]-1)) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~factor(site_label, labels = site_order), nrow = 5, dir = "v") +
    coord_cartesian(clip = "off") +
    labs(x = "", y = "") +
    si_style()

  plot_tx_t10_trend

  si_save(filename = file.path(dir_graphics, "Nigeria - Historical Treatment Growth - top 10 sites.png"),
          plot = plot_tx_t10_trend,
          dpi = 310,
          scale = 1.5,
          width = 10,
          height = 5)

# OUTPUTS =====

