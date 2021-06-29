##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: TX Proxy Linkage
##  LICENCE: MIT
##  DATE:    2021-06-23


# DEPENDENCIES ----

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glitr)
  library(glamr)
  library(gisr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(sf)
  library(ggrepel)
  library(ggnewscale)
  library(patchwork)
  library(glue)
  library(ICPIutilities)

# SETUP ----

  # Directories
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  # MER Data - get the latest MSD PSNU x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v\\d{1}_\\d{1}_N.*.zip$")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_\\d{1}_N.*.zip$")


  # Filters
  fy = 2021
  agency = 'USAID'
  link_inds <- c('TX_NEW', 'HTS_TST_POS')


# FUNCTIONS

  #' @title Linkage Style
  #'
  #' @param gt_tbl GT Table
  #' @param cols   List of column names to format
  #'
  style_linkage <- function(gt_tbl, cols = NULL) {

    # Check colnames
    if (is.null(cols)) {
      cols <- gt_tbl$`_data` %>% names();
    }

    # Apply style to every columns/rows
    for (col in cols) {

      print(glue("{col}: < .85"))

      # Link < .85
      gt_tbl <- gt_tbl %>%
        tab_style(
          style = cell_fill(color = old_rose_light, alpha = 0.75),
          locations = cells_body(
            columns = all_of(col),
            rows = !!sym(col) < .85
          )
        )

      # Link btw .85 & .95
      gt_tbl <- gt_tbl %>%
        tab_style(
          style = cell_fill(color = burnt_sienna_light, alpha = 0.75),
          locations = cells_body(
            columns = all_of(col),
            rows = !!sym(col) >= .85 & !!sym(col) < .95
          )
        )

      # Link >= .95
      gt_tbl <- gt_tbl %>%
        tab_style(
          style = cell_fill(color = scooter_light, alpha = 0.75),
          locations = cells_body(
            columns = all_of(col),
            rows = !!sym(col) >= .95
          )
        )
    }


    # Update tbl structure / style
    gt_tbl <- gt_tbl %>%
      fmt_missing(
        columns = all_of(cols),
        rows = everything(),
        missing_text = "--"
      ) %>%
      cols_align(align = "center") %>%
      tab_style(
        style = list(
          cell_borders(
            sides = c("left", "right"),
            color = grey10k,
            weight = px(1.5)
          ),
          cell_text(
            align = "center"
          )
        ),
        locations = cells_body(
          columns = all_of(cols)
        )
      ) %>%
      tab_options(
        data_row.padding = px(5)
      )

    return(gt_tbl)
  }


  #' @title Format Rows
  #'
  #' @param gt_tbl GT Table
  #' @param format Format cell values as percent, number, etc
  #' @param cols   List of column names to format
  #'
  format_rows <- function(gt_tbl,
                          format = "percent",
                          cols = NULL,
                          ...) {

    if (is.null(cols)) {
      cols <- gt_tbl$`_data` %>% names()
    }

    if (format == "percent") {
      gt_tbl <- gt_tbl %>%
        fmt_percent(columns = all_of(cols), decimals = 0, ...)
    }

    if (format == "number") {
      gt_tbl <- gt_tbl %>%
        fmt_number(columns = all_of(cols), use_seps = TRUE, decimals = 0, ...)
    }

    if (format == "date") {
      gt_tbl <- gt_tbl %>%
        fmt_date(columns = all_of(cols), date_style = 1, ...)
    }

    if (format == "missing") {
      gt_tbl <- gt_tbl %>%
        fmt_missing(columns = all_of(cols), missing_text = "--", ...)
    }

    return(gt_tbl)
  }

  #' @title Header
  #'
  #' @param gt_tbl         GT Table
  #' @param tbl_title      Table Title
  #' @param tbl_subtitle   Table Sub-title
  #'
  table_header <- function(gt_tbl, tbl_title,
                           tbl_subtible = NULL) {

    if (is.null(tbl_subtible)) {
      tbl_subtible <- ""
    }

    gt_tbl <- gt_tbl %>%
      tab_header(title = tbl_title, subtitle = tbl_subtible)

    return(gt_tbl)
  }

  #' @title Footer
  #'
  #' @param gt_tbl GT Table
  #' @param note   Note for the table footer
  #'
  table_footer <- function(gt_tbl, note = "") {
    gt_tbl <- gt_tbl %>%
      tab_source_note(
        source_note = note
      )

    return(gt_tbl)
  }


# LOAD DATA ----

  # Data
  #df_site <- file_site_im %>% read_msd()

  df_psnu <- file_psnu_im %>% read_msd()

  df_psnu <- df_psnu %>%
    clean_agency() %>%
    filter(str_detect(str_to_lower(mech_name), "placeholder", negate = TRUE)) %>%
    mutate(
      mech_name = case_when(
        # USAID
        mech_name == "Meeting Targets and Maintaining Epidemic Control (EpiC)" ~ "EpiC",
        mech_name == "STRENGHTENING INTERGRATED DELIVERY OF HIV/AIDS SERVICES(SIDHAS)" ~ "SIDHAS",
        mech_name == "SHARP Task Order 1" ~ "SHARP TO1",
        mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 2" ~ "SHARP TO2",
        mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 3" ~ "SHARP TO3",
        mech_name == "Reaching Impact, Saturation and Epidemic Control (RISE)" ~ "RISE",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 1)" ~ "ICHSSA 1",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 2)" ~ "ICHSSA 2",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 3)" ~ "ICHSSA 3",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 4)" ~ "ICHSSA 4",
        mech_name == "Care and Treatment in Sustained Support (CaTSS)" ~ "CaTSS",
        mech_name == "Integrated MARPs HIV Prevention Program (IMHIPP)" ~ "IMHIPP",
        mech_name == "Systems Transformed for Empowered Action and Enabling Responses for Vulnerable Children and Families (STEER)" ~ "STEER",
        mech_code == "MSH - Prevention Organisation Systems AIDS Care and Treatment(MSH -ProACT)" ~ "MSH -ProACT",
        mech_code == "Local Partners for Orphans & Vulnerable Children  1" ~ "Local P4OVC 1",
        mech_code == "Local Partners for Orphans & Vulnerable Children  2" ~ "Local P4OVC 2",
        mech_code == "Local Partner for Orphans and Vulnerable Children 3" ~ "Local P4OVC 3",
        # CDC
        mech_name == "Partnering Effectively to end AIDS through Results and Learning (PEARL)_2097" ~ "PEARL",
        mech_name == "Global Action towards HIV Epidemic Control in Subnational units in Nigeria (4GATES PROJECT)_2100" ~ "4GATES",
        mech_name == "ACTION to Control HIV Epidemic through Evidence (ACHIEVE)_2099" ~ "ACHIEVE",
        mech_name == "Improving Comprehensive AIDS Response Enhanced for Sustainability (iCARES)_2098" ~ "iCARES",
        TRUE ~ mech_name
      )
    )

# MUNGING ----

  # Available fields
  df_psnu %>% glimpse()

  df_psnu %>%
    filter(fiscal_year == fy,
           fundingagency == {{agency}}) %>%
    distinct(mech_name) %>% #view()
    pull()

  df_psnu %>%
    filter(mech_name == "MSH - Prevention Organisation Systems AIDS Care and Treatment(MSH -ProACT)")

  # Proxy linkage

  df_link <- df_psnu %>%
    filter(fiscal_year == {{fy}},
           fundingagency == {{agency}},
           indicator %in% {{link_inds}},
           standardizeddisaggregate == 'Total Numerator') %>%
    group_by(psnu, psnuuid, mech_code, mech_name, indicator) %>%
    summarise(across(qtr1:cumulative, sum, na.rm = TRUE)) %>%
    ungroup()

  ## TX Proxy Linkage by Mechs
  df_link %>%
    group_by(mech_code, mech_name, indicator) %>%
    summarise(across(qtr1:cumulative, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    select(mech_code, mech_name, indicator, cumulative) %>%
    pivot_wider(names_from = indicator, values_from = cumulative) %>%
    mutate(linkage = TX_NEW / HTS_TST_POS) %>%
    view()

  # Proxy Llnkages: Mech x Sex & Age
  df_link_sexage <- df_psnu %>%
    filter(fiscal_year == {{fy}},
           fundingagency == {{agency}},
           indicator %in% {{link_inds}},
           standardizeddisaggregate %in% c('Age/Sex/HIVStatus',
                                           'Modality/Age/Sex/Result')) %>%
    group_by(psnu, psnuuid, mech_code, mech_name, sex, ageasentered, indicator) %>%
    summarise(across(qtr1:cumulative, sum, na.rm = TRUE)) %>%
    ungroup()

# VIZ ----

  # Proxy Llnkages: Mech x Sex
  df_link_sexage %>%
    group_by(mech_code, mech_name, sex, indicator) %>%
    summarise(across(qtr1:cumulative, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    select(mech_code, mech_name, sex, indicator, cumulative) %>%
    pivot_wider(names_from = indicator, values_from = cumulative) %>%
    mutate(linkage = TX_NEW / HTS_TST_POS) %>%
    group_by(mech_code, mech_name) %>%
    mutate(linkage_im = sum(TX_NEW, na.rm = TRUE) / sum(HTS_TST_POS, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by() %>%
    mutate(linkage_agency = sum(TX_NEW, na.rm = TRUE) / sum(HTS_TST_POS, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(mech_name = paste0(mech_name, " (", percent(linkage_im, 1), ")")) %>%
    select(-c(mech_code, link_inds, ends_with("_im"), ends_with("_agency"))) %>%
    pivot_wider(names_from = sex, values_from = linkage) %>%
    rowwise() %>%
    mutate(avg = mean(Female, Male)) %>%
    ungroup() %>%
    arrange(desc(avg), mech_name) %>%
    select(-avg) %>%
    gt(rowname_col = "mech_name") %>%
    style_linkage(cols = c("Female", "Male")) %>%
    format_rows(format = "percent", cols = c("Female", "Male")) %>%
    table_header(tbl_title = "NIGERIA - FY21 CUMULATIVE PROXY LINKAGE BY MECHANISM",
                 tbl_subtible = "Most IMs are on track except for SHARP TO2 with Less than 85%") %>%
    table_footer(note = paste("Source: PEPFAR FY21Q2i MSD, Produced on ", Sys.Date())) %>%
    gtsave("Images/Nigeria_ProxyLinckage_by_IM_and_Gender.png")


  # Proxy Llnkages: Mech x Age
  ages <- c("<01", "01-04",
            "05-09", "10-14",
            "15-19", "20-24",
            "25-29", "30-34",
            "35-39", "40-44",
            "45-49", "50+")

  df_link_sexage %>%
    group_by(mech_code, mech_name, ageasentered, indicator) %>%
    summarise(across(qtr1:cumulative, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    select(mech_code, mech_name, ageasentered, indicator, cumulative) %>%
    pivot_wider(names_from = indicator, values_from = cumulative) %>%
    mutate(linkage = TX_NEW / HTS_TST_POS) %>%
    group_by(mech_code, mech_name) %>%
    mutate(linkage_im = sum(TX_NEW, na.rm = TRUE) / sum(HTS_TST_POS, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by() %>%
    mutate(linkage_agency = sum(TX_NEW, na.rm = TRUE) / sum(HTS_TST_POS, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(mech_name = paste0(mech_name, " (", percent(linkage_im, 1), ")")) %>%
    arrange(ageasentered) %>%
    select(-c(mech_code, link_inds, ends_with("_im"), ends_with("_agency"))) %>%
    pivot_wider(names_from = ageasentered, values_from = linkage) %>%
    mutate(linkage_im = str_extract(mech_name, "(?<=\\().*(?=%)"),
           linkage_im = as.integer(linkage_im)) %>%
    arrange(desc(linkage_im), mech_name) %>%
    select(-linkage_im) %>%
    gt(rowname_col = "mech_name") %>%
    style_linkage(cols = ages) %>%
    format_rows(format = "percent", cols = ages) %>%
    table_header(tbl_title = "NIGERIA - FY21 CUMULATIVE PROXY LINKAGE BY AGE GROUPS",
                 tbl_subtible = "SHARP TO2 and TO3 are under performing across age bands") %>%
    table_footer(note = paste("Source: PEPFAR FY21Q2i MSD, Produced on ", Sys.Date())) %>%
    gtsave("Images/Nigeria_ProxyLinckage_by_IM_and_Age.png")


  # Proxy Llnkages: Mech x State
  states <- c("Abia", "Adamawa", "Akwa Ibom",
            "Bauchi", "Bayelsa", "Borno", "Cross River",
            "Edo", "Jigawa", "Kano", "Kebbi",
            "Kwara", "Lagos", "Niger", "Sokoto", "Yobe", "Zamfara")

  df_link_sexage %>%
    filter(str_detect(psnu, "_Mil", negate = TRUE)) %>%
    group_by(mech_code, mech_name, psnu, indicator) %>%
    summarise(across(qtr1:cumulative, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    select(mech_code, mech_name, psnu, indicator, cumulative) %>%
    pivot_wider(names_from = indicator, values_from = cumulative) %>%
    mutate(linkage = TX_NEW / HTS_TST_POS) %>%
    group_by(mech_code, mech_name) %>%
    mutate(linkage_im = sum(TX_NEW, na.rm = TRUE) / sum(HTS_TST_POS, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by() %>%
    mutate(linkage_agency = sum(TX_NEW, na.rm = TRUE) / sum(HTS_TST_POS, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(mech_name = paste0(mech_name, " (", percent(linkage_im, 1), ")")) %>%
    arrange(psnu) %>%
    select(-c(mech_code, link_inds, ends_with("_im"), ends_with("_agency"))) %>%
    pivot_wider(names_from = psnu, values_from = linkage) %>%
    mutate(linkage_im = str_extract(mech_name, "(?<=\\().*(?=%)"),
           linkage_im = as.integer(linkage_im)) %>%
    arrange(desc(linkage_im), mech_name) %>%
    select(-linkage_im) %>%
    gt(rowname_col = "mech_name") %>%
    style_linkage(cols = states) %>%
    format_rows(format = "percent", cols = states) %>%
    table_header(tbl_title = "NIGERIA - FY21 CUMULATIVE PROXY LINKAGE BY STATES",
                 tbl_subtible = "SHARP TO2 is under performing across their states: Bayelsa, Edo and Lagos") %>%
    table_footer(note = paste("Source: PEPFAR FY21Q2i MSD, Produced on ", Sys.Date())) %>%
    gtsave("Images/Nigeria_ProxyLinckage_by_IM_and_State.png")


  # Proxy Llnkages: Mech x State
  sexs <- c("Female", "Male")

  df_link_sexage %>%
    filter(str_detect(psnu, "_Mil", negate = TRUE)) %>%
    group_by(psnu, sex, indicator) %>%
    summarise(across(qtr1:cumulative, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    select(psnu, sex, indicator, cumulative) %>%
    pivot_wider(names_from = indicator, values_from = cumulative) %>%
    mutate(linkage = TX_NEW / HTS_TST_POS) %>%
    group_by(psnu) %>%
    mutate(linkage_psnu = sum(TX_NEW, na.rm = TRUE) / sum(HTS_TST_POS, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by() %>%
    mutate(linkage_agency = sum(TX_NEW, na.rm = TRUE) / sum(HTS_TST_POS, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(psnu = paste0(psnu, " (", percent(linkage_psnu, 1), ")")) %>%
    arrange(psnu) %>%
    select(-c(link_inds, ends_with("_psnu"), ends_with("_agency"))) %>%
    pivot_wider(names_from = sex, values_from = linkage) %>%
    mutate(linkage_psnu = str_extract(psnu, "(?<=\\().*(?=%)"),
           linkage_psnu = as.integer(linkage_psnu)) %>%
    arrange(desc(linkage_psnu), psnu) %>%
    select(-linkage_psnu) %>%
    gt(rowname_col = "psnu") %>%
    style_linkage(cols = sexs) %>%
    format_rows(format = "percent", cols = sexs) %>%
    table_header(tbl_title = "NIGERIA - FY21 CUMULATIVE PROXY LINKAGE BY STATES",
                 tbl_subtible = "A couple of states are under performing for Male") %>%
    table_footer(note = paste("Source: PEPFAR FY21Q2i MSD, Produced on ", Sys.Date())) %>%
    gtsave("Images/Nigeria_ProxyLinckage_by_State_and_Sex.png")


