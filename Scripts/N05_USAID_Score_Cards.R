##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: USAID Quarterly Score Cards
##  LICENCE: MIT
##  DATE:    2021-10-22
##  UPDATED: 2021-11-12

# PACKAGES ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glitr)
  library(glamr)
  library(sf)
  library(gisr)
  library(janitor)
  library(glue)
  library(gt)
  library(ggtext)
  library(scales)
  library(patchwork)
  library(grid)
  library(waffle)
  #library(ggwaffle)

  library(hrbrthemes)

  library(fontawesome)
  library(emojifont)
  library(extrafont)

  # font_import(path = si_path("path_downloads"),
  #             pattern = "fa-",
  #             prompt =  FALSE)
  #

  library(showtext)

  # font_add(family = "FontAwesome5Free-Solid", regular = paste0(si_path("path_downloads"), "fa-solid-900.ttf"))
  # font_add(family = "FontAwesome5Free-Regular", regular = paste0(si_path("path_downloads"), "fa-regular-400.ttf"))
  # font_add(family = "FontAwesome5Brands-Regular", regular = paste0(si_path("path_downloads"), "fa-brands-400.ttf"))

  showtext_auto()

  fonts()

# GLOBAL ----

  # DIR - Global ----
  dir_merdata <- glamr::si_path("path_msd")
  dir_geodata <- si_path("path_vector")
  dir_raster <- si_path("path_raster")
  dir_targets <- "../../PEPFAR/COUNTRIES/Nigeria/DataPack"

  # DIR - Project ----
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"

  dir_graphics <- "Graphics"

  # Files ----
  file_msd_sites <- return_latest(
    folderpath = dir_merdata,
    pattern = "Site_IM.*_N.*"
  )

  file_msd_psnu <- return_latest(
    folderpath = dir_merdata,
    pattern = "PSNU_IM.*_N.*"
  )

  file_msd_nat <- return_latest(
    folderpath = dir_merdata,
    pattern = "NAT_SUBNAT.*"
  )

  file_shp <- return_latest(
    folderpath = dir_geodata,
    pattern = "VcPepfarPolygons*"
  )

  # Other Params ----
  cntry <- "Nigeria"
  agency <- "USAID"

  sname <- "Akwa Ibom"


  # indicators
  inds <- c("TX_CURR", "TX_PVLS")

  age_children <- c("<01", "01-04", "05-09", "10-14")
  age_youth <- c("15-19", "20-24")
  age_adults <- c("25-49", "50+")

  # Age/Sex Classification
  cat_levels <- c("Children", "Adult Female", "Adult Male")

# FUNCTIONS ----

  #' @title Theme Transparent
  #'
  #'
  theme_transparent <- function() {
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent', color = NA),
      legend.box.background = element_rect(fill='transparent', color = NA)
    )
  }


  #' @title Viz - fa-icons
  #'
  #'
  fa_icons <- function(icon = "fa-users",
                       fsize = 100,
                       fcolor = "red",
                       label = NULL,
                       lsize = 20,
                       lcolor = "#212721", # usaid_black
                       loc = "bottom",
                       save_as = NULL,
                       iwidth = 1,
                       iheight = 1) {

    # Placement Guide
    df_pos <- tibble(
      pos   = c("top", "right", "bottom", "left"),
      x     = c(0,     1.25,    0,    -1.25),
      y     = c(1,     0,       -1.25,    -0),
      hjust = c(0.5,   0,       0.5,      1),
      vjust = c(0,     0.5,     1,        0.5)
    )

    df_pos_sel <- df_pos %>% filter(pos == loc)

    viz_icon <- ggplot() +
      geom_rect(aes(xmin = -.75, xmax = .75, ymin = -1, ymax = .75),
                fill = NA, color = NA) +
      geom_rect(aes(xmin = -1.25, xmax = 1.25, ymin = -1.25, ymax = 1),
                fill = NA, color = NA) +
      geom_text(aes(0, 0),
                label = fontawesome(icon),
                family = "fontawesome-webfont",
                size = fsize,
                color = fcolor)

    # Label
    if (!is.null(label)) {

      viz_icon <- viz_icon +
        geom_text(aes(df_pos_sel$x, df_pos_sel$y),
                  label = label,
                  size = lsize,
                  color = fcolor,
                  hjust = df_pos_sel$hjust,
                  vjust = df_pos_sel$vjust)

    }

    #Theme
    viz_icon <- viz_icon +
      coord_equal(clip = "off") +
      theme_void() +
      theme(text = element_text(family = "Source Sans Pro")) +
      theme_transparent()

    if (!is.null(save_as)) {
      ggsave(filename = save_as,
             plot = viz_icon,
             dpi = 320,
             width = iwidth,
             height = iheight,
             bg = "transparent")
    }

    return(viz_icon)
  }


  #' @title Viz - Donut partion
  #'
  #'
  viz_portion <- function(.data,
                          cat = "Children",
                          icon = NULL) {

    # Filter and reshape data
    .data <- .data %>%
      filter(category == {{cat}}) %>%
      pivot_longer(
        names_to = "name",
        values_to = "value",
        cols = c(vals, diff)
      ) %>%
      mutate(
        color = case_when(
          name == 'diff' ~ grey40k, TRUE ~ color),
        names = case_when(
          name == 'vals' ~ paste0(value, "%"), TRUE ~ "")
      )

    # Validate data
    if (nrow(.data) == 0) return(NULL)

    # label
    val_label <- unique(.data$cumulative)

    # Color
    val_color <- .data %>%
      filter(name == "vals") %>%
      pull(color)

    # Icon
    if (!is.null(icon)) {
      icon <- icon
    }
    else if ("icon" %in% names(.data)) {
      icon <- unique(.data$icon)
    }
    else {
      icon <- case_when(
        cat == "Children" ~ "fa-child",
        cat == "Adult Female" ~ "fa-female",
        cat == "Adult Male" ~ "fa-male",
        TRUE ~ "fa-bar-chart"
      )
    }

    # Viz
    viz <- .data %>%
      ggplot(aes(x = 2, y = value, fill = color))+
      geom_col(show.legend = FALSE) +
      coord_polar(theta = "y", start = 0, direction = 1) +
      xlim(c(.2, 2.5)) +
      geom_text(aes(y = value, label = names),
                position = position_stack(vjust = .5),
                size = 9, color = "white", fontface = "bold") +
      geom_text(aes(x = 0.3, y = 1),
                label = fontawesome(icon),
                family = 'fontawesome-webfont',
                size = 35, color = val_color, show.legend = F) +
      geom_text(aes(x = 1.15, y = 0),
                label = comma(val_label), color = val_color,
                size = 9, fontface = "bold", show.legend = F) +
      scale_fill_identity() +
      theme_void() +
      theme(plot.title = element_text(hjust = .5),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent', color = NA),
            legend.box.background = element_rect(fill='transparent', color = NA))

    return(viz)
  }

  #' @title VIZ - Donut Portions
  #'
  #'
  viz_portions <- function(.data, cat = NULL, icon = "fa-users") {
    # Show only 1 portion
    if (!is.null(cat)) {
      viz <- viz_portion(.data, cat = cat)
      return(viz)
    }

    ttl_label <- comma(sum(.data$cumulative))

    # Show all portions
    .data %>%
      ggplot(aes(x = 2, y = vals, fill = color))+
      geom_col(show.legend = F)+
      coord_polar(theta = "y", start = 0, direction = 1) +
      xlim(c(.2, 2.5)) +
      geom_text(aes(y = vals, label = str_replace_all(names, " ", "\n")),
                position = position_stack(vjust = .5),
                color = "white", fontface = "bold") +
      geom_text(aes(x = 0.3, y = 1),
                label = fontawesome(icon),
                family = 'fontawesome-webfont',
                size = 35,
                color = grey50k) +
      geom_text(aes(x = 1.15, y = 0),
                label = ttl_label,
                size = 8,
                color = grey70k) +
      scale_fill_identity() +
      theme_void() +
      theme(plot.title = element_text(hjust = .5),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent', color = NA),
            legend.box.background = element_rect(fill='transparent', color = NA))
  }


  #' @title Map LGA TX_CURR
  #'
  #'
  tx_map <- function(spdf, spdf_states,
                     state = "Akwa Ibom",
                     ind = "TX_CURR",
                     pd = "Q3",
                     cat = NULL) {

    # Filter out state boundaries
    st_bndry <- spdf_states %>%
      dplyr::filter(orgunit == state)

    state_data <- spdf %>%
      dplyr::filter(psnu == state,
             indicator == ind,
             period == pd)

    if (!is.null(cat)) {
      state_data <- state_data %>%
        dplyr::filter(category == cat)
    }

    # Check data validity
    if (is.null(state_data) | nrow(state_data) == 0) {
      return(NULL)
    }

    # Get data extremes
    vmin <- state_data %>% pull(value) %>% min(na.rm = T)
    vmean <- state_data %>% pull(value) %>%  mean(na.rm = T)
    vmed <- state_data %>% pull(value) %>%  median(na.rm = T)
    vmax <- state_data %>% pull(value) %>%  max(na.rm = T)
    vmid <- round((vmax - vmin) / 2, 0)
    vmid1 <- round((vmax - vmin) / 3, 0)
    vmid2 <- round((vmax - vmin) * 2 / 3, 0)

    vbreaks <- c(vmin, vmid1, vmid2, vmax)
    vlabels <- vbreaks %>%
      map_chr(function(.x) {
        .x <- case_when(
        .x >= 1e+03 ~ comma(.x, scale = .001, suffix = "K"),
        .x >= 1e+06 ~ comma(.x, scale = .000001, suffix = "M"),
        .x >= 1e+09 ~ comma(.x, scale = .000000001, suffix = "M"),
        TRUE ~ as.character(.x))
      })

    #viz <- basemap +
    viz <- ggplot() +
      geom_sf(data = state_data,
              aes(fill = value),
              size = .5, color = grey10k) +
      geom_sf(data = st_bndry, fill = NA, size = 2, color = grey10k) +
      geom_sf(data = st_bndry, fill = NA, size = 1, color = grey50k) +
      scale_fill_si(palette = "genoas", breaks = vbreaks, labels = vlabels)

    # Facet if no Category
    if (is.null(cat)) {
      viz <- viz + facet_wrap(~category)
    }

    # Clean up the viz
    viz <- viz +
      labs(x = "", y = "") +
      #si_style_map() +
      theme_void() +
      theme(legend.title = element_blank(),
            legend.position = 'bottom',
            legend.justification = 'center',
            legend.key.height = unit(1, 'line'),
            legend.key.width = unit(2, 'line'),
            legend.text = element_text(size = 10, color = usaid_black),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent', color = NA),
            legend.box.background = element_rect(fill='transparent', color = NA))

    return(viz)
  }

  #' @title State and Country HIV Summary
  #'
  #'
  viz_hisstatus <- function(.data,
                            pd = NULL,
                            name = NULL) {

    # Notifications
    print(paste0(pd, " - ", name))

    # Filter and build dataset
    .data <- .data %>%
      filter(orgunit == name, metric %in% c("hiv_burden", "art_sat", "vls")) %>%
      mutate(
        value = round(value * 100),
        diff = case_when(
          value < 100 ~ 100 - value,
          TRUE ~ 0
        )
      ) %>%
      pivot_longer(names_to = "coverage", values_to = "value", cols = c(value, diff)) %>%
      mutate(coverage = factor(coverage, levels = c("value", "diff"), ordered = T),
             metric = factor(metric,
                             levels = c("hiv_burden", "art_sat", "vls"),
                             labels = c("HIV", "ART", "VLS"),
                             ordered = TRUE),
             color = case_when(
               coverage == 'diff' ~ grey30k,
               metric == 'HIV' & coverage == 'value' ~ burnt_sienna,
               metric == 'ART' & coverage == 'value' ~ scooter,
               metric == 'VLS' & coverage == 'value' ~ genoa
             ),
             value_lbl = case_when(
               value > 100 ~ "*100%",
               TRUE ~ paste0(value, "%")
             ),
             value = case_when(
               value > 100 ~ 100,
               TRUE ~ value
             ),
             label = case_when(
               orgunit == "Country" & metric == 'HIV' & coverage == 'value' ~ paste0(paste0(value_lbl, " POPULATION")),
               orgunit != "Country" & metric == 'HIV' & coverage == 'value' ~ paste0(paste0(value_lbl, " PLHIV")),
               metric == 'ART' & coverage == 'value' ~ paste0(paste0(value_lbl, " on ART")),
               metric == 'VLS' & coverage == 'value' ~ paste0(paste0(value_lbl, " VLS")),
               TRUE ~ NA_character_
             ))

    # Filter valid values
    df_labels <- .data %>% filter(coverage == "value")

    # Create Viz
    viz_sum <- ggplot(.data, aes(fill = color, values = value)) +
      geom_waffle(color = "white", size = .25, n_rows = 5) +
      geom_text(data = df_labels,
                aes(x = 10, y = 3, label = label),
                size = 40, color = "white", fontface = "bold") +
      facet_wrap(~metric, ncol = 1, strip.position = "left") +
      scale_fill_identity() +
      coord_equal() +
      theme_void() +
      theme(legend.position = "none",
            strip.text = element_blank(),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill='transparent', color = NA),
            legend.box.background = element_rect(fill='transparent', color = NA))

    # Save output
    if (!is.null(pd) & !is.null(name)) {
      fname <- paste0("./Graphics/", pd,
                         " - NIGERIA - ",
                         str_to_upper(name),
                         " - HIV Summary Status.png")

      ggsave(filename = fname,
             plot = viz_sum,
             dpi = 320,
             width = 6, height = 6,
             bg = "transparent")
    }

    return(viz_sum)
  }

# DATA ----

  # SITE x IM ----
  df_sites <- file_msd_sites %>%
    read_msd() %>%
    clean_agency()

  df_sites %>% distinct(fiscal_year)
  df_sites %>% distinct(fundingagency)

  # Periods
  curr_fy <- df_sites %>% identifypd(pd_type = "year")
  curr_pd <- df_sites %>% identifypd(pd_type = "full")
  hist_pds <- df_sites %>% identify_pds(pd_end = curr_pd, len = 4)

  # USAID FY21 States
  usaid_states <- df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency == agency,
           psnu %ni% c("_Military Nigeria",
                       "Data reported above PNSU Level")) %>%
    distinct(psnu) %>%
    arrange(psnu) %>%
    pull()

  # SUBNAT: POP & PLHIV ----
  df_nat <- file_msd_nat %>%
    read_msd() %>%
    filter(countryname == cntry)

  df_nat %>% distinct(indicator) %>% prinf()

  # Prioritization
  df_prio <- get_prioritization(df_nat, 2021)

  # Pops
  #pops <- datim_pops(ou = cntry, fy = curr_fy, level = "psnu")

  pops <- df_nat %>%
    filter(indicator %in% c("POP_EST", "PLHIV"),
           standardizeddisaggregate == "Age/Sex") %>%
    group_by(fiscal_year, countryname, psnuuid, psnu, indicator, trendscoarse, sex) %>%
    summarise(across(targets, sum, na.rm = T), .groups = "drop")

  pops <- pops %>%
    filter(fiscal_year == curr_fy & indicator == "POP_EST" |
           fiscal_year == curr_fy -1 & indicator == "PLHIV") %>%
    select(-fiscal_year)

  # GEO ----
  spdf_pepfar <- file_shp %>% read_sf()

  spdf_adm0 <- gisr::extract_boundaries(spdf_pepfar, cntry, level = 3)
  spdf_adm1 <- gisr::extract_boundaries(spdf_pepfar, cntry, level = 4)
  spdf_adm2 <- gisr::extract_boundaries(spdf_pepfar, cntry, level = 5)

  spdf_facs <- gisr::extract_locations(cntry, level = 6) %>%
    extract_facilities()

  terr <- get_raster()

# MUNGE ----

  # Prio + Pops
  df_prio <- df_prio %>%
    left_join(pops, by = c("psnuuid", "psnu"))

  # POP/PLHIV Stats ----
  df_pops <- pops %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    arrange(psnu, desc(indicator)) %>%
    pivot_wider(names_from = indicator, values_from = targets) %>%
    clean_names() %>%
    group_by(psnuuid, psnu, category) %>%
    summarise(
      plhiv = sum(plhiv, na.rm = TRUE),
      pop_est = sum(pop_est, na.rm = TRUE),
      hiv_prev = plhiv / pop_est,
      .groups = "drop"
    ) %>%
    ungroup() %>%
    group_by(psnuuid, psnu) %>%
    mutate(
      prp_plhiv = plhiv / sum(plhiv, na.rm = TRUE),
      prp_pop_est = pop_est / sum(pop_est, na.rm = TRUE),
      ttl_plhiv = sum(plhiv, na.rm = TRUE),
      ttl_pop_est = sum(pop_est, na.rm = TRUE),
      ttt_hiv_prev = ttl_plhiv / ttl_pop_est
    ) %>%
    ungroup()

  # KNOWN Status: NOT GOOD ----
  df_hiv_status <- df_nat %>%
    filter(indicator == "DIAGNOSED_SUBNAT",
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    group_by(psnuuid, psnu, trendscoarse, sex) %>%
    summarise(value = sum(targets, na.rm = T)) %>%
    rename(age = trendscoarse) %>%
    group_agesex()%>%
    group_by(psnuuid, psnu, category) %>%
    summarise(
      pos = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    group_by(psnuuid, psnu) %>%
    mutate(ttl_pos = sum(pos, na.rm = T)) %>%
    ungroup()

  # HTS TESTING ----
  df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency == agency,
           indicator %in% c("HTS_TST", "HTS_TST_POS")) %>%
    distinct(standardizeddisaggregate)

  df_hts <- df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency == agency,
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result")

  df_hts_states <- df_hts %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    group_by(psnuuid, psnu, indicator, category) %>%
    summarise(across(cumulative, sum, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    pivot_wider(names_from = indicator, values_from = cumulative) %>%
    clean_names() %>%
    mutate(hts_yield = hts_tst_pos / hts_tst) %>%
    group_by(psnuuid, psnu) %>%
    mutate(
      ttl_hts_tst = sum(hts_tst, na.rm = T),
      prp_hts_tst = hts_tst / ttl_hts_tst,
      ttl_hts_tst_pos = sum(hts_tst_pos, na.rm = T),
      prp_hts_tst_pos = hts_tst_pos / ttl_hts_tst_pos,
      ttl_hts_yield = ttl_hts_tst_pos / ttl_hts_tst
    ) %>%
    ungroup() %>%
    mutate(category = factor(category, levels = cat_levels, ordered = T)) %>%
    arrange(category)

  # TX ----
  df_tx <- df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency == agency,
           indicator %in% c("TX_RTT", "TX_NEW", "TX_ML",
                            "TX_NET_NEW", "TX_CURR"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                         "Age/Sex/ARTNoContactReason/HIVStatus")) %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    group_by(psnuuid, psnu, indicator, category) %>%
    summarise(across(c(targets, starts_with("qtr"), cumulative),
                     sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(achievement = cumulative / targets) %>%
    group_by(psnuuid, psnu, indicator) %>%
    mutate(ttl_targets = sum(targets),
           ttl_results = sum(cumulative),
           ttl_achievement = ttl_results / ttl_targets)

  df_tx_lga <- df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "DEDUP",
           community != "Data reported above Community Level",
           indicator %in% c("TX_NEW", "TX_CURR"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus")) %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    clean_column(colname = "community") %>%
    group_by(psnuuid, psnu, communityuid, community, indicator, category) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop")

  df_tx_lga <- df_tx_lga %>%
    group_by(psnuuid, psnu, communityuid, community, indicator) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(category = "Community") %>%
    bind_rows(df_tx_lga, .) %>%
    pivot_longer(names_to = "period", values_to = "value",
                 cols = starts_with("qtr")) %>%
    mutate(
      period = case_when(
        str_detect(period, "qtr") ~ str_replace(period, "qtr", "Q"),
        TRUE ~ str_to_sentence(period)
      )
    )

  # VLC/S ----
  df_vl <- df_sites %>%
    filter(
      fiscal_year == curr_fy,
      fundingagency == agency,
      community != "Data reported above Community Level",
      indicator %in% c("TX_PVLS", "TX_CURR"), #%>% distinct(indicator, standardizeddisaggregate)
      standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
    ) %>%
    clean_indicator() %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    group_by(fiscal_year,
             psnuuid,
             psnu,
             indicator,
             category) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd(clean = TRUE) %>%
    dplyr::select(-period_type) %>%
    spread(indicator, value) %>%
    group_by(psnuuid, psnu, category) %>%
    mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period),
           VLC = TX_PVLS_D / TX_CURR_LAG2) %>%
    ungroup() %>%
    relocate(TX_CURR_LAG2, .after = TX_CURR) %>%
    filter(period == curr_pd) %>%
    select(-period) %>%
    mutate(VLS = TX_PVLS / TX_PVLS_D)

  df_vl_all <- df_sites %>%
    filter(
      fiscal_year == curr_fy,
      indicator %in% c("TX_PVLS", "TX_CURR"),
      standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
    ) %>%
    clean_indicator() %>%
    group_by(fiscal_year, indicator) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    reshape_msd(clean = TRUE) %>%
    dplyr::select(-period_type) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period),
           VLC = TX_PVLS_D / TX_CURR_LAG2) %>%
    relocate(TX_CURR_LAG2, .after = TX_CURR) %>%
    filter(period == curr_pd) %>%
    select(-period) %>%
    mutate(VLS = TX_PVLS / TX_PVLS_D,
           orgunit = "Country") %>%
    relocate(orgunit, .before = 1)


  # POP, TX & VLC/S ----
  df_all_states <- df_pops %>%
    left_join(df_vl,
              by = c("orgunituid" = "psnuuid",
                     "orgunit" = "psnu", "category")) %>%
    clean_names()

  # State Levels
  df_all_states_viz <- df_all_states %>%
    group_by(orgunituid, orgunit) %>%
    summarise(across(c(pop_est, plhiv, tx_curr, tx_curr_lag2, tx_pvls, tx_pvls_d),
                     sum, na.rm = T), .groups = "drop") %>%
    mutate(hiv_prev = plhiv / pop_est,
           hiv_burden = plhiv / sum(plhiv, na.rm = T),
           art_sat = tx_curr / plhiv,
           vlc = tx_pvls_d / tx_curr_lag2,
           vls = tx_pvls / tx_pvls_d) %>%
    filter(tx_curr > 0) %>%
    pivot_longer(names_to = "metric",
                 values_to = "value",
                 cols = -c(orgunituid, orgunit))

  # National Level -> Add other agencies
  df_all_nat_viz <- df_pops %>%
    summarise(across(c(pop_est, plhiv), sum, na.rm = T)) %>%
    mutate(orgunit = "Country") %>%
    relocate(orgunit, .before = 1) %>%
    left_join(df_vl_all, by = "orgunit") %>%
    clean_names() %>%
    mutate(hiv_prev = plhiv / pop_est,
           hiv_burden = plhiv / pop_est,
           art_sat = tx_curr / plhiv,
           vlc = tx_pvls_d / tx_curr_lag2,
           vls = tx_pvls / tx_pvls_d) %>%
    pivot_longer(names_to = "metric",
                 values_to = "value",
                 cols = -orgunit)



# VIZ ----

  # GEO - LGA TX_CURR ----
  spdf_adm0 %>% gview()

  spdf_adm1 %>% gview()

  spdf_adm1 %>%
    filter(orgunit == "Akwa Ibom") %>%
    gview()

  spdf_adm2 %>% gview()

  spdf_lga_tx <- spdf_adm2 %>%
    left_join(df_tx_lga, by = c("uid" = "communityuid"))

  spdf_lga_tx %>% dview()

  tx_curr_map <- tx_map(spdf = spdf_lga_tx,
                        spdf_states = spdf_adm1,
                        cat = NULL)

  tx_curr_map <- tx_map(spdf = spdf_lga_tx,
                        spdf_states = spdf_adm1,
                        cat = "Community")

  tx_curr_map <- tx_map(spdf = spdf_lga_tx,
                        spdf_states = spdf_adm1,
                        cat = "Adult Female")

  tx_curr_map <- tx_map(spdf = spdf_lga_tx,
                        spdf_states = spdf_adm1,
                        cat = "Adult Male")

  ggsave(paste0(dir_graphics, "/Nigeria - ", sname, " - FY21Q3 LGA TX_CURR Distribution.png"),
         plot = tx_curr_map,
         width = 4, height = 6,
         bg = "transparent")

  usaid_states %>%
    map(~tx_map(spdf = spdf_lga_tx,
               spdf_states = spdf_adm1,
               state = .x))




  # POP Waffle ----
  df_state <- df_pops %>%
    filter(orgunit == "Akwa Ibom") %>%
    mutate(category = factor(
      category,
      levels = cat_levels,
      ordered = T)) %>%
    arrange(category)

  df_state %>%
    select(-orgunituid, -orgunit) %>%
    gt() %>%
    fmt_number(columns = starts_with(c("ttl", "pop", "plhiv")), decimal = 0) %>%
    fmt_percent(columns = starts_with(c("prp", "hiv", "grp_hiv")), decimal = 0)

  state_titles <- df_state %>%
    select(orgunit, pop_est, plhiv, hiv_prev) %>%
    distinct()

  state_plhiv <- c(
    paste("Children", '=', percent(df_state$prp_plhiv[df_state$category == "Children"])),
    paste("Adult Female", '=', percent(df_state$prp_plhiv[df_state$category == "Adult Female"])),
    paste("Adult Male", '=', percent(df_state$prp_plhiv[df_state$category == "Adult Male"]))
  )

  state_pops <- c(
    paste("Children", '=', percent(df_state$prp_pop_est[df_state$category == "Children"])),
    paste("Adult Female", '=', percent(df_state$prp_pop_est[df_state$category == "Adult Female"])),
    paste("Adult Male", '=', percent(df_state$prp_pop_est[df_state$category == "Adult Male"]))
  )

  state_stats <- c(
    paste("Children", '=', percent(df_state$prp_pop_est[df_state$category == "Children"]), "Pop with", percent(df_state$prp_plhiv[df_state$category == "Children"]), "PLHIV"),
    paste("Adult Female", '=', percent(df_state$prp_pop_est[df_state$category == "Adult Female"]), "Pop with", percent(df_state$prp_plhiv[df_state$category == "Adult Female"]), "PLHIV"),
    paste("Adult Male", '=', percent(df_state$prp_pop_est[df_state$category == "Adult Male"]), "Pop with", percent(df_state$prp_plhiv[df_state$category == "Adult Male"]), "PLHIV")
  )

  ggplot(df_state,
         aes(values = ttl_pop_est, fill = category)) +
    geom_waffle(n_rows = 10, make_proportional = TRUE)


  ggplot(df_state,
         aes(label = category,
             values = ttl_pop_est,
             color = category)) +
    geom_pictogram(n_rows = 10, make_proportional = TRUE) +
    scale_color_manual(
      name = glue("About {number(state_titles$pop_est, accuracy=.1, scale=1e-6, suffix='M')} live in Akwa Ibom."),
      values = c(
        Children = "#c68958",
        `Adult Female` = "#ae6056",
        `Adult Male` = "#a40000"
      ),
      labels = state_stats
    ) +
    scale_label_pictogram(
      name = glue("About {number(state_titles$pop_est, accuracy=.1, scale=1e-6, suffix='M')} live in Akwa Ibom."),
      values = c(
        Children = "child",
        `Adult Female` = "female",
        `Adult Male` = "male"
      ),
      labels = state_stats
    ) +
    #labs(title = glue("About {comma(state_titles$pop_est)} live in Akwa Ibom.")) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "right",
          legend.key.size = unit(2, "line"),
          legend.title = element_text(margin = margin(b = 10)))


  # ICON ----

  icon <- fa_icons(icon = "fa-users", fcolor = grey50k)
  icon <- fa_icons(icon = "fa-user", fcolor = grey50k)

  # POPS
  fa_icons(icon = "fa-child", fcolor = grey50k,
           save_as = "./Graphics/Icon fa-child-grey50k.png",
           iheight = 1.30)

  fa_icons(icon = "fa-female", fcolor = grey50k,
           save_as = "./Graphics/Icon fa-female-grey50k.png",
           iheight = 1.30)

  fa_icons(icon = "fa-male", fcolor = grey50k,
           save_as = "./Graphics/Icon fa-male-grey50k.png",
           iheight = 1.30)

  # HTS
  fa_icons(icon = "fa-child", fcolor = burnt_sienna,
           save_as = "./Graphics/Icon fa-child-burnt-sienna.png",
           iheight = 1.30)

  fa_icons(icon = "fa-female", fcolor = burnt_sienna,
           save_as = "./Graphics/Icon fa-female-burnt-sienna.png",
           iheight = 1.30)

  fa_icons(icon = "fa-male", fcolor = burnt_sienna,
           save_as = "./Graphics/Icon fa-male-burnt-sienna.png",
           iheight = 1.30)

  # TX
  fa_icons(icon = "fa-child", fcolor = scooter,
           save_as = "./Graphics/Icon fa-child-scooter.png",
           iheight = 1.30)

  fa_icons(icon = "fa-female", fcolor = scooter,
           save_as = "./Graphics/Icon fa-female-scooter.png",
           iheight = 1.30)

  fa_icons(icon = "fa-male", fcolor = scooter,
           save_as = "./Graphics/Icon fa-male-scooter.png",
           iheight = 1.30)

  # VL
  fa_icons(icon = "fa-child", fcolor = genoa,
           save_as = "./Graphics/Icon fa-child-genoa.png",
           iheight = 1.30)

  fa_icons(icon = "fa-female", fcolor = genoa,
           save_as = "./Graphics/Icon fa-female-genoa.png",
           iheight = 1.30)

  fa_icons(icon = "fa-male", fcolor = genoa,
           save_as = "./Graphics/Icon fa-male-genoa.png",
           iheight = 1.30)

  # Others
  icon <- fa_icons(icon = "fa-child", fcolor = burnt_sienna)
  icon <- fa_icons(icon = "fa-female", fcolor = moody_blue)
  icon <- fa_icons(icon = "fa-male", fcolor = scooter)

  icon <- fa_icons(icon = "fa-user-md", fcolor = usaid_medblue)

  icon <- fa_icons(icon = "fa-thermometer-full", fcolor = usaid_red)

  icon <- fa_icons(icon = "fa-flask", fcolor = genoa)

  icon <- fa_icons(icon = "fa-check-circle-o", fcolor = genoa)
  icon <- fa_icons(icon = "fa-check-square-o", fcolor = genoa)

  icon <- fa_icons(icon = "fa-crosshairs", fcolor = burnt_sienna)

  ggsave(filename = paste0("./Graphics/Icon fa-check.png"),
         plot = icon,
         dpi = 320,
         width = 1.25, height = 1.25,
         bg = "transparent")



  # ICONs ----
  iron(
    waffle(
      c('TRUE' = 8, 'FALSE' = 2),
      colors = c("pink", "grey70"),
      #use_glyph = "female",
      glyph_size = 12,
      title = "Female vs Male",
      rows = 1,
      size = 1.5,
      legend_pos = "none"
    ) +
      theme(plot.title = element_text(hjust = 0.5))
    ,
    waffle(
      c('TRUE' = 3, 'FALSE' = 7),
      colors = c("skyblue", "grey70"),
      #use_glyph = "male",
      glyph_size = 12,
      rows = 1,
      size = 1.5,
      legend_pos = "none"
    ),
    waffle(
      c('TRUE' = 6, 'FALSE' = 4),
      colors = c("skyblue", "grey70"),
      #use_glyph = "male",
      glyph_size = 12,
      rows = 1,
      size = 1.5,
      legend_pos = "none"
    ) +
      xlab("1 sq == 10%")
  )

  # WAFFLE ----
  waffle(parts = c('One' = 50, 'Two' = 35, 'Three' = 15),
         rows = 10,
         keep = TRUE,
         xlab = NULL,
         title = NULL,
         colors = NA,
         size = 1,
         flip = FALSE,
         reverse = FALSE,
         equal = TRUE,
         pad = 0,
         use_glyph = FALSE,
         glyph_size = 20,
         glyph_font = "Font Awesome 5 Free Solid",
         glyph_font_family = "FontAwesome5Free-Solid",
         legend_pos = "right")

  waffle(
    c(`Poor=10` =10, `Average=18` = 18, `Excellent=7` =7),
    rows = 5,
    colors = c("#FD6F6F", "#93FB98", "#D5D9DD"),
    use_glyph = "female",
    glyph_size = 12 ,
    title = 'Girls Performance',
    legend_pos="bottom"
  )



  # HTS POS ----
  df_hts_state <- df_hts_states %>%
    filter(psnu == sname)

  df_hts_state %>%
    ggplot(aes(values = hts_tst, fill = category)) +
      geom_waffle(n_rows = 10, make_proportional = TRUE,
                  color = "white",
                  radius = unit(2, "pt"), flip = F) +
    scale_fill_manual(
      name = "% Tested",
      values = c(
        Children = "blue",
        `Adult Female` = "red",
        `Adult Male` = "grey50"
      )) +
    theme_void()


  df_hts_state %>%
    ggplot(aes(values = hts_tst,
               label = category,
               color = category)) +
    geom_pictogram(n_rows = 10, make_proportional = TRUE) +
    scale_color_manual(
      name = "% Tested",
      values = c(
        Children = "blue",
        `Adult Female` = "red",
        `Adult Male` = "grey50"
      )
    ) +
    scale_label_pictogram(
      name = "% Tested",
      values = c(
        Children = "child",
        `Adult Female` = "female",
        `Adult Male` = "male"
      )#,
      #labels = state_stats
    ) +
    theme_void()

  w_hts_tst <- df_hts_state %>%
    pull(prp_hts_tst) %>%
    equal_parts()

  child <- glue('Children ({w_hts_tst[1]}%)')
  adult_fem <- glue('Adult Female ({w_hts_tst[2]}%)')
  adult_male <- glue('Adult Male ({w_hts_tst[3]}%)')

  df_parts <- tibble(
    category = df_hts_state %>% pull(category),
    cumulative = df_hts_state %>% pull(hts_tst),
    names = c(child, adult_fem, adult_male),
    vals = c(w_hts_tst[1], w_hts_tst[2], w_hts_tst[3]),
    diff = 100 - vals,
    color = c(burnt_sienna, moody_blue, scooter)
  )

  # Waffle for proportions ----
  # Dunuts for all portions ----
  df_parts %>% viz_portions(icon = "fa-users")

  df_parts %>%
    ggplot(aes(x = 2, y = vals, fill = color))+
    geom_col(show.legend = F)+
    coord_polar(theta = "y", start = 0, direction = 1) +
    xlim(c(.2, 2.5)) +
    geom_text(aes(y = vals, label = str_replace_all(names, " ", "\n")),
              position = position_stack(vjust = .5),
              color = "white", fontface = "bold") +
    geom_text(aes(x = 0.3, y = 1),
              label = fontawesome("fa-users"),
              family = 'fontawesome-webfont',
              size = 35,
              color = grey50k) +
    geom_text(aes(x = 1.15, y = 0),
              label = comma(sum(df_hts_state$hts_tst)),
              size = 8,
              color = grey70k) +
    scale_fill_identity() +
    theme_void() +
    theme(plot.title = element_text(hjust = .5))


  # Dunuts for single portions ----
  df_parts %>%
    filter(category == "Children") %>%
    viz_portion()

  df_parts %>% viz_portion()

  viz_child <- df_parts %>% viz_portion(cat = "Children")
  viz_adfem <- df_parts %>% viz_portion(cat = "Adult Female")
  viz_admale <- df_parts %>% viz_portion(cat = "Adult Male")


  viz <- (viz_child + viz_adfem + viz_admale)


  # State Summary ----

  viz_hisstatus(.data = df_all_states_viz,
                pd = curr_pd,
                name = "Cross River")

  df_all_states_viz %>%
    distinct(orgunit) %>%
    pull() %>%
    map(~viz_hisstatus(.data = df_all_states_viz,
                       pd = curr_pd,
                       name = .x))

  viz_cntry_sum <- viz_hisstatus(.data = df_all_nat_viz,
                                 pd = curr_pd,
                                 name = "Country")

#































  # waffle(
  #   c('Children=' = round(w_hts_tst[1] * 100, 0),
  #     'Adult Female=' = round(w_hts_tst[2], 0),
  #     'Adult Male=' = round(w_hts_tst[3])),
  #   rows = 10, colors = c("red", "green", "grey50"),
  #   title = 'Responses', legend_pos="bottom"
  # )
  #
  # waffle(
  #   c('Yes=70%' = 70, 'No=30%' = 30),
  #   rows = 10, colors = c("red", "grey50"),
  #   title = 'Responses', legend_pos="bottom"
  # )
  #
  # waffle(
  #   c('Children=70%' = df_hts_state$hts_tst_pos[df_hts_state$category == "Children"],
  #     'Adult Female=30%' = df_hts_state$hts_tst_pos[df_hts_state$category == "Adult Female"],
  #     'Adult Male=30%' = df_hts_state$hts_tst_pos[df_hts_state$category == "Adult Male"]),
  #   rows = 10,
  #   colors = c("#FD6F6F", "#93FB98", "red"),
  #   title = 'Responses', legend_pos="bottom"
  # )
  #
  # poss <- c(children = df_hts_state$hts_tst_pos[df_hts_state$category == "Children"],
  #   `Adult Female` = df_hts_state$hts_tst_pos[df_hts_state$category == "Adult Female"],
  #   `Adult Male` = df_hts_state$hts_tst_pos[df_hts_state$category == "Adult Male"])
  #
  # poss <- c(children = 55,
  #           `Adult Female` = 30,
  #           `Adult Male` = 20)
  #
  # waffle(poss, rows=10, size = 1,
  #        glyph_font = "child")



