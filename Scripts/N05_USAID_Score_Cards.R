##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: USAID Quarterly Score Cards
##  LICENCE: MIT
##  DATE:    2021-10-22
##  UPDATED: 2021-11-30

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

  font_add(family = "FontAwesome5Free-Solid", regular = paste0(si_path("path_downloads"), "fa-solid-900.ttf"))
  font_add(family = "FontAwesome5Free-Regular", regular = paste0(si_path("path_downloads"), "fa-regular-400.ttf"))
  font_add(family = "FontAwesome5Brands-Regular", regular = paste0(si_path("path_downloads"), "fa-brands-400.ttf"))

  showtext_auto()

  fonts()

  # load utilities
  source("./Scripts/N00_Utilities.R")
  source("./Scripts/N00_Viz_Utilities.R")

# GLOBAL ----

  # DIR - Global ----
  dir_merdata <- glamr::si_path("path_msd")
  dir_geodata <- si_path("path_vector")
  dir_raster <- si_path("path_raster")
  dir_targets <- "../../PEPFAR/COUNTRIES/Nigeria/DataPack"
  dir_sid <- "../../PEPFAR/COUNTRIES/Nigeria/SID"

  # DIR - Project ----
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"

  dir_graphics <- "Graphics"

  # Files ----
  file_msd_sites <- return_latest(
    folderpath = dir_merdata,
    pattern = "Site_IM_FY20.*_N.*"
  )

  file_msd_psnu <- return_latest(
    folderpath = dir_merdata,
    pattern = "PSNU_IM_FY20.*_N.*"
  )

  file_msd_nat <- return_latest(
    folderpath = dir_merdata,
    pattern = "NAT_SUBNAT_FY15.*"
  )

  file_sid <- return_latest(
    folderpath = dir_sid,
    pattern = "Nigeria SID 2021_Expert.*"
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
  cat_levels <- c("Children", "Adult Female", "Adult Male", "All")

# FUNCTIONS ----

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
                     pd = "FY21Q4",
                     cat = "All",
                     lsize = 10) {

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
      scale_fill_si(palette = "scooters", breaks = vbreaks, labels = vlabels)

    # Facet if no Category
    if (is.null(cat)) {
      viz <- viz + facet_wrap(~category)
    }

    # Clean up the viz
    viz <- viz +
      labs(x = "", y = "") +
      theme_void() +
      theme(legend.title = element_blank(),
            legend.position = 'bottom',
            legend.justification = 'center',
            legend.key.height = unit(1, 'line'),
            legend.key.width = unit(4, 'line'),
            legend.text = element_text(size = lsize, color = usaid_black),
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
  viz_hivstatus <- function(.data,
                            pd = "FY21Q4",
                            cat = "All",
                            name = NULL,
                            lsize = 30) {

    # Notifications
    print(paste0(pd, " - ", name))

    # Filter and build data
    .data <- .data %>%
      select(-share) %>%
      filter(psnu == name,
             category == cat,
             indicator %in% c("HIV_BURDEN", "ART_SAT", "VLS")) %>%
      mutate(
        value = round(value * 100),
        diff = case_when(
          value < 100 ~ 100 - value,
          TRUE ~ 0
        )
      ) %>%
      pivot_longer(names_to = "coverage",
                   values_to = "value",
                   cols = c(value, diff)) %>%
      mutate(coverage = factor(coverage,
                               levels = c("value", "diff"),
                               ordered = T),
             indicator = factor(indicator,
                                levels = c("HIV_BURDEN", "ART_SAT", "VLS"),
                                labels = c("HIV", "ART", "VLS"),
                                ordered = TRUE),
             color = case_when(
               coverage == 'diff' ~ grey30k,
               indicator == 'HIV' & coverage == 'value' ~ burnt_sienna,
               indicator == 'ART' & coverage == 'value' ~ scooter,
               indicator == 'VLS' & coverage == 'value' ~ genoa
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
               psnu == "Country" & indicator == 'HIV' & coverage == 'value' ~ paste0(paste0(value_lbl, " POPULATION")),
               psnu != "Country" & indicator == 'HIV' & coverage == 'value' ~ paste0(paste0(value_lbl, " PLHIV")),
               indicator == 'ART' & coverage == 'value' ~ paste0(paste0(value_lbl, " on ART")),
               indicator == 'VLS' & coverage == 'value' ~ paste0(paste0(value_lbl, " VLS")),
               TRUE ~ NA_character_
             ))

    # Filter valid values
    df_labels <- .data %>% filter(coverage == "value")

    # Create Viz
    viz_sum <- ggplot(.data, aes(fill = color, values = value)) +
      geom_waffle(color = "white", size = .25, n_rows = 5) +
      geom_text(data = df_labels,
                aes(x = 10, y = 3, label = label),
                size = lsize, color = "white", fontface = "bold") +
      facet_wrap(~indicator, ncol = 1, strip.position = "left") +
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
                         " - ScoreCard_HIV_SummaryStatus_",
                         str_to_upper(name),
                         ".png")

      ggsave(filename = fname,
             plot = viz_sum,
             dpi = 320,
             width = 6, height = 6,
             bg = "transparent")
    }

    return(viz_sum)
  }

# DATA ----

  # SID ----
  df_sid <- file_sid %>%
    read_excel(sheet = "Raw Element Data") %>%
    clean_names()

  df_sid <- df_sid %>%
    rename(elements = nigeria) %>%
    mutate(domains = ifelse(is.na(sid_2015),
                           elements,
                           NA_character_),
           sid_2021 = NA) %>%
    fill(domains) %>%
    relocate(domains, .before = 1) %>%
    filter(!is.na(sid_2015)) %>%
    mutate(across(starts_with("sid"), str_replace, "N/A", "")) %>%
    mutate(across(starts_with("sid"), as.numeric)) %>%
    mutate(across(starts_with("sid"), comma, accuracy = 0.01))



  # SITE x IM ----
  df_sites <- file_msd_sites %>%
    read_msd() %>%
    clean_agency()

  df_sites %>%
    distinct(fiscal_year) %>%
    arrange(fiscal_year)

  df_sites %>% distinct(fundingagency)

  # Periods ----
  curr_fy <- df_sites %>% identifypd(pd_type = "year")
  curr_pd <- df_sites %>% identifypd(pd_type = "full")
  hist_pds <- df_sites %>% identify_pds(pd_end = curr_pd, len = 6)

  # USAID FY21 States ----
  usaid_states <- df_sites %>%
    filter(fiscal_year == curr_fy,
           funding_agency == agency,
           psnu %ni% c("_Military Nigeria",
                       "Data reported above PNSU Level")) %>%
    distinct(psnu) %>%
    arrange(psnu) %>%
    pull()

  # SUBNAT: POP & PLHIV ----
  df_nat <- file_msd_nat %>% read_msd()

  #df_nat %>% distinct(indicator) %>% prinf()

  # GEO ----
  spdf_pepfar <- file_shp %>% read_sf()

  spdf_adm0 <- gisr::extract_boundaries(spdf_pepfar, cntry, level = 3)
  spdf_adm1 <- gisr::extract_boundaries(spdf_pepfar, cntry, level = 4)
  spdf_adm2 <- gisr::extract_boundaries(spdf_pepfar, cntry, level = 5)

  spdf_facs <- gisr::extract_locations(cntry, level = 6) %>%
    extract_facilities()

  terr <- get_raster()

# MUNGE ----

  # Prio + Pops ----

  # Prioritization
  df_prio <- get_prioritization(df_nat, curr_fy, cntry)

  # Pops

  #pops <- datim_pops(ou = cntry, fy = curr_fy, level = "psnu")

  pops <- df_nat %>%
    filter(country == cntry,
           indicator %in% c("POP_EST", "PLHIV"),
           standardizeddisaggregate == "Age/Sex") %>%
    rename(value = targets)

  pops <- pops %>%
    filter(fiscal_year == curr_fy & indicator == "POP_EST" |
             fiscal_year == curr_fy -1 & indicator == "PLHIV") %>%
    select(-fiscal_year) %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    group_by(psnuuid, psnu, indicator, category) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop")

  pops <- pops %>%
    group_by(psnuuid, psnu, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(category = "All") %>%
    bind_rows(pops, .) %>%
    arrange(psnu, indicator, category)

  pops <- pops %>%
    group_by(psnuuid, psnu, indicator) %>%
    mutate(share = value / value[category == "All"]) %>%
    ungroup() %>%
    arrange(psnu, indicator, category)

  # df_prio <- df_prio %>%
  #   left_join(pops, by = c("psnuuid", "psnu"))

  # POP/PLHIV Stats ----
  df_pops <- pops %>%
    group_by(psnuuid, psnu, category) %>%
    summarise(
      value = value[indicator == "PLHIV"] / value[indicator == "POP_EST"],
      share = NA_real_,
      .groups = "drop"
    ) %>%
    mutate(indicator = "HIV_PREV") %>%
    bind_rows(pops, .)


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
           funding_agency == agency,
           indicator %in% c("HTS_TST", "HTS_TST_POS")) %>%
    distinct(standardizeddisaggregate)

  df_hts_all <- df_sites %>%
    filter(funding_agency == agency,
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result")

  df_hts_all <- df_hts_all %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    reshape_msd() %>%
    filter(period_type == "results") %>%
    group_by(period, psnuuid, psnu, indicator, category) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop")

  df_hts_all <- df_hts_all %>%
    group_by(period, psnuuid, psnu, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(category = "All") %>%
    bind_rows(df_hts_all, .) %>%
    arrange(period, psnu, indicator, category)

  # Calcualate Yield
  df_hts_all <- df_hts_all %>%
    group_by(period, psnuuid, psnu, category) %>%
    summarise(value = value[indicator == "HTS_TST_POS"] / value[indicator == "HTS_TST"],
              .groups = "drop") %>%
    mutate(indicator = "HTS_YIELD") %>%
    bind_rows(df_hts_all, .) %>%
    group_by(period, psnuuid, psnu, indicator) %>%
    mutate(
      share = case_when(
        indicator != "HTS_YIELD" ~ value / value[category == 'All'],
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    arrange(period, psnu, indicator, category)

  # HTS for Current PD
  df_hts <- df_hts_all %>%
    filter(period == curr_pd) %>%
    select(-period)

  # TX ----
  df_tx_all <- df_sites %>%
    filter(funding_agency == agency,
           indicator %in% c("TX_RTT", "TX_NEW", "TX_ML",
                            "TX_NET_NEW", "TX_CURR"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                         "Age/Sex/ARTNoContactReason/HIVStatus")) %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    reshape_msd() %>%
    filter(period_type == "results") %>%
    group_by(period, psnuuid, psnu, indicator, category) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop")

  df_tx_all <- df_tx_all %>%
    group_by(period, psnuuid, psnu, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(category = "All") %>%
    relocate(category, .after = indicator) %>%
    bind_rows(df_tx_all) %>%
    arrange(period, psnu, indicator, category)

  df_tx <- df_tx_all %>%
    filter(period == curr_pd) %>%
    select(-period) %>%
    group_by(psnuuid, psnu, indicator) %>%
    mutate(share = value / value[category == "All"]) %>%
    ungroup()

  # TX_CURR HISTORY by LGA ----
  df_tx_lga_all <- df_sites %>%
    filter(funding_agency != "DEDUP",
           community != "Data reported above Community Level",
           indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus")) %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    clean_column(colname = "community") %>%
    reshape_msd() %>%
    filter(period_type == "results") %>%
    group_by(period, psnuuid, psnu, communityuid, community, indicator, category) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop")

  df_tx_lga_all <- df_tx_lga_all %>%
    group_by(period, psnuuid, psnu, communityuid, community, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(category = "All") %>%
    relocate(category, .after = indicator) %>%
    bind_rows(df_tx_lga_all) %>%
    arrange(psnu, community, psnu, indicator, category)

  # VLC/S ----
  df_vl_all <- df_sites %>%
    filter(
      funding_agency == agency,
      community != "Data reported above Community Level",
      indicator %in% c("TX_PVLS", "TX_CURR"),
      standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
    ) %>%
    clean_indicator() %>%
    rename(age = trendscoarse) %>%
    group_agesex() %>%
    reshape_msd() %>%
    filter(period_type == "results") %>%
    group_by(period,
             psnuuid,
             psnu,
             indicator,
             category) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>%
    ungroup()

  # Add "All" Category
  df_vl_all <- df_vl_all %>%
    group_by(period, psnuuid, psnu, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(category = "All") %>%
    relocate(category, .after = psnu) %>%
    bind_rows(df_vl_all) %>%
    arrange(period, psnuuid, psnu, category)

  # TX_CURR LAG2
  df_vl_all <- df_vl_all %>%
    filter(indicator == "TX_CURR") %>%
    group_by(psnuuid, psnu, category) %>%
    mutate(value = lag(value, order_by = period)) %>%
    ungroup() %>%
    mutate(indicator = "TX_CURR_LAG2") %>%
    bind_rows(df_vl_all) %>%
    arrange(period, psnu, category, indicator)

  # VLC
  df_vl_all <- df_vl_all %>%
    group_by(period, psnuuid, psnu, category) %>%
    summarise(value = value[indicator == "TX_PVLS_D"] / value[indicator == "TX_CURR_LAG2"],
              indicator = "VLC",
              .groups = "drop") %>%
    bind_rows(df_vl_all, .) %>%
    arrange(period, psnu, category, indicator)

  # VLS
  df_vl_all <- df_vl_all %>%
    group_by(period, psnuuid, psnu, category) %>%
    summarise(value = value[indicator == "TX_PVLS"] / value[indicator == "TX_PVLS_D"],
              indicator = "VLS",
              .groups = "drop") %>%
    ungroup() %>%
    bind_rows(df_vl_all, .) %>%
    arrange(period, psnu, category, indicator)

  # VL for Current Period
  df_vl <- df_vl_all %>%
    filter(period == curr_pd) %>%
    select(-period) %>%
    mutate(category = factor(category, levels = cat_levels))

  # POP, TX & VLC/S ----
  df_all_states <- df_pops %>%
    bind_rows(df_hts, df_tx %>% filter(indicator != "TX_CURR"), df_vl) %>%
    mutate(category = factor(category, levels = cat_levels, ordered = TRUE))

  df_all_states %>% glimpse()

  df_all_states %>% distinct(indicator)

  df_all_states %>%
    distinct(indicator, category) %>%
    arrange(indicator, category)

  # Add ART Saturation
  df_all_states <- df_all_states %>%
    filter(indicator %in% c("PLHIV", "TX_CURR")) %>%
    group_by(psnuuid, psnu, category) %>%
    summarise(
      value = value[indicator == "TX_CURR"] / value[indicator == "PLHIV"],
      share = NA_real_,
      .groups = "drop"
    ) %>%
    mutate(indicator = "ART_SAT",
           value = if_else(value > 1, 1, value)) %>%
    bind_rows(df_all_states, .)

  # Add HIV Burden
  df_all_states <- df_all_states %>%
    filter(indicator == "PLHIV") %>%
    group_by(category) %>%
    mutate(
      value = value / sum(value, na.rm = TRUE),
      share = NA_real_
    ) %>%
    ungroup() %>%
    mutate(indicator = "HIV_BURDEN") %>%
    bind_rows(df_all_states, .)

  # National HIV Burden, Saturation & VLS
  df_all_nat <- df_all_states %>%
    select(-share) %>%
    filter(indicator %in% c("POP_EST", "PLHIV", "TX_CURR",
                            "TX_PVLS", "TX_PVLS_D"))

  df_all_nat %>%
    group_by(category) %>%
    summarise(
      hiv_prev = sum(value[indicator == "PLHIV"]) / sum(value[indicator == "POP_EST"]),
      art_sat = sum(value[indicator == "TX_CURR"]) / sum(value[indicator == "PLHIV"]),
      vls = sum(value[indicator == "TX_PVLS"]) / sum(value[indicator == "TX_PVLS_D"])) %>%
    ungroup() %>%
    mutate(psnu = "country") %>%
    relocate(psnu, .before = 1) %>%
    pivot_longer(names_to = "indicator",
                 values_to = "value",
                 cols = c(hiv_prev, art_sat, vls)) %>%
    mutate(indicator = str_to_upper(indicator))






# VIZ ----

  # GEO - LGA TX_CURR ----
  spdf_adm0 %>% gview()

  spdf_adm1 %>% gview()

  spdf_adm1 %>%
    filter(orgunit == "Akwa Ibom") %>%
    gview()

  spdf_adm2 %>% gview()

  spdf_lga_tx <- spdf_adm2 %>%
    left_join(df_tx_lga_all %>%
                filter(psnu == "Akwa Ibom",
                       period == curr_pd),
              by = c("uid" = "communityuid")) %>%
    filter(!is.na(value))

  spdf_lga_tx <- spdf_adm2 %>%
    left_join(df_tx_lga_all %>%
                filter(period == curr_pd),
              by = c("uid" = "communityuid")) %>%
    filter(!is.na(value))

  tx_curr_map <- tx_map(spdf = spdf_lga_tx,
                        spdf_states = spdf_adm1,
                        cat = "Children",
                        pd = curr_pd)

  tx_curr_map <- tx_map(spdf = spdf_lga_tx,
                        spdf_states = spdf_adm1,
                        cat = "Adult Female",
                        pd = curr_pd)

  tx_curr_map <- tx_map(spdf = spdf_lga_tx,
                        spdf_states = spdf_adm1,
                        cat = "Adult Male",
                        pd = curr_pd)

  tx_curr_map <- tx_map(spdf = spdf_lga_tx,
                        spdf_states = spdf_adm1,
                        cat = "All",
                        pd = curr_pd,
                        lsize = 22)

  ggsave("./Graphics/ScoreCard_TX_CURR_Distribution_IkwaIbom.png",
         plot = tx_curr_map,
         width = 5, height = 6,
         units = "in", dpi = 320,
         bg = "transparent")



  usaid_states %>%
    map(function(.x) {
      m <- tx_map(spdf = spdf_lga_tx,
                  spdf_states = spdf_adm1,
                  state = .x,
                  cat = "All",
                  pd = curr_pd,
                  lsize = 40)

      print(m)

      ggsave(filename = file.path(
        dir_graphics,
        glue("{curr_pd} - Nigeria - USAID - {.x} - TX_CURR Distribution by LGA_{curr_date()}.png")),
        plot = m)
    })




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


  # State HIV Summary ----

  viz_hivstatus(.data = df_all_states,
                pd = curr_pd,
                name = "Akwa Ibom",
                lsize = 40)

  df_all_states_viz %>%
    distinct(orgunit) %>%
    pull() %>%
    map(~viz_hisstatus(.data = df_all_states_viz,
                       pd = curr_pd,
                       name = .x))

  viz_cntry_sum <- viz_hisstatus(.data = df_all_nat_viz,
                                 pd = curr_pd,
                                 name = "Country")

  # TX History ----
  viz_tx_trend <- df_tx_all %>%
    filter(psnu == "Akwa Ibom",
           category == "All",
           indicator == "TX_CURR",
           period %in% hist_pds) %>%
    mutate(period = str_replace(period, "Q", " Q")) %>%
    ggplot(aes(x = period, y = value)) +
    geom_col(fill = scooter) +
    geom_text(aes(label = comma(value)),
              vjust = 2, color = "white",
              fontface = "bold", size = 10) +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 22, face = "bold", color = usaid_black),
          plot.margin = unit(c(0,0,0,0), "in")) +
    theme_transparent()

  ggsave("./Graphics/ScoreCard_TX_Trend_IkwaIbom.png",
         plot = viz_tx_trend,
         width = 4, height = 2,
         units = "in", dpi = 320,
         bg = "transparent")


  # SID Status ----
  df_sid %>%
    group_by(domains) %>% gt()

  df_sid %>%
    group_by(domains) %>%
    gt() %>%
    cols_label(
      elements = "ELEMENTS",
      sid_2015 = "2015",
      sid_2017 = "2017",
      sid_2019 = "2019",
      sid_2021 = "2021"
    ) %>%
    fmt_missing(
      columns = tidyselect::everything(),
      missing_text = "N/A"
    ) %>%
    cols_width(1:2 ~ px(700), 3:6 ~ px(300)) %>%
    # domains / elements
    tab_style(
      style = list(
        cell_text(color = usaid_black, weight = "bold", size = 20)
      ),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = list(
        cell_text(color = usaid_black, indent = px(20))
      ),
      locations = cells_body(
        columns = elements,
        rows = everything()
      )
    ) %>%
    # Font size
    tab_style(
      style = list(
        cell_text(size = "x-large")
      ),
      locations = cells_body(
        columns = everything()
      )
    ) %>%
    # 2015
    tab_style(
      style = list(
        cell_fill(color = usaid_red),
        cell_text(color = "white")
      ),
      locations = cells_body(
        columns = sid_2015,
        rows = sid_2015 < 3.50
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = golden_sand),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2015,
        rows = sid_2015 >= 3.50 & sid_2015 < 6.99
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = genoa_light),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2015,
        rows = sid_2015 >= 7.00 & sid_2015 < 8.49
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = genoa),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2015,
        rows = sid_2015 >= 8.50 & sid_2015 <= 10
      )
    ) %>%
    # 2017
    tab_style(
      style = list(
        cell_fill(color = usaid_red),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2017,
        rows = sid_2017 < 3.50
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = golden_sand),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2017,
        rows = sid_2017 >= 3.50 & sid_2017 < 6.99
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = genoa_light),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2017,
        rows = sid_2017 >= 7.00 & sid_2017 < 8.49
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = genoa),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2017,
        rows = sid_2017 >= 8.50 & sid_2017 <= 10
      )
    ) %>%
    # 2019
    tab_style(
      style = list(
        cell_fill(color = usaid_red),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2019,
        rows = sid_2019 < 3.50
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = golden_sand),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2019,
        rows = sid_2019 >= 3.50 & sid_2019 < 6.99
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = genoa_light),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2019,
        rows = sid_2019 >= 7.00 & sid_2019 < 8.49
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = genoa),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2019,
        rows = sid_2019 >= 8.00 & sid_2019 <= 10.0
      )
    ) %>%
    # 2021
    tab_style(
      style = list(
        cell_fill(color = grey30k),
        cell_text(color = "white")),
      locations = cells_body(
        columns = sid_2021
      )
    )































