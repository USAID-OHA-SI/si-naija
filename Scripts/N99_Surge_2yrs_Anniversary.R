##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: TX_CURR Results Trend by Gender
##  LICENCE: MIT
##  DATE:    2021-03-08
##  UPDATED: 2021-08-23


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
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

  rep_pd <- file_psnu_im %>% identify_pd()
  msd_version <- ifelse(str_detect(file_psnu_im, ".*_\\d{8}_v1_\\d"), "i", "c")
  msd_caption <- rep_pd %>% paste0(msd_version)

  # Shapefile path
  file_shp <- dir_geodata %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

  # Indicators
  inds <- c("HTS_TST_POS",
            "TX_CURR",
            "TX_NEW",
            "OVC_SERV",
            "PrEP_CURR",
            "KP_PREV")

  # Target Agency
  agency <- "USAID"


# FUNCTIONS ----

  #' @title Get n% of a number
  #'
  #' @param num  Number
  #' @param prop Proportion, default 1
  #' @param ...  Other valid options of round
  #'
  get_proportion <- function(n, p = 1, ...) {
    base::sapply(n, function(x) {
      round((x * (p / 100)), ...)
    })
  }


  #' @title Surge Trend
  #'
  surge_trend <- function(df, targets,
                          indicator = "TX_CURR",
                          save = FALSE) {

    df <- df %>%
      filter(indicator == {{indicator}})

    targets <- targets %>%
      filter(indicator == {{indicator}})

    t_fy19 <- targets %>% filter(period == 'FY19') %>% pull(value)
    t_fy20 <- targets %>% filter(period == 'FY20') %>% pull(value)
    t_fy21 <- targets %>% filter(period == 'FY21') %>% pull(value)

    # limits
    #start <- 'Q1\nDec `18'
    start <- df %>% arrange(pd_order) %>% pull(period3) %>% first()
    #surge <- 'Q2\nMar `19'
    surge <- df %>% arrange(pd_order) %>% pull(period3) %>% nth(2)
    #end <- 'Q1\nDec `20'
    end <- df %>% arrange(pd_order) %>% pull(period3) %>% last()

    # Min / Max
    min <- df %>% pull(value) %>% min()
    max <- df %>% pull(value) %>% max()

    # gaps
    gap_top <- min %>% get_proportion(., 20)
    gap_bottom <- min %>% get_proportion(., 10)

    # c("HTS_TST_POS",
    #   "TX_CURR",
    #   "TX_NEW",
    #   "OVC_SERV",
    #   "PrEP_CURR",
    #   "KP_PREV")

    # colors
    ind_colors <- case_when(
      str_detect(indicator, "^TX") ~ "genoas",
      indicator == 'HTS_TST_POS' ~ "burnt_siennas",
      indicator == 'OVC_SERV' ~ "moody_blues",
      indicator == 'PrEP_CURR' ~ "scooters",
      indicator == 'KP_PREV' ~ "golden_sands"
    )

    # Plot
    viz <- df %>%
      ggplot(aes(x = reorder(period3, pd_order), y = value)) +
      geom_hline(yintercept = 0, color = usaid_darkgrey) +
      geom_rect(aes(xmin = -Inf, ymin = min, xmax = Inf, ymax = max),
                fill = grey10k, alpha = .1) +
      geom_hline(yintercept = min, color = usaid_lightgrey,
                 linetype = "dashed", lwd = .5) +
      geom_hline(yintercept = max, color = usaid_lightgrey,
                 linetype = "solid", lwd = .5) +
      geom_col(aes(fill = value), show.legend = F) +
      geom_line(aes(y = value + gap_top, group = 1), color = usaid_darkgrey) +
      geom_label(aes(y = value + gap_top, label = comma(value)),
                 size = 4, color = grey90k)

    if (indicator == "TX_CURR") {
      viz <- viz +
        annotate("text", x = surge, y = gap_bottom,
                 label = "SURGE", color = grey90k)
    }

    viz <- viz +
      # annotate("segment", x = start, xend = surge,
      #          y = max - gap_bottom, yend = max - gap_bottom,
      #          arrow = arrow(), size = 1, color = usaid_lightgrey) +
      annotate("text", x = start , y = max - (gap_bottom/2) ,
               label = "GAIN", color = grey90k) +
      scale_fill_si(
        palette = ind_colors,
        discrete = FALSE,
        alpha = 0.9
      ) +
      labs(x = "", y = "",
           title = paste0(indicator)
           # caption = paste0(
           #   indicator,
           #   " Targets: ",
           #   "FY19 = ", comma(t_fy19),
           #   ", FY20 = ", comma(t_fy20),
           #   ", FY21 = ", comma(t_fy21),
           #   "\nSource: DATIM MSD ",
           #   msd_caption,
           #   " - Produced by OHA/SIEI/SI on ",
           #                  format(Sys.Date(), "%Y%m%d"))
           ) +
      si_style_nolines() +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold")
      )

    print(viz)

    if (save == TRUE) {
      si_save(
        filename = file.path(
          dir_graphics,
          paste0("NIGERIA - ", indicator, " Trend since start of surge - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = viz,
        width = 10,
        height = 6)
    }
  }

# LOAD DATA ----

  # Data
  df_site <- file_site_im %>% read_msd()

  df_psnu <- file_psnu_im %>% read_msd()

# MUNGING ----

  # data
  df_psnu <- df_psnu %>% reshape_msd()

  df_psnu %>% glimpse()

  df_psnu %>%
    distinct(indicator) %>%
    arrange(indicator) %>%
    prinf()

  df_psnu %>%
    filter(fundingagency == agency,
           indicator %in% inds,
           period_type == "results") %>%
    distinct(indicator, standardizeddisaggregate) %>%
    arrange(indicator, standardizeddisaggregate) %>%
    prinf()

  df_psnu %>%
    filter(fundingagency == agency,
           indicator %in% inds,
           period_type == "results") %>%
    distinct(period, indicator, standardizeddisaggregate) %>%
    arrange(period, indicator, standardizeddisaggregate) %>%
    prinf()


  df_targets <- df_psnu %>%
    filter(fundingagency == agency,
           indicator %in% inds,
           period_type == "targets",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(period, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup()


  df_surge <- df_psnu %>%
    filter(fundingagency == agency,
           indicator %in% inds,
           period_type == "results",
           standardizeddisaggregate == "Total Numerator") %>%
    group_by(period, indicator) %>%
    summarise(across(value, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      pd_order = as.integer(str_remove_all(period, "FY|Q")),
      period1 = str_replace(period, "Q", " Q"),
      period2 = case_when(
        endsWith(period, "1") ~ paste0("Dec `", as.integer(str_sub(period, 3, 4)) - 1),
        endsWith(period, "2") ~ str_replace(period, "FY", "Mar `"),
        endsWith(period, "3") ~ str_replace(period, "FY", "Jun `"),
        endsWith(period, "4") ~ str_replace(period, "FY", "Sep `")
      ),
      period2 = str_remove(period2, "Q.*"),
      period2 = fct_reorder(period2, pd_order),
      period3 = paste0(str_sub(period, 5, 6), "\n", period2),
      period3 = fct_reorder(period3, pd_order)
    )


  df_surge %>% prinf()

  df_surge %>%
    arrange(indicator) %>%
    view()


# VIZ ----


  df_surge %>% surge_trend(df_targets)
  df_surge %>% surge_trend(df_targets, "HTS_TST_POS")

  df_surge %>%
    distinct(indicator) %>%
    pull() %>%
    map(~surge_trend(df_surge, df_targets, .x, save = TRUE))

  df_surge %>%
    filter(indicator == 'TX_CURR') %>%
    ggplot(aes(x = reorder(period3, pd_order), y = value)) +
      geom_col(aes(fill = value), show.legend = F) +
      geom_line(aes(y = value + 15000, group = 1), color = usaid_darkgrey) +
      geom_hline(yintercept = 0, color = usaid_darkgrey) +
      geom_label(aes(y = value + 15000,
                     label = comma(value)),
                 size = 4,
                 color = grey90k) +
      scale_fill_si(
        palette = "genoas", #"burnt_siennas",
        discrete = FALSE,
        alpha = 0.8
      ) +
      labs(x = "", y = "") +
      si_style_nolines() +
      theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10)
      )








