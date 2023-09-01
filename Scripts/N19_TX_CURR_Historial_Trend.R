# PURPOSE: Si Support for Nigiria
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Historical TX Data - When was the surge?
# REF ID:  eceb00f7
# LICENSE: MIT
# DATE:    2023-09-01
# UPDATE:  2023-09-01
# NOTES:   This is for the "Getting the data right effort"

# Libraries ====

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gisr)
  library(gophr)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(glue)


# LOCALS & SETUP ====

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_cntry <- file.path("../../PEPFAR/COUNTRIES/country")


  # Files

  file_nat1 <- si_path() %>% return_latest("NAT_SUBNAT_FY15")
  file_nat2 <- si_path() %>% return_latest("NAT_SUBNAT_FY21")

  file_ou1 <- si_path() %>% return_latest("OU_IM_FY15")
  file_ou2 <- si_path() %>% return_latest("OU_IM_FY21")

  get_metadata(file_ou2)

  meta <- metadata

  # Set Params

  ref_id <- "eceb00f7"
  agency <- "USAID"
  cntry <- "Nigeria"

# Functions  =====

# LOAD DATA =====

  df_tx <- file_ou1 %>%
    c(file_ou2) %>%
    map_dfr(function(.x) {
      read_psd(.x) %>%
        filter(operatingunit == cntry,
               str_detect(indicator, "TX_"))
    })

# MUNGE =====

  df_tx %>% glimpse()

  df_tx %>% distinct(indicator) %>% pull()
  df_tx %>% distinct(funding_agency)
  df_tx %>% distinct(standardizeddisaggregate)

  df_tx_his_ou <- df_tx %>%
    clean_indicator() %>% #distinct(indicator) %>% pull()
    filter(funding_agency != "Dedup",
           indicator %in% c("TX_NEW", "TX_ML", "TX_RTT", "TX_NET_NEW", "TX_CURR",
                            "TX_PVLS", "TX_PVLS_D", "TX_VIRAL", "TX_VIRAL_D"),
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(cumulative, \(x) sum(cumulative, na.rm = T)),
              .by = c(fiscal_year, indicator, standardizeddisaggregate)) %>%
    arrange(fiscal_year) %>%
    filter(cumulative > 0)


# VIZ =====

  tx_range <- df_tx_his_ou %>%
    filter(indicator == "TX_CURR") %>%
    pull(cumulative) %>%
    range()

  tx_pds <- df_tx_his_ou %>%
    filter(indicator == "TX_CURR") %>%
    pull(fiscal_year) %>%
    range()

  viz_trend <- df_tx_his_ou %>%
    filter(indicator %in% c("TX_CURR", "TX_PVLS")) %>%
    ggplot(aes(x = fiscal_year, y = cumulative,
               group = indicator, label = comma(cumulative))) +
    geom_rect(xmin = 2019, xmax = 2022, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .05) +
    geom_vline(xintercept = c(2019, 2022), color = usaid_darkgrey, size = .6, linetype = "dashed") +
    geom_line(aes(color = indicator), size = 1) +
    geom_point(aes(fill = indicator), shape = 21, size = 4, color = grey10k) +
    geom_text(hjust = -.3, vjust = 1, size = 4) +
    annotate(geom = "text", x = 2020.5, y = 2000000, label = "SURGE", size = 6) +
    scale_color_manual(values = c("TX_CURR" = old_rose, "TX_PVLS" = genoa)) +
    scale_fill_manual(values = c("TX_CURR" = old_rose, "TX_PVLS" = genoa)) +
    scale_x_continuous(limits = c(2015, 2023), breaks = 2015:2023) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       limits = c(0, 2015000),
                       breaks = seq(0, 2015000, 250000)) +
    coord_cartesian(clip = "off") +
    labs(x = "", y = "",
         caption = paste0("Source: FY15-FY23Q3i - Viz updated on ", curr_date(), " - Ref. ID: ", ref_id),
         title = "NIGERIA - # of PEOPLE LIVING WITH HIV ON ARV",
         subtitle = glue("TX_CURR grew from {comma(tx_range[1])} to {comma(tx_range[2])} between {tx_pds[1]} and {tx_pds[2]}")) +
    si_style()

  viz_trend

  si_save(filename = file.path(dir_graphics, "Nigeria - Historical Treatment Volumes.png"),
          plot = viz_trend,
          scale = 2,
          width = 10,
          height = 6)



# OUTPUTS =====

