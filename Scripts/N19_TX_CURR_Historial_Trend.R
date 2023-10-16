# PURPOSE: Si Support for Nigeria
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Historical TX Data - When was the surge?
# REF ID:  eceb00f7
# LICENSE: MIT
# DATE:    2023-09-01
# UPDATE:  2023-10-05
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

  file_psnu1 <- si_path() %>% return_latest("PSNU_IM_FY15")
  file_pnsu2 <- si_path() %>% return_latest("PSNU_IM_FY21")

  get_metadata(file_ou2)

  meta <- metadata

  # Set Params

  ref_id <- "eceb00f7"
  agency <- "USAID"
  cntry <- "Nigeria"

  meta$caption <- paste(meta$caption, "| Ref. ID:", ref_id)

# Functions  =====

# LOAD DATA =====

  df_plhiv <- file_nat1 %>%
    c(file_nat2) %>%
    map_dfr(function(.x) {
      read_psd(.x) %>%
        filter(operatingunit == cntry,
               indicator == "PLHIV",
               standardizeddisaggregate == "Total Numerator")
    })

  df_tx <- file_ou1 %>%
    c(file_ou2) %>%
    map_dfr(function(.x) {
      read_psd(.x) %>%
        filter(operatingunit == cntry,
               str_detect(indicator, "TX_"))
    })

  df_tx_psnu <- file_psnu1 %>%
    c(file_psnu1) %>%
    map_dfr(function(.x) {
      read_psd(.x) %>%
        filter(operatingunit == cntry,
               str_detect(indicator, "TX_"))
    })

# MUNGE =====

  df_hiv <- df_plhiv %>%
    summarise(across(targets, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnu, indicator)) %>%
    rename(plhiv = targets) %>%
    select(-indicator)

  df_tx %>% glimpse()

  df_tx %>% distinct(indicator) %>% pull()
  df_tx %>% distinct(funding_agency)
  df_tx %>% distinct(standardizeddisaggregate)

  df_tx_curr <- df_tx_psnu %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(cumulative, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnu, indicator)) %>%
    rename(tx_curr = cumulative) %>%
    select(-indicator) %>%
    left_join(df_hiv, by = c("fiscal_year", "psnu")) %>%
    mutate(art_sat = tx_curr / plhiv)

  df_tx_curr %>%
    filter(!is.na(tx_curr), !is.na(plhiv)) %>%
    summarise(across(c(tx_curr, plhiv), \(x) sum(x, na.rm = T)),
              .by = fiscal_year) %>%
    mutate(psnu = "USAID",
           art_sat = tx_curr / plhiv) %>%
    bind_rows(df_tx_curr, .) %>%
    arrange(fiscal_year, psnu) %>%
    write_csv(file = file.path(dir_dataout, "Nigeria - historical art saturation.csv"),
              na = "")


  df_tx_hist_ou_all <- df_tx %>%
    clean_indicator() %>%
    mutate(
      indicator = case_when(
        str_detect(indicator, "TX_ML_IIT") ~ "TX_ML_IIT",
        TRUE ~ indicator
      )
    ) %>% #distinct(indicator) %>% pull()
    filter(funding_agency %ni% c("Dedup", "Default"),
           indicator %in% c("TX_NEW", "TX_ML", "TX_ML_IIT",
                            "TX_RTT", "TX_NET_NEW", "TX_CURR",
                            "TX_PVLS", "TX_PVLS_D", "TX_VIRAL", "TX_VIRAL_D"),
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(cumulative, \(x) sum(cumulative, na.rm = T)),
              .by = c(fiscal_year, funding_agency, indicator, standardizeddisaggregate)) %>%
    clean_agency() %>%
    arrange(fiscal_year, funding_agency, indicator) %>%
    filter(cumulative > 0)

  df_tx_hist_ou_all <- df_tx_hist_ou_all %>%
    summarise(across(cumulative, \(x) sum(cumulative, na.rm = T)),
              .by = c(fiscal_year, indicator, standardizeddisaggregate)) %>%
    mutate(funding_agency = "OU") %>%
    bind_rows(df_tx_hist_ou_all, .) %>%
    arrange(fiscal_year, funding_agency, indicator)

  df_tx_hist_ou_all %>% distinct(indicator) %>% pull()

# VIZ =====


  # TX_CURR & TX_PVLS

  df_tx_hist_ou <- df_tx_hist_ou_all %>%
    filter(indicator %in% c("TX_CURR", "TX_PVLS"),
           fiscal_year >= 2018)

  tx_range <- df_tx_hist_ou %>%
    filter(indicator == "TX_CURR") %>%
    pull(cumulative) %>%
    range()

  tx_pds <- df_tx_hist_ou %>%
    filter(indicator == "TX_CURR") %>%
    pull(fiscal_year) %>%
    range()

  viz_trend <- df_tx_hist_ou %>%
    mutate(funding_agency = factor(funding_agency, levels = c("OU", "CDC", "USAID", "DOD"), ordered = T)) %>%
    filter(funding_agency != "DOD") %>%
    ggplot(aes(x = fiscal_year, y = cumulative,
               label = comma(cumulative))) +
    geom_rect(xmin = 2019, xmax = 2022, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .05) +
    geom_vline(xintercept = c(2019, 2022), color = usaid_darkgrey, size = .6, linetype = "dashed") +
    geom_line(aes(color = indicator, group = indicator), linewidth = 1.5) +
    geom_point(aes(fill = indicator, group = funding_agency), shape = 21, size = 5, color = grey10k) +
    geom_text(vjust = 1.5, hjust = "inward", size = 4, color = grey90k) +
    annotate(geom = "text", x = 2020.5, y = 2000000, label = "SURGE", size = 6) +
    scale_color_manual(values = c("TX_CURR" = old_rose, "TX_PVLS" = genoa)) +
    scale_fill_manual(values = c("TX_CURR" = old_rose, "TX_PVLS" = genoa)) +
    scale_x_continuous(limits = c(tx_pds[1], tx_pds[2]), breaks = tx_pds[1]:tx_pds[2]) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       limits = c(0, tx_range[2] + 1000),
                       breaks = seq(0, tx_range[2] + 1000, 250000)) +
    coord_cartesian(clip = "off") +
    facet_wrap(~funding_agency, ncol = 4) +
    labs(x = "", y = "",
         caption = glue("Source: FY23Q3c MSD | Ref. ID: eceb00f7 - Updated on {curr_date()}"),
         title = "NIGERIA - # of PEOPLE LIVING WITH HIV ON ARV",
         subtitle = glue("TX_CURR grew from {comma(tx_range[1])} to {comma(tx_range[2])} between {tx_pds[1]} and {tx_pds[2]}")) +
    si_style() +
    theme(legend.title = element_blank())

  viz_trend

  si_save(filename = file.path(dir_graphics, "Nigeria - Historical Treatment Volumes.png"),
          plot = viz_trend,
          scale = 2,
          width = 10,
          height = 6)

  # TX_CURR & TX_IIT

  df_tx_hist_ou_all %>% distinct(indicator)


  df_tx_lost_hist_ou <- df_tx_hist_ou_all %>%
    filter(indicator %in% c("TX_NEW", "TX_ML", "TX_ML_IIT"),
           fiscal_year >= 2018)

  tx_lost_range <- df_tx_lost_hist_ou %>%
    pull(cumulative) %>%
    range()

  tx_lost_pds <- df_tx_lost_hist_ou %>%
    pull(fiscal_year) %>%
    range()

  viz_lost_trend <- df_tx_lost_hist_ou %>%
    mutate(funding_agency = factor(funding_agency, levels = c("OU", "CDC", "USAID", "DOD"), ordered = T)) %>%
    filter(funding_agency != "DOD") %>%
    ggplot(aes(x = fiscal_year, y = cumulative,
               group = indicator, label = comma(cumulative))) +
    geom_rect(xmin = 2019, xmax = 2022, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .07) +
    geom_vline(xintercept = c(2019, 2022), color = usaid_darkgrey, size = .6, linetype = "dashed") +
    geom_line(aes(color = indicator), linewidth = 1.5) +
    geom_point(aes(fill = indicator), shape = 21, size = 5, color = grey10k) +
    geom_text(hjust = "inward", vjust = 1.5, size = 4, color = grey90k) +
    annotate(geom = "text", x = 2020.5, y = tx_lost_range[2] + 1000, label = "SURGE", size = 6) +
    scale_color_manual(values = c("TX_NEW" = scooter, "TX_NET_NEW" = scooter_light, "TX_ML" = burnt_sienna, "TX_ML_IIT" = burnt_sienna_light)) +
    scale_fill_manual(values = c("TX_NEW" = scooter, "TX_NET_NEW" = scooter_light, "TX_ML" = burnt_sienna, "TX_ML_IIT" = burnt_sienna_light)) +
    scale_x_continuous(limits = c(tx_pds[1], tx_pds[2]), breaks = tx_pds[1]:tx_pds[2]) +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                       limits = c(0, tx_lost_range[2] + 1000),
                       breaks = seq(0, tx_lost_range[2] + 1000, 100000)) +
    coord_cartesian(clip = "off") +
    facet_wrap(~funding_agency, ncol = 4) +
    labs(x = "", y = "",
         caption = glue("Source: FY23Q3c MSD | Ref. ID: eceb00f7 - Updated on {curr_date()}"),
         title = "NIGERIA - # of NEW HIV+ ON ARV",
         subtitle = glue("TX_NEW has fallen below pre-surve level while IIT # are on the rise")) +
    si_style() +
    theme(legend.title = element_blank())

  viz_lost_trend

  si_save(filename = file.path(dir_graphics, "Nigeria - Historical Treatment Lost.png"),
          plot = viz_lost_trend,
          scale = 2,
          width = 10,
          height = 6)



# OUTPUTS =====

