# PROJECT: si-naija
# PURPOSE: PMTCT Coverage Gaps
# AUTHOR: Baboyma Kagniniwa | USAID/GH - Office of HIV-AIDS
# LICENSE: MIT
# REF. ID: 917c6aad
# CREATED: 2024-11-19
# UPDATED: 2024-11-19
# NOTES:

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(gagglr)
  library(grabr)
  library(gisr)
  library(sf)
  library(glitr)
  library(tidytext)
  library(ggtext)
  library(scales)
  library(patchwork)
  library(systemfonts)
  library(glue)
  library(janitor)

# Set paths  ====

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_datim <- glamr::si_path("path_datim")


# Params

  ref_id <- "917c6aad"
  ou <-  "Nigeria"
  cntry <- ou
  agency <- "USAID"

# FILES

  file_nat <- dir_mer %>%
    return_latest(glue("NAT_SUBNAT_FY22.*.zip$"))

  file_ou <- dir_mer %>%
    return_latest(glue("OU_IM_FY22.*.zip$"))

  file_psnu <- dir_mer %>%
    return_latest(glue("PSNU_IM_FY22.*_{ou}.zip$"))

  file_site <- dir_mer %>%
    return_latest(glue("Site_IM_FY22.*_{ou}.zip$"))

  meta <- file_psnu %>% get_metadata()

# Functions  ====



# LOAD DATA ====

  ## NAT-SUBNAT
  df_nat <- file_nat %>%
    read_psd() %>%
    filter(operatingunit == cntry)

  ## PSNU x IM
  df_msd <- file_psnu %>% read_psd()

# MUNGE ====


  df_nat %>% glimpse()

  df_nat %>% distinct(indicator)

  df_nat %>%
    filter(source_name != "DATIM") %>%
    distinct(indicator)

  ## Pops

  df_pops <- df_nat %>%
    select(!1:cop22_snuprioritization,
           -c(snuprioritization, categoryoptioncomboname, otherdisaggregate, qtr4, source_name),
           -contains(c("_age","age_"))) %>%
    filter(indicatortype == "Sub-national",
           str_detect(indicator, "POP|PLHIV")) %>%
    clean_indicator() %>%
    select(!c(indicatortype, numeratordenom)) %>%
    relocate(fiscal_year, .before = 1)

  ## PMTCT

  df_pmtct <- df_msd %>%
    filter(str_detect(indicator, "PMTCT")) %>%
    clean_agency() %>%
    clean_indicator() %>%
    mutate(
      indicator = case_when(
        str_detect(indicator, "Two_Twelve_Months") ~ str_replace(indicator, "Two_Twelve_Months", "12M"),
        str_detect(indicator, "Less_Equal_Two_Months") ~ str_replace(indicator, "Less_Equal_Two_Months", "2M"),
        TRUE ~ indicator
      )
    )

  df_pmtct %>% distinct(indicator)
  df_pmtct %>% distinct(indicator, standardizeddisaggregate) %>% arrange(indicator)
  df_pmtct %>% distinct(indicator, sex, ageasentered)

  df_pmtct_cov <- df_pmtct %>%
    filter(
      indicator %in% c("PMTCT_STAT", "PMTCT_STAT_POS") &
        standardizeddisaggregate == "Age/Sex/KnownNewResult" |
      indicator == "PMTCT_STAT_D" &
        standardizeddisaggregate == "Total Denominator" |
      indicator == "PMTCT_ART" &
        standardizeddisaggregate == "Age/NewExistingArt/Sex/HIVStatus" |
      indicator %in% c("PMTCT_EID", "PMTCT_EID_2M", "PMTCT_EID_12M") &
        standardizeddisaggregate == "Total Numerator" |
      indicator == "PMTCT_EID_D" & standardizeddisaggregate == "Total Denominator"
    ) %>%
    reshape_msd() %>%
    summarise(
      value = sum(value, na.rm = T),
      .by = c(period, period_type, funding_agency, psnuuid, psnu,
              indicator, statushiv, otherdisaggregate)
    ) %>%
    mutate(
      statushiv = case_when(
        indicator %in% c("PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_STAT_D") &
          !is.na(otherdisaggregate) ~ paste0(statushiv, " - ", otherdisaggregate),
        indicator %in% c("PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_STAT_D") &
          !is.na(otherdisaggregate) ~ "All Clients",
        TRUE ~ statushiv
      ),
      screening = case_when(
        indicator %in% c("PMTCT_STAT", "PMTCT_STAT_D") &
          is.na(otherdisaggregate) ~ "ANC1 Clients",
        indicator %in% c("PMTCT_STAT", "PMTCT_STAT_D") &
          !is.na(otherdisaggregate) ~ "Clients Screened",
        indicator == "PMTCT_STAT_POS" ~ "HIV+ Pregnant Women",
        indicator == "PMTCT_ART" ~ "HIV+ Pregnant Women on ART",
        indicator == "PMTCT_EID" ~ "Enfant with VL Test",
        indicator == "PMTCT_EID_2M" ~ "Enfant with VL Test by 2M",
        indicator == "PMTCT_EID_12M" ~ "Enfant with VL Test btw 2 & 12M",
        indicator == "PMTCT_EID_D" ~ "Enfant eligible for VL Test",
        TRUE ~ NA_character_
      )
    )

  df_pmtct_cov %>% distinct(statushiv)
  df_pmtct_cov %>% distinct(screening)


# VIZ ====

  viz_screen <- df_pmtct_cov %>%
    filter(period == meta$curr_fy_lab, period_type == "cumulative",
           indicator %in% c("PMTCT_STAT", "PMTCT_STAT_D")) %>%
    summarise(value = sum(value, na.rm = T),
              .by = c(period, indicator, screening)) %>%
    mutate(coverage = value[indicator == "PMTCT_STAT"] / value[indicator == "PMTCT_STAT_D"]) %>%
    ggplot(aes(screening, value/1e6, fill = screening)) +
    geom_col(width = .7) +
    geom_hline(yintercept = 0, size = 1, color = usaid_medgrey) +
    geom_text(aes(label = comma(value)),
              size = 6, color = usaid_black,
              fontface = "bold", vjust = 1.5) +
    scale_fill_manual(
      values = c(
        "ANC1 Clients" = moody_blue,
        "Clients Screened" = moody_blue_light
      )) +
    scale_y_continuous(labels = comma_format(suffix="M")) +
    labs(x="", y="") +
    si_style_ygrid() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 15, color = usaid_black, face = "bold"))


  viz_pos <- df_pmtct_cov %>%
    filter(period == meta$curr_fy_lab, period_type == "cumulative",
           indicator %in% c("PMTCT_STAT", "PMTCT_STAT_D"),
           screening == "Clients Screened") %>%
    summarise(value = sum(value, na.rm = T),
              .by = c(period, indicator, statushiv)) %>%
    mutate(prop = value / sum(value)) %>%
    filter(str_detect(statushiv, "^Positive")) %>%
    ggplot(aes(value/1e3, fct_reorder(statushiv, value))) +
    geom_col(width = .7, fill = burnt_sienna) +
    geom_vline(xintercept = 0, size = 1, color = usaid_darkgrey) +
    geom_text(aes(x = 0, label = statushiv),
              size = 8, fontface = "bold",
              hjust = -0.05, vjust = -8, color = usaid_darkgrey) +
    geom_text(aes(label = paste0(comma(value), "\n(", percent(prop, .01), ")")),
              size = 8, fontface = "bold", hjust = 1.3, color = grey10k) +
    scale_x_continuous(labels = comma_format(suffix="K"), position = "top") +
    labs(x="", y="") +
    si_style_xgrid() +
    theme(axis.text.y = element_blank())

  df_pmtct_cov %>%
    filter(period == meta$curr_fy_lab, period_type == "cumulative",
           str_detect(indicator, "STAT_POS|ART|EID|EID_D")) %>%
    summarise(value = sum(value, na.rm = T),
              .by = c(period, indicator, screening)) %>%
    #filter(str_detect(statushiv, "^Positive")) %>%
    ggplot(aes(screening, value/1e3)) +
    geom_col(width = .7, fill = burnt_sienna) +
    geom_vline(xintercept = 0, size = 1, color = usaid_darkgrey) +
    geom_text(aes(label = comma(value)),
              size = 8, fontface = "bold", vjust = 1, color = grey10k) +
    scale_y_continuous(labels = comma_format(suffix="K"), position = "top") +
    labs(x="", y="") +
    si_style_ygrid() +
    theme(axis.text.x = element_markdown())

  (viz_screen + viz_pos)


# EXPORT ====

