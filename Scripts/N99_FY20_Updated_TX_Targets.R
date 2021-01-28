##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: Update TX Targets
##  LICENCE: MIT
##  DATE:    2021-01-19


# PACKAGES -------------------------------------------------

library(tidyverse)
library(readxl)
library(glitr)
library(scales)
library(glamr)
library(gisr)
library(janitor)
library(extrafont)

# GLOBAL --------------------------------------------------

  # Load configs
  source("./Scripts/N00_Config.R")

  # Country name
  country <- "Nigeria"

  # file
  file_targets <- list.files(
      path = data,
      pattern = "^Site Tool_Nig.*_\\d{14}_F.*.xlsx$",
      full.names = TRUE
    )

  # Latest MSD PSNU x IM File
  file_msd <- list.files(
      path = merdata,
      pattern = "MER_S.*_PSNU_IM_.*_\\d{8}_v.*_N.*.zip",
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

  # Latest MSD Site x IM File
  file_msd_sites <- list.files(
      path = merdata,
      pattern = "MER_S.*_Site_IM_.*_\\d{8}_v.*_N.*.zip",
      full.names = TRUE
    ) %>%
    sort() %>%
    last()

# DATA ----------------------------------------------------

  # MSD Data

  # PSNU
  df_msd <- file_msd %>%
    read_msd() %>%
    reshape_msd(clean = TRUE)

  df_msd %>% glimpse()

  # Sites
  df_msd_sites <- file_msd_sites %>%
    read_msd() %>%
    reshape_msd(clean = TRUE)

  df_msd_sites %>% glimpse()

  # IMs
  df_ims <- df_msd %>%
    distinct(mech_code, mech_name, primepartner)

  df_ims %>% glimpse()

  # MSD PSNU Targets
  df_msd_psnu_trgts <- df_msd %>%
    clean_agency() %>%
    filter(operatingunit == country,
           #fundingagency == "USAID",
           fundingagency %in% c("USAID", "CDC"),
           indicator %in% c("TX_CURR", "TX_NEW"),
           period == "FY20",
           period_type %in% c("targets", "cumulative"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(psnu, indicator, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup()

  df_msd_psnu_trgts %>% glimpse()

  # MSD IM Targets
  df_msd_im_trgts <- df_msd %>%
    filter(operatingunit == country,
           #fundingagency == "USAID",
           fundingagency %in% c("USAID", "CDC"),
           indicator %in% c("TX_CURR", "TX_NEW"),
           period == "FY20",
           period_type %in% c("targets", "cumulative"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    group_by(mech_code, indicator, period_type) %>%
    summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
    ungroup()

  df_msd_im_trgts %>% glimpse()


  # Updated targets

  df_targets <- file_targets %>%
    read_excel(sheet = "TX", skip = 4) %>%
    clean_names() %>%
    rename(tx_curr_n = tx_curr_n_age_sex_hiv_status_20t,
           tx_new_n = tx_new_n_age_sex_hiv_status_20t,
           tx_pvls_n = tx_pvls_n_age_sex_indication_hiv_status_20t_routine,
           tx_pvls_d = tx_pvls_d_age_sex_indication_hiv_status_20t_routine)

  df_targets %>% glimpse()

  df_targets %>% head()

  df_trgts <- df_targets %>%
    separate(site, into = c("psnu", "facility"), sep = " > ", remove = FALSE) %>%
    mutate(site_type = str_extract(site, " (?<=\\[#).*(?=\\] )"),
           facility = if_else(is.na(facility), psnu, facility),
           psnu = if_else(str_detect(psnu, "_Mil"), "_Military Nigeria", psnu)) %>%
    separate(facility, into = c("facility", NA, "orgunituid"), sep = " \\[") %>%
    mutate_at(vars(facility, orgunituid), str_remove, pattern = "\\]") %>%
    mutate(mechanism = str_replace(mechanism, " - ", " -- ")) %>%
    separate(mechanism, into = c("mech_code", "partner"), sep = " -- ", remove = F) %>%
    mutate(mechanism = str_replace(mechanism, " -- ", " - ")) %>%
    relocate(site_type, .after = site) %>%
    pivot_longer(cols = starts_with("tx_"),
                 names_to = "indicator",
                 values_to = "val") %>%
    mutate(denominatornumerator = str_sub(indicator, -1),
           denominatornumerator = str_to_upper(denominatornumerator)) %>%
    mutate(indicator = str_sub(indicator, 1, -3),
           indicator = str_to_upper(indicator),
           period_type = "cop19") %>%
    relocate(val, .after = last_col())


  df_trgts %>% glimpse()

  View(df_trgts)


  # PSNU targets
  df_psnu_trgts <- df_trgts %>%
    filter(status == "Active") %>%
    group_by(psnu, indicator, period_type, denominatornumerator) %>%
    summarise_at(vars(val), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           denominatornumerator == "N") %>%
    select(-denominatornumerator) %>%
    bind_rows(df_msd_psnu_trgts) %>%
    mutate(indicator = paste0(indicator, "_", period_type),
           indicator = str_to_lower(indicator)) %>%
    select(-period_type) %>%
    pivot_wider(names_from = indicator,
                values_from = val) %>%
    mutate(
      tx_curr_ach1 = round(tx_curr_cumulative / tx_curr_targets * 100),
      tx_curr_ach2 = round(tx_curr_cumulative / tx_curr_cop19 * 100),
      tx_new_ach1 = round(tx_new_cumulative / tx_new_targets * 100),
      tx_new_ach2 = round(tx_new_cumulative / tx_new_cop19 * 100)
    ) %>%
    relocate(tx_curr_cumulative, tx_curr_targets, tx_curr_cop19,
             tx_curr_ach1, tx_curr_ach2,
             tx_new_cumulative, tx_new_targets, tx_new_cop19,
             tx_new_ach1, tx_new_ach2,
             .after = psnu)

  df_psnu_trgts %>% glimpse()

  df_psnu_trgts %>% prinf()

  #df_psnu_trgts %>% View()

  # Export
  write_csv(x = df_psnu_trgts,
            file = file.path(dataout,
                             paste0(country,
                                    "_TX_PSNU_Performance_under2scenarios_", format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")


  # Prep data for viz
  df_psnu_trgts2 <- df_psnu_trgts %>%
    filter(!is.na(tx_curr_cumulative), !is.na(tx_new_cumulative)) %>%
    pivot_longer(cols = starts_with("tx"),
                 names_to = "indicator",
                 values_to = "val") %>%
    mutate(indicator = str_replace(indicator, "_", "-")) %>%
    separate(indicator, into = c("indicator", "metric"), sep = "_") %>%
    mutate(indicator = str_replace(indicator, "-", "_"),
           indicator = str_to_upper(indicator),
           metric = factor(metric,
                           levels = c("cumulative", "targets", "cop19", "ach1", "ach2"),
                           labels = c("Results", "Current Targets", "Updated Targets", "Achievement 1", "Achievement 2")))


  # Facility TX_CURR Targets

  df_msd_sites %>% glimpse()

  df_agency_sites <- df_msd_sites %>%
    clean_agency() %>%
    filter(operatingunit == country,
           #fundingagency %in% c("USAID", "CDC", "DOD"),
           indicator == "TX_CURR",
           period == "FY20",
           period_type %in% c("targets", "cumulative"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>%
    distinct(fundingagency, facilityuid)

  df_tx_curr_sites <- df_trgts %>%
    filter(indicator == "TX_CURR",
           denominatornumerator == "N",
           period_type == "cop19",
           status == "Active") %>%
    group_by(psnu, facility, orgunituid) %>%
    summarise(val = sum(val, na.rm = TRUE)) %>%
    ungroup()

  df_tx_curr_sites %>% glimpse()

  #df_msd %>% glimpse()

  df_sites <- extract_locations(country, datim_user(), datim_pwd()) %>%
    extract_facilities()

  df_sites <- df_sites %>%
    select(id, longitude, latitude)

  df_sites %>% glimpse()

  df_tx_curr_sites <- df_tx_curr_sites %>%
    left_join(df_sites, by = c("orgunituid" = "id")) %>%
    left_join(df_agency_sites, by = c("orgunituid" = "facilityuid"))

  df_tx_curr_sites %>% glimpse()

  df_tx_curr_sites %>%
    filter(val > 0, is.na(longitude)) %>%
    group_by(fundingagency, psnu) %>%
    summarise(sites = n(), tx_curr = sum(val, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(-tx_curr) %>%
    gt()


  # IM targets
  df_im_trgts <- df_trgts %>%
    filter(status == "Active") %>%
    group_by(mech_code, indicator, period_type, denominatornumerator) %>%
    summarise_at(vars(val), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(indicator %in% c("TX_CURR", "TX_NEW"),
           denominatornumerator == "N") %>%
    select(-denominatornumerator) %>%
    bind_rows(df_msd_im_trgts) %>%
    mutate(indicator = paste0(indicator, "_", period_type),
           indicator = str_to_lower(indicator)) %>%
    select(-period_type) %>%
    pivot_wider(names_from = indicator,
                values_from = val) %>%
    mutate(
      tx_curr_ach1 = round(tx_curr_cumulative / tx_curr_targets * 100),
      tx_curr_ach2 = round(tx_curr_cumulative / tx_curr_cop19 * 100),
      tx_new_ach1 = round(tx_new_cumulative / tx_new_targets * 100),
      tx_new_ach2 = round(tx_new_cumulative / tx_new_cop19 * 100)
    ) %>%
    left_join(df_ims, by = "mech_code") %>%
    relocate(mech_name, primepartner, .after = mech_code) %>%
    relocate(tx_curr_cumulative, tx_curr_targets, tx_curr_cop19,
             tx_curr_ach1, tx_curr_ach2,
             tx_new_cumulative, tx_new_targets, tx_new_cop19,
             tx_new_ach1, tx_new_ach2,
             .after = primepartner)

  df_im_trgts %>% glimpse()

  df_im_trgts %>% prinf()

  df_im_trgts %>% View()

  df_im_trgts %>% gt()

  # Export
  write_csv(x = df_im_trgts,
            file = file.path(dataout,
                             paste0(country,
                                    "_TX_IM_Performance_under2scenarios_",
                                    format(Sys.Date(), "%Y%m%d"),
                                    ".csv")), na = "")

  # Prep data for viz
  df_im_trgts2 <- df_im_trgts %>%
    filter(!is.na(tx_curr_cumulative), !is.na(tx_new_cumulative)) %>%
    pivot_longer(cols = starts_with("tx"),
                 names_to = "indicator",
                 values_to = "val") %>%
    mutate(indicator = str_replace(indicator, "_", "-")) %>%
    separate(indicator, into = c("indicator", "metric"), sep = "_") %>%
    mutate(indicator = str_replace(indicator, "-", "_"),
           indicator = str_to_upper(indicator),
           metric = factor(metric,
                           levels = c("cumulative", "targets", "cop19", "ach1", "ach2"),
                           labels = c("Results", "Current Targets", "Updated Targets", "Achievement 1", "Achievement 2")))

  View(df_im_trgts2)

# VIZ -------------------------------

  # SNU1 TX Results

  # Change color
  cols <- c(
    "dicrease" = usaid_red,
    "increase" = grey70k,
    "no change" = grey70k
  )

  # TX_CURR
  df_psnu_tx_curr <- df_psnu_trgts2 %>%
    group_by(psnu, indicator) %>%
    mutate(psnu_label = paste0(psnu, " (",
                               val[metric == "Achievement 1"], "% / ",
                               val[metric == "Achievement 2"], "%)"),
           change = val[metric == "Achievement 2"] - val[metric == "Achievement 1"],
           change_label = case_when(
             change < 0 ~ "dicrease",
             change > 0 ~ "increase",
             TRUE ~ "no change"
           )) %>%
    ungroup() %>% #view()
    filter(indicator == "TX_CURR", !str_detect(metric, "Ach")) %>%
    arrange(fct_relevel(metric, "Results"), val) %>%
    mutate(psnu_label = fct_inorder(psnu_label))

  df_psnu_tx_curr %>%
    ggplot(aes(psnu_label, val,
               fill = metric, group = metric,
               label = comma(val, 1))) +
    geom_col(position = position_dodge(), show.legend = F) +
    geom_text(size = 3, color = grey80k, hjust = 0) +
    scale_fill_si(palette = "category20", discrete = TRUE) +
    facet_wrap(metric ~ .) +
    coord_flip(clip = "off") +
    labs(title = "NIGERIA - TX_CURR Achievements by States",
         subtitle = "COP19 Targets have been updated, and achievements for states in red will dicrease",
         x = "", y = "",
         caption = paste0("Source: MSD F20Q4c & USAID/Nigeria Updated Site Tool
                          OHA/SIEI - Strategic Information Branch, ", format(Sys.Date(), "%Y-%m-%d"))) +
    si_style_xgrid() +
    theme(axis.text.y = element_text(color = cols[df_psnu_tx_curr$change_label]),
          strip.placement = "outside")

  # Save output
  ggsave(file.path(graphics, paste0("Nigeria - TX_CURR - Achievements by SNU - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")

  # TX_NEW
  df_psnu_tx_new <- df_psnu_trgts2 %>%
    group_by(psnu, indicator) %>%
    mutate(psnu_label = paste0(psnu, " (",
                               val[metric == "Achievement 1"], "% / ",
                               val[metric == "Achievement 2"], "%)"),
           change = val[metric == "Achievement 2"] - val[metric == "Achievement 1"],
           change_label = case_when(
             change > 0 ~ "increase",
             change < 0 ~ "dicrease",
             TRUE ~ "no change"
           )) %>%
    ungroup() %>%
    filter(indicator == "TX_NEW", !str_detect(metric, "Ach")) %>%
    arrange(fct_relevel(metric, "Results"), val) %>%
    mutate(psnu_label = fct_inorder(psnu_label))

  df_psnu_tx_new %>%
    ggplot(aes(psnu_label, val,
               fill = metric, group = metric,
               label = comma(val, 1))) +
    geom_col(position = position_dodge(), show.legend = F) +
    geom_text(size = 3, color = grey80k, hjust = 0) +
    scale_fill_si(palette = "category10", discrete = TRUE) +
    scale_y_continuous(position = "right") +
    facet_wrap(metric ~ .) +
    coord_flip(clip = "off") +
    labs(title = "NIGERIA - TX_NEW Achievements by States",
         subtitle = "COP19 Targets have been updated, and achievements\nfor states in red will dicrease. States are sorted by results",
         x = "", y = "",
         caption = paste0("Source: MSD F20Q4c & USAID/Nigeria Updated Site Tool
                          OHA/SIEI - Strategic Information Branch, ", format(Sys.Date(), "%Y-%m-%d"))) +
    si_style_xgrid() +
    theme(axis.text.y = element_text(color = cols[df_psnu_tx_new$change_label]),
          strip.placement = "outside")

  # Save output
  ggsave(file.path(graphics, paste0("Nigeria - TX_NEW - Achievements by SNU - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")


  # Map TX_CURR Sites

  basemap <- terrain_map(country,
                         terr_path = si_path("path_raster"),
                         mask = TRUE)

  basemap

  nga_adm0 <- get_admin0(country) %>% select(admin)

  nga_adm1 <- get_admin1(country) %>%
    select(name) %>%
    mutate(name = if_else(name == "Federal Capital Territory", "FCT", name))

  df_tx_curr_sites %>%
    filter(val > 0, !is.na(longitude)) %>%
    distinct(val) %>%
    min()

  df_tx_curr_sites %>%
    filter(val > 0, !is.na(longitude)) %>%
    distinct(val) %>%
    max()


  basemap +
    geom_point(data = df_tx_curr_sites %>%
                 filter(val > 0, !is.na(longitude)),
               aes(longitude, latitude, fill = fundingagency),
               shape = 21, color = "white", size = 3, alpha = .8, show.legend = F) +
    geom_sf(data = nga_adm1, fill = NA, color = grey50k, size = .2) +
    geom_sf(data = nga_adm0, fill = NA, colour = "grey93", size = 1) +
    geom_sf(data = nga_adm0, fill = NA, colour = grey50k, size = .5) +
    geom_sf_text(data = nga_adm1, aes(label = name), color = grey60k) +
    scale_fill_manual(values = c(usaid_lightblue, usaid_red, grey90k)) +
    labs(title = "NIGERIA - FY20 TX_CURR Sites Distribution by Agency",
         subtitle = "USAID Sites are in red and CDC Sites are light blue.",
         x = "", y = "",
         caption = paste0("Source: MSD F20Q4c & USAID/Nigeria Updated Site Tool\nOHA/SIEI - Strategic Information Branch, ", format(Sys.Date(), "%Y-%m-%d"))) +
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5))


  # Save output
  ggsave(file.path(graphics, paste0("Nigeria - TX_CURR - Sites Locations - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")


  basemap +
    geom_point(data = df_tx_curr_sites %>%
                 filter(val > 0, !is.na(longitude)),
               aes(longitude, latitude, size = val),
               shape = 21, fill = NA, show.legend = T) +
    geom_point(data = df_tx_curr_sites %>%
                 filter(val > 0, !is.na(longitude)),
               aes(longitude, latitude, size = val, fill = fundingagency),
               shape = 21, alpha = .8, color = "white", show.legend = T) +
    geom_sf(data = nga_adm1, fill = NA, color = grey50k, size = .2) +
    geom_sf(data = nga_adm0, fill = NA, colour = "grey93", size = 1) +
    geom_sf(data = nga_adm0, fill = NA, colour = grey50k, size = .5) +
    geom_sf_text(data = nga_adm1, aes(label = name), color = grey70k, size = 2.5) +
    scale_fill_manual(name = "Funding Agency",
                      values = c(usaid_lightblue, usaid_red, grey90k)) +
    scale_size_area(name = "Targets",
                    breaks = c(1, 100, 1000, 10000, 15000, 20000, 25000), max_size = 20) +
    labs(title = "NIGERIA - FY20 TX_CURR Site Targets Distribution by Agency",
         subtitle = "USAID Sites are in red while CDC Sites are light blue.",
         x = "", y = "",
         caption = paste0("Source: MSD F20Q4c & USAID/Nigeria Updated Site Tool\nOHA/SIEI - Strategic Information Branch, ", format(Sys.Date(), "%Y-%m-%d"))) +
    guides(colour = guide_legend(show = F)) +
    theme(plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5))

  # Save output
  ggsave(file.path(graphics, paste0("Nigeria - TX_CURR - Sites Targets - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")
