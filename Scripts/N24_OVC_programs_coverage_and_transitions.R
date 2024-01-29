# PURPOSE: SI-Naija
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: OVC - Partners / Agencies transition
# REF ID:  b9b9e6f9
# LICENSE: MIT
# DATE:    2024-01-29
# UPDATE:  2024-01-29
# NOTES:   Looking into transition & performance of OVC Programs

# Libraries ====

  library(tidyverse)
  library(glamr)
  library(gophr)
  library(glitr)
  library(gisr)
  library(sf)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
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

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT")
  file_ou1 <- si_path() %>% return_latest("OU_IM_FY15")
  file_ou2 <- si_path() %>% return_latest("OU_IM_FY21")
  file_psnu <- si_path() %>% return_latest("PSNU_IM_FY21-.*_Nigeria")
  file_site1 <- si_path() %>% return_latest("Site_IM_FY15-.*_Nigeria")
  file_site2 <- si_path() %>% return_latest("Site_IM_FY21-.*_Nigeria")

  get_metadata(file_psnu)

  meta <- metadata

  # Set Params

  ref_id <- "b9b9e6f9"
  agency <- "USAID"
  cntry <- "Nigeria"

  curr_fy <- source_info(path = file_psnu, type = "PSNU_IM", return = "period")
  curr_pd <- source_info(path = file_psnu, type = "PSNU_IM", return = "period")

  # Indicators

  ind_ovc <- "OVC"

# Functions  =====

# LOAD DATA =====

  df_ovc <- file_site1 %>%
    c(file_site2) %>%
    map(function(.file){
      .file %>%
        read_psd() %>%
        filter(str_detect(indicator, ind_ovc))
    }) %>%
    bind_rows()

  df_ovc %>% glimpse()

  df_ovc %>% distinct(indicator, standardizeddisaggregate)

# MUNGE =====

  df_mechs <- df_ovc %>%
    filter(funding_agency != "Dedup") %>%
    distinct(fiscal_year, funding_agency, mech_code, mech_name, prime_partner_name) %>%
    clean_agency() %>%
    mutate(partner = paste0(prime_partner_name, " [", funding_agency, "]"))

  df_ovc_cov <- df_ovc %>%
    filter(indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator") %>%
    summarise(across(cumulative, \(.x) sum(.x, na.rm =  TRUE)),
              .by = c(fiscal_year, orgunituid, sitename, psnu, mech_code)) %>%
    filter(cumulative > 0)

  df_ovc_trans <- df_ovc_cov %>%
    group_by(fiscal_year, mech_code, psnu) %>%
    summarise(
      site = n_distinct(orgunituid),
      value = sum(cumulative),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    group_by(mech_code, psnu) %>%
    mutate(
      site_prev = lag(site, order_by = fiscal_year),
      site_change = magrittr::subtract(site, site_prev),
      value_prev = lag(value, order_by = fiscal_year),
      value_change = magrittr::subtract(value, value_prev),
      value_direction = case_when(
        value_change > 0 ~ 24,
        value_change < 0 ~ 25,
        value_change == 0 ~ 23,
        TRUE ~ NA_integer_
      )
    ) %>%
    ungroup() %>%
    group_by(fiscal_year, psnu) %>%
    arrange(mech_code) %>%
    mutate(mech_position = ((row_number() / 10) - .1) * 1) %>%
    ungroup() %>%
    select(fiscal_year, mech_code, psnu, mech_position,
           starts_with("site"),
           starts_with("value")) %>%
    left_join(df_mechs, by = c("fiscal_year", "mech_code"),
              relationship = "many-to-many") %>%
    mutate(
      funding_agency = case_when(
        is.na(funding_agency) ~ "OTHER",
        TRUE ~ funding_agency
      )
    )


# VIZ =====

  # Time Frame
  hist_pd <- seq(min(df_ovc_trans$fiscal_year), max(df_ovc_trans$fiscal_year), 1)

  # Agencies

  df_ovc_trans %>% distinct(funding_agency)

  agencies <- c("CDC", "USAID", "DOD")

  # Sort psnu by OVC volume at the latest fy
  psnu_order <- df_ovc_trans %>%
    summarise(value = sum(value), .by = c(fiscal_year, psnu)) %>%
    filter(fiscal_year == max(fiscal_year)) %>%
    arrange(desc(value)) %>%
    pull(psnu)

  # Sort psnu (not reporting in latest fy) by OVC volume at their latest year
  psnu_order_previous <- df_ovc_trans %>%
    summarise(value = sum(value), .by = c(fiscal_year, psnu)) %>%
    filter(fiscal_year != max(fiscal_year), psnu %ni% psnu_order) %>%
    group_by(psnu) %>%
    summarise(value = value[fiscal_year == max(fiscal_year)]) %>%
    ungroup() %>%
    arrange(desc(value)) %>%
    pull(psnu)

  psnu_order <- c(psnu_order, psnu_order_previous)

  # Cleaned viz data
  df_viz <- df_ovc_trans %>%
    mutate(psnu = factor(psnu, levels = rev(psnu_order), ordered = T),
           funding_agency = factor(funding_agency,
                                   levels = rev(agencies), ordered = T))

  # Text / label details
  curr_ovc <- df_viz %>%
    filter(fiscal_year == max(fiscal_year)) %>%
    summarise(value = sum(value), .by = funding_agency) %>%
    mutate(share = value / sum(value))

  # OVC Trend - fy x psnu

  df_viz %>%
    ggplot(aes(fiscal_year + mech_position, psnu, group = funding_agency)) +
    geom_point(aes(fill = funding_agency, size = value),
               shape = 21, color = trolley_grey_light,
               show.legend = F) +
    geom_point(aes(shape = value_direction), size = 2, color = "white") +
    geom_text(data = df_viz %>% filter(fiscal_year == max(fiscal_year)),
              aes(label = comma(value)),
              hjust = -.3) +
    geom_text(data = df_viz %>% filter(fiscal_year == min(fiscal_year)),
              aes(label = comma(value)),
              hjust = 1.3) +
    scale_size_continuous(range = c(5, 10)) +
    scale_shape_identity() +
    scale_fill_manual(values = c("CDC" = scooter_light, "USAID" = denim,
                                 "DOD" = genoa_light, "OTHER" = trolley_grey)) +
    scale_x_continuous(breaks = hist_pd) +
    labs(x = "", y = "",
         title = str_to_upper("Nigeria - OVC Programs Coverage and Transition over the years"),
         subtitle = glue("As of **{curr_fy}**, Nigeria is providing **OVC Services to {comma(sum(curr_ovc$value))} children**, \\
                         with <span style='color:{scooter_light}'>**CDC**</span> {percent(curr_ovc$share[curr_ovc$funding_agency == 'CDC'], .1)}, \\
                         <span style='color:{genoa_light}'>**DOD**</span> {percent(curr_ovc$share[curr_ovc$funding_agency == 'DOD'], .1)}, and \\
                         <span style='color:{denim}'>**USAID**</span> {percent(curr_ovc$share[curr_ovc$funding_agency == 'USAID'], .1)}")) +
    si_style() +
    theme(plot.subtitle = element_markdown())

  # OVC Trend - fy x psnu x IM

  df_viz %>%
    filter(psnu == "Benue") %>%
    mutate(mech_code = paste0(funding_agency, " [", mech_code, "]")) %>%
    ggplot(aes(fiscal_year, mech_code, group = funding_agency)) +
    geom_point(aes(fill = funding_agency, size = value),
               shape = 21, color = trolley_grey_light,
               show.legend = F) +
    geom_point(aes(shape = value_direction), size = 2, color = "white") +
    geom_text(data = df_viz %>%
                mutate(mech_code = paste0(funding_agency, " [", mech_code, "]")) %>%
                filter(fiscal_year == max(fiscal_year),
                       psnu == "Benue"),
              aes(label = comma(value)),
              hjust = -.3) +
    geom_text(data = df_viz %>%
                mutate(mech_code = paste0(funding_agency, " [", mech_code, "]")) %>%
                filter(fiscal_year == min(fiscal_year),
                       psnu == "Benue"),
              aes(label = comma(value)),
              hjust = 1.3) +
    scale_size_continuous(range = c(5, 10)) +
    scale_shape_identity() +
    scale_fill_manual(values = c("CDC" = scooter_light, "USAID" = denim,
                                 "DOD" = genoa_light, "OTHER" = trolley_grey)) +
    scale_x_continuous(breaks = hist_pd) +
    labs(x = "", y = "",
         title = str_to_upper("Nigeria - OVC Programs Coverage and Transition over the years"),
         subtitle = glue("As of **{curr_fy}**, Nigeria is providing **OVC Services to {comma(sum(curr_ovc$value))} children**, \\
                         with <span style='color:{scooter_light}'>**CDC**</span> {percent(curr_ovc$share[curr_ovc$funding_agency == 'CDC'], .1)}, \\
                         <span style='color:{genoa_light}'>**DOD**</span> {percent(curr_ovc$share[curr_ovc$funding_agency == 'DOD'], .1)}, and \\
                         <span style='color:{denim}'>**USAID**</span> {percent(curr_ovc$share[curr_ovc$funding_agency == 'USAID'], .1)}")) +
    si_style() +
    theme(plot.subtitle = element_markdown())


# OUTPUTS =====

