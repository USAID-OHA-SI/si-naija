# PROJECT: si-naija
# PURPOSE: OVC DQA for IP Transition
# AUTHOR: Baboyma Kagniniwa | USAID - SI
# LICENSE: MIT
# REF. ID: 4f2b0b84
# CREATED: 2024-07-17
# UPDATED: 2024-07-17
# NOTES: Use this sample size for

# Libraries ====

  library(tidyverse)
  library(gagglr)
  library(grabr)
  library(sf)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(openxlsx)

  source("./Scripts/N00_lqas_sampling.R")


# Set paths  ====

  data   <- "Data"
  dataout <- "Dataout"
  images  <- "Images"
  graphs  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_datim <- glamr::si_path("path_datim")

# Params

  ref_id <- "4f2b0b84"
  cntry <- "Nigeria"
  agency <- "USAID"

  set.seed(234)

  file_sites <- dir_mer %>%
    return_latest("Genie-SiteByIMs-Nigeria", recursive = T)

  file_output <- file.path(dataout, "Nigeria - OVC_SERV DQA Sampling.xlsx")

  meta <- file_sites %>% get_metadata()

# Functions  ====


# LOAD DATA ====

  df_msd <- file_sites %>% read_psd()

  df_msd %>% glimpse()

  df_msd %>% distinct(fiscal_year)

  df_msd %>%
    filter(fiscal_year == meta$curr_fy,
           str_detect(indicator, "OVC")) %>%
    distinct(indicator, standardizeddisaggregate) %>%
    arrange(indicator)

# MUNGE ====

  # Extract only USAID OVC_SERV indicator data
  df_ovc <- df_msd %>%
    filter(fiscal_year == meta$curr_fy,
           funding_agency == agency,
           indicator == "OVC_SERV",
           standardizeddisaggregate == "Total Numerator",
           # standardizeddisaggregate %in% c("Age/Sex/ProgramStatus",
           #                                 "Age/Sex/Preventive",
           #                                 "Age/Sex/ProgramStatusCaregiver"),
           !is.na(cumulative)) %>%
    mutate(mech_name = extract_text(mech_name))

  s_cols <- c("pop", "sample",
              "decision_rule",
              "actual_alpha_error",
              "actual_beta_error")

  # Summary & Sampling of USAID Sites
  df_ovc_usaid_sites <- df_ovc %>%
    summarise(
      ovc_serv = sum(cumulative, na.rm = T),
      sites = n_distinct(orgunituid),
      .by = c(funding_agency)
    ) %>%
    rowwise() %>%
    mutate(
      lqas = hyperPlan(N = sites) %>% unlist(use.names = F) %>% paste0(collapse = ",")
    ) %>%
    ungroup() %>%
    separate_wider_delim(lqas, delim = ",", names = s_cols) %>%
    mutate(across(all_of(s_cols), ~as.integer(.x)))

  # Summary & Sampling of IM/IP Sites
  df_ovc_im_sites <- df_ovc %>%
    summarise(
      ovc_serv = sum(cumulative, na.rm = T),
      sites = n_distinct(orgunituid),
      .by = c(mech_code, mech_name)
    ) %>%
    arrange(mech_name, desc(ovc_serv)) %>%
    rowwise() %>%
    mutate(
      lqas = hyperPlan(N = sites) %>% unlist(use.names = F) %>% paste0(collapse = ",")
    ) %>%
    ungroup() %>%
    separate_wider_delim(lqas, delim = ",", names = s_cols) %>%
    mutate(across(all_of(s_cols), ~as.integer(.x)))

  # Summary & Sampling of states Sites
  df_ovc_states_sites <- df_ovc %>%
    summarise(
      ovc_serv = sum(cumulative, na.rm = T),
      sites = n_distinct(orgunituid),
      .by = c(mech_code, mech_name, psnu)
    ) %>%
    arrange(mech_name, desc(ovc_serv)) %>%
    rowwise() %>%
    mutate(
      lqas = hyperPlan(N = sites) %>% unlist(use.names = F) %>% paste0(collapse = ",")
    ) %>%
    ungroup() %>%
    separate_wider_delim(lqas, delim = ",", names = s_cols) %>%
    mutate(across(all_of(s_cols),
                  ~case_when(str_detect(.x, "NO.*") ~ NA_character_,
                             TRUE ~ .x))) %>%
    mutate(across(all_of(s_cols), ~as.integer(.x))) %>%
    mutate(
      sample = case_when(
        is.na(sample) ~ sites,
        TRUE ~ sample
      )
    )

  # Summary & Sampling of clients from sites
  df_ovc_site_clients <- df_ovc %>%
    filter(funding_agency == agency) %>%
    summarise(
      ovc_serv = sum(cumulative, na.rm = T),
      .by = c(mech_code, mech_name, psnu, community)
    ) %>%
    mutate(
      security = case_when(
        psnu %in% c("Zamfara") ~ "Yes",
        TRUE ~ "No"
      ),
      volume = case_when(
        security == "No" & ovc_serv > 19 ~ "Yes",
        TRUE ~ "No"
      ),
      qualify = case_when(
        security == "No" & volume == "Yes" ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    arrange(mech_name, psnu, desc(ovc_serv))

  ## Stratification

  s_grps_lbls1 <- 1:4 %>% paste0("Q", .)
  s_grps_lbls2 <- c("<25", "25-50", "50-75", "75+")

  df_ovc_site_clients <- df_ovc_site_clients %>%
    filter(qualify == "Yes") %>%
    group_by(mech_code, mech_name) %>%
    mutate(
      im_sites_group = cut(
        ovc_serv,
        breaks = get_quantiles(ovc_serv)$value,
        labels = s_grps_lbls2,
        include.lowest = T
      )
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      lqas = hyperPlan(N = ovc_serv) %>% unlist(use.names = F) %>% paste0(collapse = ",")
    ) %>%
    ungroup() %>%
    separate_wider_delim(lqas, delim = ",", names = s_cols) %>%
    mutate(across(all_of(s_cols),
                  ~case_when(str_detect(.x, "NO.*") ~ NA_character_,
                             TRUE ~ .x))) %>%
    mutate(across(all_of(s_cols), ~as.integer(.x)))


  df_ovc_site_clients %>% distinct(im_sites_group)
  df_ovc_site_clients %>% filter(is.na(im_sites_group))

  df_ovc_im_sites_sample <- df_ovc_im_sites %>%
    distinct(mech_name) %>%
    arrange(mech_name) %>%
    pull() %>%
    map(function(.mech){

      cli::cli_alert_info("Sites sampling for {(.mech)} ...")

      df_ovc_site_clients %>%
        filter(mech_name == .mech) %>%
        summarise(
          im_grp_clients = sum(ovc_serv, na.rm = T),
          im_grp_sites = n(),
          .by = c(mech_code, mech_name, im_sites_group)
        ) %>%
        left_join(
          x = .,
          y = df_ovc_im_sites %>%
            select(mech_name, im_sites = sites, im_sites_sample_size = sample),
          by = "mech_name"
        ) %>%
        mutate(
          im_grp_sites_sample_size = distribute_sample(
            s = first(im_sites_sample_size),
            wts = im_grp_sites
          ),
          .by = c(mech_code, mech_name)
        ) %>%
        rowwise() %>%
        mutate(
          site_index = sample(
              x = im_grp_sites,
              size = im_grp_sites_sample_size,
              replace = FALSE
            ) %>%
            sort() %>%
            paste0(collapse = ",")
        ) %>%
        ungroup()

    }) %>%
    bind_rows() %>%
    relocate(im_sites, im_sites_sample_size, .after = mech_name)

  df_ovc_im_sites_sample_list <- df_ovc_im_sites_sample %>%
    distinct(mech_code, mech_name, im_sites_group, site_index) %>%
    separate_rows(site_index, sep = ",") %>%
    mutate(site_index = as.integer(site_index),
           site_dqa = 1)

  df_ovc_sites_dqa <- df_ovc_site_clients %>%
    select(-pop) %>%
    arrange(mech_code, mech_name, desc(im_sites_group), desc(ovc_serv)) %>%
    group_by(mech_code, mech_name, im_sites_group) %>%
    mutate(site_index = row_number()) %>%
    ungroup() %>%
    left_join(
      df_ovc_im_sites_sample_list,
      by = c("mech_code", "mech_name", "im_sites_group", "site_index")
    ) %>%
    mutate(
      site_dqa = case_when(
        is.na(site_dqa) ~ 0,
        TRUE ~ site_dqa
      )
    )


# VIZ ====

  #

# EXPORT ====

  wb <- createWorkbook()

  # Agency based sample
  #addWorksheet(wb, sheetName = "USAID")
  #writeDataTable(wb, sheet = "USAID", x = df_ovc_usaid_sites)

  # Partner based sample
  sht_im <- "PARTNERS"
  addWorksheet(wb, sheetName = sht_im)
  writeDataTable(wb, sheet = sht_im, x = df_ovc_im_sites)

  sht_im_sites <- "IM SITES SAMPLE"
  addWorksheet(wb, sheetName = sht_im_sites)
  writeDataTable(wb, sheet = sht_im_sites, x = df_ovc_im_sites_sample)

  sht_im_clients <- "IM CLIENTS SAMPLE BY SITE"
  addWorksheet(wb, sheetName = sht_im_clients)
  writeDataTable(wb, sheet = sht_im_clients, x = df_ovc_site_clients)

  sht_sites_dqa <- "IM DQA SITES"
  addWorksheet(wb, sheetName = sht_sites_dqa)
  writeDataTable(wb, sheet = sht_sites_dqa, x = df_ovc_sites_dqa)

  saveWorkbook(wb, file = file_output, overwrite = T)

  open_path(file_output)

  # dfs_list <- list(
  #   "USAID" = df_ovc_usaid_sites,
  #   "PARTERS" = df_ovc_im_sites,
  #   "STATES" = df_ovc_states_sites,
  #   "CLIENTS" = df_ovc_site_clients
  # )
  #
  # write.xlsx(dfs_list, file = file_output, asTable = T)

  df_ovc %>%
    write_csv(file = file.path(dataout, "Nigeria - OVC_SERV DQA Sampling.csv"))

  df_ovc_im_sites_sample %>%
    write_csv(file = file.path(dataout, "Nigeria - OVC_SERV DQA Sampling sites.csv"))

  df_ovc_site_clients %>%
    write_csv(file = file.path(dataout, "Nigeria - OVC_SERV DQA Sampling clients.csv"))

  sht_sites_dqa %>%
    write_csv(file = file.path(dataout, "Nigeria - OVC_SERV DQA Sampling clients.csv"))
