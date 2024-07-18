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
      lqas = hyperPlan(N = sites) %>% unlist(use.names = F) %>% paste0(collapse = ", ")
    ) %>%
    ungroup() %>%
    separate_wider_delim(lqas, delim = ", ", names = s_cols)

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
      lqas = hyperPlan(N = sites) %>% unlist(use.names = F) %>% paste0(collapse = ", ")
    ) %>%
    ungroup() %>%
    separate_wider_delim(lqas, delim = ", ", names = s_cols)

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
      lqas = hyperPlan(N = sites) %>% unlist(use.names = F) %>% paste0(collapse = ", ")
    ) %>%
    ungroup() %>%
    separate_wider_delim(lqas, delim = ", ", names = s_cols) %>%
    mutate(across(all_of(s_cols),
                  ~case_when(str_detect(.x, "NO.*") ~ NA_character_,
                             TRUE ~ .x))) %>%
    mutate(across(all_of(s_cols), as.integer)) %>%
    mutate(
      sample = case_when(
        is.na(sample) ~ sites,
        TRUE ~ sample
      )
    )


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

  df_ovc_site_clients %>%
    filter(qualify == "Yes") %>%
    pull(ovc_serv) %>%
    quantile()

# VIZ ====

  #

# EXPORT ====
  wb <- createWorkbook()

  addWorksheet(wb, sheetName = "USAID")
  writeDataTable(wb, sheet = "USAID", x = df_ovc_usaid_sites)

  addWorksheet(wb, sheetName = "PARTERS")
  writeDataTable(wb, sheet = "PARTERS", x = df_ovc_im_sites)

  addWorksheet(wb, sheetName = "STATES")
  writeDataTable(wb, sheet = "STATES", x = df_ovc_states_sites)

  addWorksheet(wb, sheetName = "CLIENTS")
  writeDataTable(wb, sheet = "CLIENTS", x = df_ovc_site_clients)

  saveWorkbook(wb, file = file_output, overwrite = T)

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

  df_ovc_sites %>%
    write_csv(file = file.path(dataout, "Nigeria - OVC_SERV DQA Sampling sites.csv"))

  df_ovc_site_clients %>%
    write_csv(file = file.path(dataout, "Nigeria - OVC_SERV DQA Sampling clients.csv"))
