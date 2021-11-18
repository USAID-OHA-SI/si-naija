##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: MDB Tables
##  LICENCE: MIT
##  DATE:    2021-11-09

# DEPENDENCIES ----

  library(tidyverse)
  library(gophr)
  library(glitr)
  library(glamr)
  library(gt)
  library(selfdestructin5)
  library(fontawesome)

# SETUP ----

  # Directories
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"

  dir_merdata <- si_path("path_msd")

  dir_results <- "../../PEPFAR/COUNTRIES/Nigeria/FY21 - APR/data"

  cntry <- "Nigeria"

# FILES ----

  # OU x IM Data
  file_ou_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_OU_IM_FY19-22_.*.zip")

  file_ou_results <- dir_results %>%
    return_latest(pattern = "^FY21 - Key Indicators Results Data - \\d{8}.xls")

# FUNCTIONS ----

  calc_growth <- function(x, y) {
    ifelse(x > 0.000, (x / y) - 1, NA_real_)
  }

# LOAD DATA ----

  # OU x IM
  df_ou <- file_ou_im %>% read_msd()

  #df_ou <- df_ou %>% filter(countryname == cntry)

  curr_fy <- df_ou %>% identifypd(pd_type = "year")
  curr_pd <- df_ou %>% create_pd()
  #curr_pd <- "FY21Q4"

  msd_source <- curr_pd %>% msd_period(period = .)

  # FY21 Results
  df_rst <- file_ou_results %>%
    read_excel(sheet = 1, skip = 1)

  df_rst %>% glimpse()

  names(df_rst) <- c("fundingagency", "psnu", "indicator",
                     "qtr1", "qtr2", "qtr3", "qtr4")

# MUNGING ----

  # MDBs Indicators
  inds_main <- df_ou %>% fetch_indicators()

  inds_tx <- fetch_indicators(df = df_ou, tab = "treatment")

  inds_all <- inds_main %>% bind_rows(inds_tx)


  # Other data
  df_mdb <- df_ou %>%
    filter(countryname == cntry) %>%
    make_mdb_df()

  #tbl_mdb <- reshape_mdb_df(df = df_mdb, pd = curr_pd)

  df_tx_mdb <- df_ou %>%
    make_mdb_tx_df(df = .) %>%
    filter(operatingunit == cntry)

  #tbl_tx_mdb <- reshape_mdb_tx_df(df = df_tx_mdb, pd = curr_pd)

  # Add APR / Q4 Data ----

  # Clean Results
  df_rst <- df_rst %>%
    clean_agency() %>%
    mutate(
      indicator = str_remove(indicator, "FY21 Results Analytic Indicators: "),
      indicator = case_when(
        str_detect(indicator, "HTS_TST Yield \\(HTS_TST") ~ "HTS_TST_YIELD",
        str_detect(indicator, "HTS_TST Total") ~ "HTS_TST",
        str_detect(indicator, "HTS_TST_POS") ~ "HTS_TST_POS",
        str_detect(indicator, "TX_NEW Total") ~ "TX_NEW",
        str_detect(indicator, "TX_CURR Total") ~ "TX_CURR",
        str_detect(indicator, "TX_CURR 3-5 months") ~ "TX_MMD35",
        str_detect(indicator, "TX_CURR 6\\+ months") ~ "TX_MMD6+",
        str_detect(indicator, "TX_CURR <3 months") ~ "TX_MMDlt3",
        str_detect(indicator, "TX_PVLS Total") ~ "TX_PVLS",
        str_detect(indicator, "TX_PVLS Denominator") ~ "TX_PVLS_D",
        str_detect(indicator, "PrEP_NEW Total") ~ "PrEP_NEW",
        str_detect(indicator, "PrEP_CURR Total") ~ "PrEP_CURR",
        TRUE ~ indicator
      ),
      fiscal_year = 2021
    ) %>%
    rename(agency = fundingagency) %>%
    relocate(fiscal_year, .before = 1)

  df_ou_rst <- df_rst %>%
    group_by(fiscal_year, agency, indicator) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    rowwise() %>%
    mutate(
      cumulative = case_when(
        str_detect(indicator, "TX_MMD.*") | indicator %in% snapshot_ind ~ qtr4,
        TRUE ~ sum(qtr1, qtr2, qtr3, qtr4, na.rm = TRUE)
      ),
      agg_type = "OU"
    ) %>%
    ungroup()

  df_ou_rst %>% glimpse()
  df_ou_rst %>% distinct(indicator)

  # Calculate MMD3+
  df_ou_rst <- df_ou_rst %>%
    filter(indicator %in% c("TX_MMD35", "TX_MMD6+")) %>%
    group_by(fiscal_year, agency, agg_type) %>%
    summarise(across(c(starts_with("qtr"), cumulative), sum, na.rm = T), .groups = "drop") %>%
    mutate(indicator = "TX_MMD3+") %>%
    relocate(indicator, .after = agency) %>%
    relocate(agg_type, .after = last_col()) %>%
    bind_rows(df_ou_rst, .) %>%
    filter(indicator != "HTS_TST_YIELD")

  # Group all non-usaid agencies
  df_ou_rst <- df_ou_rst %>%
    mutate(
      agency = case_when(
        agency != "USAID" ~ "ALL OTHER AGENCIES",
        TRUE ~ agency
      )
    )

  df_ou_rst <- df_ou_rst %>%
    filter(agency != "USAID") %>%
    group_by(fiscal_year, agency, indicator, agg_type) %>%
    summarise(across(c(starts_with("qtr"), cumulative), sum, na.rm = T), .groups = "drop") %>%
    bind_rows(df_ou_rst %>% filter(agency == "USAID"), .)

  df_ou_rst <- df_ou_rst %>%
    mutate(operatingunit = cntry) %>%
    relocate(operatingunit, .after = indicator) %>%
    left_join(inds_all %>% select(indicator, indicator_plain) %>% distinct(),
              by = "indicator")

  # Replace Current FY Data for Key Indicators ----
  df_ou_rst %>% glimpse()

  df_mdb %>% glimpse()

  df_mdb_nga <- df_mdb %>%
    filter(operatingunit == cntry)

  df_mdb_nga_fy21 <- df_mdb_nga %>%
    filter(fiscal_year == curr_fy,) %>%
    select(agency, indicator, targets) %>%
    left_join(df_ou_rst, ., by = c("agency", "indicator")) %>%
    filter(indicator %in% unique(df_mdb_nga$indicator)) %>%
    relocate(targets, .before = qtr1)

  df_mdb_nga_new <- df_mdb_nga %>%
    filter(fiscal_year != curr_fy) %>%
    bind_rows(df_mdb_nga_fy21)

  tbl_mdb_nga_new <- reshape_mdb_df(df = df_mdb_nga_new, pd = "FY21Q4")

  # Replace Current FY Data for TX Indicators ----
  df_ou_rst %>% glimpse()

  df_tx_mdb %>% glimpse()

  df_tx_mdb %>% distinct(indicator)

  df_tx_mdb_nga <- df_tx_mdb %>%
    filter(operatingunit == cntry)

  df_tx_mdb_nga_fy21 <- df_tx_mdb_nga %>%
    filter(fiscal_year == curr_fy) %>%
    select(agency, indicator, targets) %>% #distinct(indicator)
    left_join(df_ou_rst, ., by = c("agency", "indicator")) %>%
    filter(indicator %in% unique(df_tx_mdb_nga$indicator)) %>%
    relocate(targets, .before = qtr1)

  df_tx_mdb_nga_new <- df_tx_mdb_nga %>%
    filter(fiscal_year != curr_fy) %>%
    bind_rows(df_tx_mdb_nga_fy21) %>%
    filter(fiscal_year != 2022) %>%
    select(-indicator_plain)

  tbl_tx_mdb_nga_new <- reshape_mdb_tx_df(df = df_tx_mdb_nga_new, pd = "FY21Q4")

# MDB TABLES ----

  #legend_chunk <- gt::md(glue::glue("Legend: Cumulative Indicators <img src= 'https://user-images.githubusercontent.com/5873344/131568585-cef89158-ee77-41a8-9dc7-a3d83ca0ba2c.png' style='height:15px;'>    &emsp; Snapshot (TX_CURR) <img src= '{legend_snapshot}' style='height:15px;'> "))
  legend_chunk <- gt::md(glue::glue("<img src= '{legend_snapshot}' style='height:15px;'> "))

  create_mdb(df = tbl_mdb_nga_new %>% filter(indicator != "VMMC_CIRC"),
             ou = cntry,
             type = "main",
             pd = "FY21Q4",
             msd_source = "FY21Q4d MSD",
             legend = legend_chunk) %>%
  gtsave(., path = dir_graphics, filename = "FY21Q4 - Nigeria mdb_main.png")

  create_mdb(df = tbl_tx_mdb_nga_new,
             ou = cntry,
             type = "treatment",
             pd = "FY21Q4",
             msd_source = msd_source) %>%
  gtsave(., path = dir_graphics, filename = "FY21Q4 - Nigeria mdb_treatment.png")



