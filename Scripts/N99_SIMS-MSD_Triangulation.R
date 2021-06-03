##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B. Kagniniwa, A. Devlin | USAID
##  PURPOSE: SIMS & MSD Triangulation
##  LICENCE: MIT
##  DATE:    2021-05-25
##  UPDATED: 2021-05-26

# DEPENDENCIES ----

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glitr)
  library(glamr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(tidytext)
  library(glue)
  library(here)
  library(ICPIutilities)
  library(gt)

# SETUP ----

  # Directories
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"

  dir_merdata <- si_path("path_msd")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

  # MER Data - get the latest MSD Sites x IM file
  file_site_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_Site_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

  file_sims_site_im <- dir_merdata %>%
    return_latest(pattern = "^SIMS_.*_Site_IM_.*_\\d{8}_v\\d{1}_1_N.*.xlsx$",
                  recursive = TRUE)

# GLOBAL VARS

  # Indicators
  # indicators <- c('TX_CURR', 'TX_NEW', 'HTS_TST',
  #                 'HTS_INDEX', 'TX_ML', 'TX_PVLS')
  indicators <- c('HTS_TST', 'PMTCT_STAT', 'TX_NEW')

# IMPORT DATA

  # MSD - Site * IM
  df_sites <- file_site_im %>% read_msd()

  df_sites_sims <- df_sites %>%
    filter(indicator %in% indicators) %>%
    select(!c(facilityprioritization, communityprioritization,
              prime_partner_duns, award_number, pre_rgnlztn_hq_mech_code,
              categoryoptioncomboname, source_name, statuscx,
              statushiv, statustb, hiv_treatment_status,
              snuprioritization)) %>%
    mutate(sitename = str_to_title(sitename),
           facility = str_to_title(facility),
           primepartner = str_to_title(primepartner),
           mech_name = str_to_title(mech_name)) %>%
    filter(
      fundingagency == "USAID",
      indicator %in% indicators,
      standardizeddisaggregate %in% c('Total Numerator', 'Total Denominator')
    ) %>%
    pivot_longer(cols = qtr1:cumulative,
                 names_to = 'quarter',
                 values_to = 'results') %>%
    remove_empty(which = 'cols', quiet = FALSE) %>%
    arrange(fiscal_year, psnu, community, sitename, indicator)

  df_sites_sims %>% glimpse()

  df_sites_sims %>%
    head(1000) %>%
    view()

  #Export individual datasets
  df_sites_sims %>%
    write_csv(file.path(dir_dataout, 'Nigeria_MSD_Site_IM_FY19_21.csv'))



  # %>%
  #   reshape_msd()

  # SIMS Sheets
  file_sims_site_im %>% excel_sheets()

  # [1] "CEE_Score_pivot"
  # [2] "CEE_Question_pivot"
  # [3] "mech"
  # [4] "org"
  # [5] "sims_dataelements"
  # [6] "asmt_coversheet"
  # [7] "cee_results"

  ## Below uses read_excel
  sims_results <- file_sims_site_im %>%
    read_excel(sheet = 'cee_results') %>%
    clean_names() %>%
    select(!c(organisation_hst_srgt, mechanism_srgt, period_srgt_key)) %>%
    mutate(cee_score_value = as.numeric(cee_score_value),
           quarter = str_c('qtr', fiscal_quarter)) %>%
    filter(!is.na(cee_score_value)) %>%
    remove_empty(which = 'cols', quiet = FALSE)


  sims_cee_elements <- file_sims_site_im %>%
    read_excel(sheet = 'sims_dataelements') %>%
    clean_names() %>%
    select(c(31:80), sims_univ_data_element_srgt, sims_set_id, sims_set_name,
           sims_tool_type_name, cee_id,
           cee_name, cee_activity_type, cee_required, cee_question_label) %>%
    pivot_longer(cols = c(1:50), names_to = 'indicator', values_to = 'present') %>%
    mutate(indicator = str_to_upper(indicator)) %>%
    filter(indicator %in% indicators) %>%
    remove_empty(which = 'cols', quiet = FALSE)


  sims_mech <- file_sims_site_im %>%
    read_excel(sheet = 'mech') %>%
    clean_names() %>%
    select(-c(mechanism_id, pre_rgnlztn_hq_mech_code, prime_partner_duns, award_number)) %>%
    mutate(mech_name = str_to_title(mech_name),
           prime_partner = str_to_title(prime_partner),
           fiscal_year = str_remove(planning_reporting_cycle, " COP$"),
           fiscal_year = as.integer(fiscal_year) + 1) %>%
    rename(primepartner = prime_partner,
           fundingagency = funding_agency) %>%
    relocate(c(fiscal_year, planning_reporting_cycle), .after = 1)
    remove_empty(which = 'cols', quiet = FALSE)


  sims_org <- file_sims_site_im %>%
    read_excel(sheet = 'org') %>%
    clean_names() %>%
    select(!c(region_uid, region, facility_prioritization,
              community_prioritization, sn_uprioritization)) %>%
    mutate(site_type_cleaned = ifelse(site_type == 'Facility', 'Facility',
                                      ifelse(site_type == 'Community', 'Community', 'Other'))) %>%
    mutate(facility = str_to_title(facility)) %>%
    rename_at(vars(operating_unit_uid, operating_unit, country_name, site_type,
                org_unituid, psn_uuid, community_uid, facility_uid,
                type_military), str_remove_all, pattern = "_") %>%
    remove_empty(which = 'cols', quiet = FALSE)


  sims_asmt <- file_sims_site_im %>%
    read_excel(sheet = 'asmt_coversheet') %>%
    clean_names() %>%
    select(c(sims4_cover_sheet_srgt, asmt_id, asmt_date,
             asmt_type, asmt_tool_type, asmt_prtd_set)) %>%
    remove_empty(which = 'cols', quiet = FALSE)


  ##Combining SIMS sheets together
  df_sims_results <- sims_results %>%
    inner_join(sims_mech %>% select(-fiscal_year),
               by = 'mechanism_hst_srgt') %>%
    inner_join(sims_cee_elements,
               by = 'sims_univ_data_element_srgt') %>%
    inner_join(sims_org,
               by = 'organisation_srgt') %>%
    inner_join(sims_asmt,
               by = 'sims4_cover_sheet_srgt') %>%
    arrange(fiscal_year, psnu, community, facility, indicator)


  df_sims_results %>% glimpse()

  df_sims_results %>% head(1000) %>% view()


  # Export
  df_sims_results %>%
    write_csv(file.path(dir_dataout, 'Nigeria_SIMS_Site_IM_FY19_21.csv'))

  # Merge SIMS Results to MDS
  df_sims <- df_sites_sims %>%
    inner_join(df_sims_results,
               by = c("orgunituid", "operatingunit", "operatingunituid",
                      "snu1", "snu1uid", "psnu", "psnuuid", "typemilitary",
                      "dreams", "primepartner", "fundingagency", "mech_code",
                      "mech_name", "communityuid", "community", "facilityuid",
                      "facility", "sitetype", "indicator",
                      "fiscal_year", "quarter"),
               copy = TRUE)



  df_sims %>% glimpse()
  df_sims %>% head(1000) %>% view()

  # Export
  df_sims %>%
    write_csv(file.path(dir_dataout, 'Nigeria_SIMS_MSD_Site_IM_FY19_21.csv'))


