##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: HRH Processor - Data Munging
##  LICENCE: MIT
##  DATE:    2022-01-17

# PACKAGES ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(grabr)
  library(glamr)
  library(janitor)

# GLOBAL

  dir_hrh <- file.path(si_path(), "../HRHDATA")

  file_site <- list.files(dir_hrh,
                          pattern = "^HRH_.*_Site_IM_FY21-22_\\d{8}",
                          full.names = TRUE)

  file_proc_pending <- file.path(dir_hrh, "HRH Processor - Pending files") %>%
    list.files(pattern = ".xlsx", full.names = TRUE)

  file_proc_accpeted <- file.path(dir_hrh, "HRH Processor - Accepted files") %>%
    list.files(pattern = ".xlsx", full.names = TRUE)

  curr_fy <- 2022

  cntry <- "Nigeria"

  agency <- "USAID"

# DATA ----

  # MSD HRH
  df_hrh_msd <- read_msd(file_site)

  df_hrh_msd %>% glimpse()

  df_hrh_msd %<>%
    filter(operating_unit == cntry,
           fiscal_year == curr_fy,
           funding_agency == agency)

  df_hrh_msd %>% distinct(psnu)

  # HRH Processors - CoverSheet 1

  df_hrh_coversheets <- file_proc_pending %>%
    c(file_proc_accpeted) %>%
    #first() %>%
    map_dfr(function(.file){
      .file %>%
        read_excel(sheet = "CoverSheet",
                   range = "B2:D14",
                   col_names = F,
                   col_types = "text") %>%
        rename(variable = ...1, value = ...3) %>%
        select(-...2) %>%
        filter(!is.na(variable)) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        janitor::clean_names() %>%
        rename(
          operatingunit = operating_unit_country,
          mech_code = mechanism_id,
          mech_name = mechanism_name,
          prime_partner_poc = prime_ip_point_of_contact,
          prime_partner_poc_info = prime_ip_contact_info,
          subrecipients = count_of_subrecipients
        ) %>%
        mutate(filename = basename(.file),
               dirname = basename(dirname(.file)))
    })

  df_hrh_meta <- df_hrh_coversheets %>%
    select(-subrecipients, -starts_with(c("complet", "prime_partner_")))

  # HRH Processors - CoverSheet 2

  df_hrh_subrecipients <- df_hrh_coversheets %>%
    filter(subrecipients > 0) %>%
    select(filename, dirname) %>%
    pmap_dfr(function(filename, dirname){
      file.path(dir_hrh, dirname, filename) %>%
        read_excel(sheet = "CoverSheet", skip = 15) %>%
        select(1:3) %>%
        janitor::clean_names() %>%
        rename(sub_partner_code = x1,
               sub_partner_uei = uei) %>%
        filter(!is.na(sub_partner_code)) %>%
        mutate(filename = filename,
               dirname = dirname)
    })

  # HRH Processors - Staff List

  df_hrh_stafflist <- file_proc_pending %>%
    c(file_proc_accpeted) %>%
    #first() %>%
    map_dfr(function(.file){
      .file %>%
        read_excel(sheet = "StaffList") %>%
        janitor::clean_names() %>%
        select(-record_number_optional, -matches(c("level\\dcheck", "snu\\d"))) %>%
        rename(
          prime_or_sub = employed_through_prime_or_sub_ip,
          subrecipient_name = if_sub_select_ip_name,
          mode_of_hiring = mode_of_hire,
          moh_secondment = moh_staff_or_seconded_to_moh,
          months_of_work = months_of_work_in_past_year,
          avg_fte_per_month = average_fte_per_month,
          is_community_primarily = primarily_support_work_in_the_community,
          roving = work_in_or_support_multiple_facility_sites_roving_staff,
          is_tech_assist = provide_technical_assistance,
          is_outside_ou = position_based_outside_of_ou,
          program = primary_program_area,
          beneficiary = primary_beneficiary,
          interaction_type = deliver_services_directly_to_beneficiaries,
          is_covid_support = in_past_year_provided_support_for_the_covid_response,
          exp_pepfar = sum_of_annual_pepfar_expenditure_excluding_fringe_and_non_monetary,
          exp_fringe = annual_pepfar_fringe_expenditure_excluding_non_monetary,
          exp_non_monetary = annual_pepfar_non_monetary_expenditure_excluding_fringe
        ) %>%
        filter(!is.na(prime_or_sub)) %>%
        mutate(fiscal_year = 2022,
               filename = basename(.file),
               dirname = basename(dirname(.file))) %>%
        separate(employment_title,
                 into = c("er_category", "employment_title"),
                 sep = ": ") %>%
        separate(program,
                 into = c("site_level", "sub_program"),
                 sep = ": ") %>%
        mutate(program = site_level,
               sub_program = case_when(
                 is.na(sub_program) & program == "Program Management" ~ "IM Program Management",
                 TRUE ~ sub_program
               )) %>%
        relocate(program, .after = site_level) %>%
        left_join(df_hrh_meta, by = c("filename", "dirname")) %>%
        left_join(select(df_hrh_subrecipients, -sub_partner_code),
                  by = c("filename", "dirname", "subrecipient_name" = "sub_partner_name"))
    })

  df_hrh_stafflist %>% glimpse()
  df_hrh_stafflist %>% names()

  df_hrh_stafflist_clean <- df_hrh_stafflist %>%
    select(-filename, -dirname, -starts_with("x")) %>%
    mutate(sitename = facility) %>%
    select(fiscal_year, operatingunit, country = operatingunit,
           sitename, facility, community, psnu,
           funding_agency, mech_code, mech_name,
           employment_title, er_category, site_level, program, sub_program,
           interaction_type, beneficiary, gender, prime_or_sub,
           subrecipient_name, subrecipient_uei = sub_partner_uei,
           mode_of_hiring, roving, everything()) %>%
    rowwise() %>%
    mutate(sitename = first(na.omit(c(facility, community, psnu))),
           sitename = case_when(
             is.na(sitename) ~ "Data reported above Site level",
             TRUE ~ sitename
           ),
           facility = case_when(
             is.na(facility) ~ "Data reported above Facility level",
             TRUE ~ facility
           ),
           community = case_when(
             is.na(community) ~ "Data reported above Community level",
             TRUE ~ community
           ),
           psnu = case_when(
             is.na(psnu) ~ "Data reported above PSNU level",
             TRUE ~ psnu
           ),
           individual_count = 1) %>%
    ungroup()

  df_hrh_stafflist_clean %>%
    write_csv(file = "./Dataout/FY22_Nigeria_HRH_Collated_Datasets.csv",
              na = "")


