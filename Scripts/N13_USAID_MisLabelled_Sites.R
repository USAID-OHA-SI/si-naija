##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: Identify Miss-labeled sites
##  LICENCE: MIT
##  DATE:    2022-01-24

# PACKAGES ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(grabr)
  library(glamr)
  library(janitor)

# Directores & Files

  dir_mer <- si_path()

  file_site_im <- dir_mer %>%
    return_latest("Site_IM_FY20-23.*_Nigeria")

# Params

  cntry <- "Nigeria"
  agency <- "USAID"

# RAW DATA

  df_sites <- file_site_im %>% read_msd()

# MUNGING

  df_flag <- df_sites %>%
    filter(funding_agency == agency,
           str_detect(psnu, "_Mil"))

  flag_uids <- df_flag %>%
    distinct(orgunituid, sitename,
             sitetype, typemilitary,
             communityuid, community,
             psnuuid, psnu) %>%
    pull(orgunituid)

  flag_uids

  # Mechs reporting on USAID/Mil sites

  df_flag %>%
    filter(orgunituid %in% flag_uids) %>%
    select(fiscal_year, orgunituid, sitetype,
           mech_code, mech_name,
           prime_partner_name) %>%
    distinct() %>%
    arrange(fiscal_year, mech_code)

  # Orgs

  #https://final.datim.org/api/organisationUnits?filter=level:eq:4&filter=path:like:PqlFzhuPcF1
  #https://final.datim.org/api/organisationUnits?filter=level:eq:5&filter=path:like:PqlFzhuPcF1&paging=false&includeAncestors=true&format=json

  df_nga_orgs <- get_ouuid(cntry) %>%
    get_ouorgs(level = 4)

  df_nga_orgs <- get_outable() %>%
    filter(country == cntry) %>%
    pivot_longer(cols = ends_with("lvl"),
                 names_to = "label",
                 values_to = "level") %>%
    select(country_uid, label, level) %>%
    arrange(desc(level)) %>%
    pmap_dfr(function(country_uid, level, label){
      get_ouorgs(ouuid = country_uid, level = level) %>%
        mutate(level = level, label = str_remove(label, "_lvl"))
    })

  df_nga_orgs %>%
    filter(uid %in% flag_uids)

  # orgs
  df_orgs <- df_sites %>%
    distinct(orgunituid, sitename,
             sitetype, typemilitary,
             communityuid, community,
             psnuuid, psnu)

  df_orgs %>%
    filter(typemilitary == 'Y') %>%
    count(psnu)

  df_orgs %>%
    filter(sitetype == 'Military') %>%
    count(psnu)

  df_orgs %>%
    count(psnu, sitetype) %>%
    prinf()

  df_orgs %>%
    count(psnu, sitetype) %>%
    filter(sitetype == "Above Site", n != 1)


