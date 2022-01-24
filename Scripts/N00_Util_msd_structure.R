##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Utility functions
##  LICENCE: MIT
##  DATE:    2021-10-08

## Libraries ----

  library(tidyverse)
  library(gophr)
  library(glamr)

## GLOBALS ----

  dir_merdata <- glamr::si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"

  cntry <- "Nigeria"

# FUNCITONS ----

  #' @title Datim Resources
  #'
  datim_resources <- function(...,
                              base_url = NULL,
                              username = NULL,
                              password = NULL,
                              res_name = NULL,
                              dataset = FALSE) {
    # datim credentials
    if (missing(username))
      username <- datim_user()

    if (missing(password))
      password <- datim_pwd()

    # Base url
    if (missing(base_url))
      base_url <- "https://final.datim.org"

    # URL Query Options
    options <- "?format=json&paging=false"

    # List of columns
    cols <- list(...)

    print(paste0(cols))

    # API URL
    api_url <- base_url %>%
      paste0("/api/resources", options)

    # Query data
    data <- api_url %>%
      datim_execute_query(username, password, flatten = TRUE) %>%
      purrr::pluck("resources") %>%
      tibble::as_tibble() %>%
      rename(name = displayName)

    # Filter if needed
    if (!base::is.null(res_name)) {
      data <- data %>%
        filter(name == res_name)
    }

    # Return only the url when results is just 1 row
    if(base::nrow(data) == 1 && dataset == FALSE) {
      return(data$href)

    } else if (base::nrow(data) == 1 && dataset == TRUE) {

      dta_url <- data$href
      print(dta_url)

      end_point <- dta_url %>%
        str_split("\\/") %>%
        unlist() %>%
        last()

      #print(end_point)

      dta_url <- dta_url %>% paste0(options)

      print(length(cols))

      if (length(cols) > 0) {
        dta_url <- dta_url %>%
          paste0("&fields=", paste0(cols, collapse = ","))
      } else {
        dta_url <- dta_url %>%
          paste0("&fields=:nameable")
      }

      print(dta_url)

      data <- dta_url %>%
        datim_execute_query(username, password, flatten = TRUE) %>%
        purrr::pluck(end_point) %>%
        tibble::as_tibble()
    }

    return(data)
  }

  # datim_resources()
  #
  # datim_resources(res_name = "Data Elements")
  #
  # datim_resources("id", "code", "name", "code", "shortName",
  #                 "shortDisplayName", "description",
  #                 "categoryoptioncombocode",
  #                 res_name = "Data Elements", dataset = T)
  #
  # datim_resources(res_name = "Data Elements", dataset = TRUE)
  #
  # datim_resources(res_name = "Data Sets", dataset = TRUE)
  #
  # datim_resources(res_name = "Category Option Combos", dataset = TRUE)
  #
  # datim_resources(res_name = "dataValueSets", dataset = TRUE)

  # MER Data Sets - Results
  mer_results <- c(
    "MER Results: Facility Based",
    "MER Results: Community Based",
    "MER Results: Medical Store",
    "MER Results: Community Based - DoD ONLY",
    "MER Results: Facility Based - DoD ONLY",
    "Host Country Results: Facility (USG)"
  )

  # MER Data Sets - Targets
  mer_targets <- c(
    "Host Country Targets: COP Prioritization SNU (USG)",
    "MER Target Setting: PSNU (Facility and Community Combined)",
    "MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY"
  )

  datim_resources(res_name = "Data Sets",
                  dataset = TRUE) %>%
    filter(name %in% mer_results) %>%
    pull(href) %>%
    #pull(id) %>%
    map_dfr(function(.x){
      data <- .x %>%
        paste0("/?data.json&paging=false") %>%
        datim_execute_query(username, password, flatten = TRUE) %>%
        purrr::pluck("dataElements") %>%
        tibble::as_tibble()
    })

  #' @title Datim SQLViews
  #'
  datim_sqlviews <- function(base_url = NULL,
                             username = NULL,
                             password = NULL,
                             view_name = NULL,
                             dataset = FALSE) {

    # datim credentials
    if (missing(username))
      username <- datim_user()

    if (missing(password))
      password <- datim_pwd()

    # Base url
    if (missing(base_url))
      base_url <- "https://final.datim.org"

    end_point <- "/api/sqlViews/"

    options <- "?format=json&paging=false"

    # API URL
    api_url <- base_url %>%
      paste0(end_point, options)

    #print(api_url)

    # Query data
    data <- api_url %>%
      datim_execute_query(username, password, flatten = TRUE) %>%
      purrr::pluck("sqlViews") %>%
      tibble::as_tibble() %>%
      rename(uid = id, name = displayName)

    # Filter if needed
    if (!base::is.null(view_name)) {

      print(glue::glue("Searching for SQL View: {view_name} ..."))

      data <- data %>%
        filter(str_detect(
          str_to_lower(name),
          paste0("^", str_to_lower(view_name))))
    }

    # Return only ID when results is just 1 row
    if(base::nrow(data) == 0) {
      base::warning("No match for the requested SQL View")
      return(NULL)
    }
    else if (base::nrow(data) == 1 && dataset == FALSE) {
      return(data$uid)
    }
    else if (base::nrow(data) > 1 && dataset == TRUE) {
      base::warning("There are more than 1 match for the requested SQL View data. Please try to be specific.")
      return(data)
    }
    else if(base::nrow(data) == 1 && dataset == TRUE) {

      dta_uid <- data$uid

      dta_url <- base_url %>%
        paste0(end_point, dta_uid, "/data", options, "&fields=*") #:identifiable, :nameable

      print(glue::glue("SQL View url: {dta_url}"))

      # Query data
      data <- dta_url %>%
        datim_execute_query(username, password, flatten = TRUE)

      # Headers
      headers <- data %>%
        purrr::pluck("listGrid") %>%
        purrr::pluck("headers") %>%
        dplyr::pull(column)

      # Data
      data <- data %>%
        purrr::pluck("listGrid") %>%
        purrr::pluck("rows") %>%
        tibble::as_tibble(.name_repair = "unique") %>%
        janitor::clean_names() %>%
        magrittr::set_colnames(headers)
    }

    return(data)
  }

  datim_sqlviews()
  datim_sqlviews(view_name = "Country, Partner, Agencies")
  datim_sqlviews(view_name = "OU countries", dataset = TRUE)
  datim_sqlviews(view_name = "MER Data Elements", dataset = TRUE)
  datim_sqlviews(view_name = "MER category option combos", dataset = TRUE)





  #' @title Data Elements
  #'
  datim_data_elements <- function(username = NULL,
                                  password = NULL,
                                  base_url = NULL) {

    # datim credentials
    if (missing(username))
      username <- datim_user()

    if (missing(password))
      password <- datim_pwd()

    # Base url
    if (missing(base_url))
      base_url <- "https://final.datim.org"

    # API URL
    api_url <- base_url %>%
      paste0("/api/dataElements?format=json&paging=false&fields=:nameable")#:identifiable

    data <- api_url %>%
      datim_execute_query(username, password, flatten = TRUE) %>%
      purrr::pluck("dataElements") %>%
      tibble::as_tibble()

    print(data)
  }

  datim_data_elements()

  # Data Elements
  datim_mecanisms <- function() {}

  # Validate Site Level Data


## DATA ----

  # MSD - Site x IM

  df_sites <- glamr::return_latest(
      folderpath = glamr::si_path(),
      pattern = "Site_IM_.*_Nigeria"
    ) %>%
    gophr::read_msd()

  df_sites <- df_sites %>%
    filter(fundingagency != "Dedup") %>%
    clean_agency() %>%
    clean_indicator()

  df_sites %>% glimpse()

  df_sites %>% names()

# COLUMNS ----

  # "orgunituid"                 # ID
  # "sitename"
  # "operatingunit"
  # "operatingunituid"
  # "countryname"
  # "snu1"
  # "snu1uid"
  # "psnu"
  # "psnuuid"
  # "snuprioritization"
  # "typemilitary"
  # "dreams"
  #
  # "primepartner"
  # "fundingagency"
  # "mech_code"                  # ID
  # "mech_name"
  # "pre_rgnlztn_hq_mech_code"
  # "prime_partner_duns"
  # "award_number"
  #
  # "communityuid"
  # "community"
  # "communityprioritization"
  # "facilityuid"
  # "facility"
  # "facilityprioritization"
  # "sitetype"
  #
  # "indicator"
  # "numeratordenom"
  # "indicatortype"
  # "disaggregate"
  # "standardizeddisaggregate"
  # "categoryoptioncomboname"    # ID
  # "ageasentered"
  # "trendsfine"
  # "trendssemifine"
  # "trendscoarse"
  # "sex"
  # "statushiv"
  # "statustb"
  # "statuscx"
  # "hiv_treatment_status"
  # "otherdisaggregate"
  # "otherdisaggregate_sub"
  # "modality"
  #
  # "fiscal_year"
  # "targets"
  # "qtr1"
  # "qtr2"
  # "qtr3"
  # "qtr4"
  # "cumulative"
  # "source_name"

# DATIM Data Structure ----

  # DIMENSIONS
  #
  # WHERE
  # Org Units
  # Eg: Clinical Facility, Community Site, or OU Level
  #
  # WHAT
  # Data Elements (Indicators) + Disaggregations [Targets and Results and DSD and TA]
  # Eg: Number of HTC tests for Females 1-4
  #
  # WHEN
  # Reporting Period - Quarter
  # Eg: 2021 qtr2 or January through March 2021
  #
  # WHO
  # Funding mechanism (attribute option combination)
  # Eg: USAID funds RISE which is implemented by FHI360
  #
  # VALUE (MEASUREMENT)
  # Value (Targets, Qtr1-4, Cumulative)

# SITE x IM Structure ----

  str_msd_sites <- list(
    # WHEN
    "periods" = c(
      "fiscal_year"
    ),
    # WHERE
    "orgunits" = c(
      "orgunituid",                 # ID
      "sitename",
      "operatingunit",
      "operatingunituid",
      "countryname",
      "snu1",
      "snu1uid",
      "psnu",
      "psnuuid",
      "snuprioritization",
      "typemilitary",
      "dreams",
      "communityuid",
      "community",
      "communityprioritization",
      "facilityuid",
      "facility",
      "facilityprioritization",
      "sitetype"
    ),
    # WHAT: dataElements
    "indicators" = c(
      "indicator",                  # ID
      "numeratordenom",             # ID
      "indicatortype",              # ID
      "source_name"
    ),
    # WHAT: CategoryOptionsCombo
    "disaggregates" = c(
      "disaggregate",               # ID
      "standardizeddisaggregate",   # ID
      "categoryoptioncomboname",    # ID
      "ageasentered",
      "trendsfine",
      "trendssemifine",
      "trendscoarse",
      "sex",
      "statushiv",
      "statustb",
      "statuscx",
      "hiv_treatment_status",
      "otherdisaggregate",
      "otherdisaggregate_sub",
      "modality"
    ),
    # WHO: Mechs - AttributesOptionsCombo
    "mechanisms" = c(
      "fundingagency",
      "award_number",
      "mech_code",                  # ID
      "mech_name",
      "pre_rgnlztn_hq_mech_code",
      "primepartner",
      "prime_partner_duns"
    ),
    # VALUE
    "values" = c(
      "targets",
      "qtr1",
      "qtr2",
      "qtr3",
      "qtr4",
      "cumulative"
    ),
    # OTHER
    "others" = c(),
    # Slimmed Version of MSD,
    # Similar to Flat Files exported from EMRs
    "data" = c(
      "fiscal_year",  # => Used to reshape reshape / pivot
      "orgunituid",   # => Join to Org Hierarchy
      "mech_code",    # => Join to AttributesOptionsCombo tbl
      "indicator",
      "disaggregate",
      "standardizeddisaggregate",
      "categoryoptioncomboname",
      "targets",
      "qtr1",
      "qtr2",
      "qtr3",
      "qtr4",
      "cumulative"
    ),
    # Columns for cleaned version
    "cleaned" = c()
  )


# NORMALISE

  # df_sites %>%
  #   filter(fundingagency != "Dedup") %>%
  #   select(indicator, numeratordenom) %>%
  #   distinct() %>%
  #   count(indicator)
  #
  # df_sites %>%
  #   filter(fundingagency != "Dedup") %>%
  #   select(indicator, indicatortype) %>%
  #   distinct() %>%
  #   count(indicator)
  #
  # df_sites %>%
  #   filter(fundingagency != "Dedup") %>%
  #   select(indicator, indicatortype, numeratordenom) %>%
  #   distinct() %>%
  #   pivot_wider(names_from = numeratordenom, values_from = indicatortype)

  # Orgs
  df_orgs <- df_sites %>%
    filter(sitetype != "Above Site") %>%
    select(all_of(str_msd_sites$orgunits)) %>%
    distinct()

  # Mechs
  df_mechs <- df_sites %>%
    select(one_of(c(str_msd_sites$periods, str_msd_sites$mechanisms))) %>%
    distinct()

  # Indicators
  df_inds <- df_sites %>%
    select(one_of(c(str_msd_sites$periods,
                    str_msd_sites$indicators,
                    str_msd_sites$disaggregates))) %>%
    distinct()

  # Data
  df_msd <- df_sites %>%
    select(all_of(str_msd_sites$data))

  df_msd %>%
    filter(fiscal_year == 2021,
           indicator == "HTS_TST") %>%
    filter(orgunituid == "XM5FIOVzxeq")


  df_msd %>% reshape_msd()

  df_msd %>%
    head() %>%
    view()

  df_msd %>%
    filter(fiscal_year == 2021,
           indicator %in% c("HTS_TST",
                            "HTS_TST_POS",
                            "HTS_TST_NEG")) %>%
    distinct(standardizeddisaggregate)

  df_inds %>%
    filter(indicator == "HTS_TST") %>%
    distinct(indicator, modality) %>%
    rename(mod = modality) %>%
    clean_modalities(colname = "mod")

  df_inds %>%
    filter(fiscal_year == 2021,
           indicator %in% c("HTS_TST")) %>%
    clean_modalities()

  df_msd %>%
    filter(fiscal_year == 2021,
     #orgunituid == "XM5FIOVzxeq",
     indicator %in% c("HTS_TST"),
     standardizeddisaggregate == "Modality/Age/Sex/Result") %>%
    facility_type()

  df_hts_tn <- df_msd %>%
    filter(fiscal_year == 2021,
           orgunituid == "XM5FIOVzxeq",
           indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG"),
           standardizeddisaggregate == "Total Numerator")


  df_hts_td <- df_msd %>%
    filter(fiscal_year == 2021,
           orgunituid == "XM5FIOVzxeq",
           indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_TST_NEG"),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>%
    group_by(orgunituid, indicator) %>%
    summarise(across(targets:cumulative, sum, na.rm = T), .groups = "drop")

  df_hts_pos <- df_msd %>%
    filter(fiscal_year == 2021,
           orgunituid == "XM5FIOVzxeq",
           indicator %in% c("HTS_TST"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           str_detect(categoryoptioncomboname, "Newly Tested Positives|Positive")) %>%
    group_by(orgunituid, indicator) %>%
    summarise(across(targets:cumulative, sum, na.rm = T), .groups = "drop") %>%
    mutate(indicator = "HTS_TST (Positive)")

  df_hts_neg <- df_msd %>%
    filter(fiscal_year == 2021,
           orgunituid == "XM5FIOVzxeq",
           indicator %in% c("HTS_TST"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           str_detect(categoryoptioncomboname, "New Negatives|Negative")) %>%
    group_by(orgunituid, indicator) %>%
    summarise(across(targets:cumulative, sum, na.rm = T), .groups = "drop") %>%
    mutate(indicator = "HTS_TST (Negative)")

  df_hts_tn %>%
    bind_rows(df_hts_td, df_hts_pos, df_hts_neg)



