# install.packages("devtools")
# require(devtools)
#
# install_github("pepfar-datim/datim-validation", force=TRUE)
# require(datimvalidation)
#
# install_github("pepfar-datim/datimutils", force=TRUE)
# require(datimutils)
#
# install.packages("sqldf")

#library(devtools

  library(tidyverse)
  library(datimvalidation)
  library(sqldf)
  library(readr)
  library(dplyr)
  library(glamr)

# GLOBALS ----

  dir_flatfiles <- "../../PEPFAR/COUNTRIES/Nigeria/Datim-Validations"

  dir_ff_pd <- "USAID FY21 Q3 Validated Flat Files"

  ff_pd <- "USAID_FY21Q3_Consolidated_FlatFile.csv"

  path <- paste0(dir_flatfiles, "/", dir_ff_pd, "/", ff_pd)

  dir.exists(dir_flatfiles)

  list.files(dir_flatfiles)

  file.exists(path)

# FUNCTIONS ----

  getDataElementCadenceMapForPeriod <- function(period,d2session = d2_default_session) {
    url <-
      URLencode(
        paste0(
          d2session$base_url,
          "api/",
          api_version(),
          "/dataStore/dataElementCadence/",
          period
        )
      )
    sig <- digest::digest(url, algo = 'md5', serialize = FALSE)
    cadence_map <- getCachedObject(sig)
    if (is.null(cadence_map)) {
      r <- httr::GET(url , httr::timeout(300), handle = d2session$handle)
      if (r$status == 200L) {
        r <- httr::content(r, "text")
        cadence_map_json <- jsonlite::fromJSON(r)
        cadence_map<-cadence_map_json$dataElements
        cadence_map$period<-cadence_map_json$period
        saveCachedObject(cadence_map, sig)
      } else {
        stop("Could not retreive data element cadence map for period ",
             period)
      }
    }
    return(cadence_map)
  }

# RAW DATA ----

  df_ff <- path %>% read_csv()

  df_dims <- datim_dimensions()

  df_disaggs <- glamr::datim_dim_items("Disaggregation Type")

  df_coc_map <- datimvalidation::getCategoryOptionCombosMap()
  df_de_map <- datimvalidation::getDataElementMap()
  #datimvalidation::getPeriodInfo("2021Q2")
  df_orgs_map <- datimvalidation::getOrganisationUnitMap()

  # https://final.datim.org/api/dataSets.json?paging=false
  df_mer_ds <- getCurrentMERDataSets(type="RESULTS")

  # df_mer_ds %>% map_dfr(function(id){
  #   url <- glue::glue("https://final.datim.org/api/sqlViews/{id}/data.json&paging=false")
  # })
  df_de <- getValidDataElements(df_mer_ds)

# Pre-PROCESSING ----

  clearCache(force=TRUE)
  rm(list=ls())

  #clear environment
  rm(list=ls())

  #clear cache
  datimvalidation::clearCache(force=TRUE)

  # secrets="./../_secrets/datim_validation_secret.json"
  # datimutils::loginToDATIM(config_path = secrets)

  datimutils::loginToDATIM(username = glamr::datim_user(),
                           password = glamr::datim_pwd(),
                           base_url = "https://final.datim.org/")

  type <- "csv"
  idScheme <- "id"
  dataElementIdScheme <- "id"
  orgUnitIdScheme <- "id"
  expectedPeriod <- "2021Q3"

  ds <- getCurrentMERDataSets()
  ts <- format(Sys.time(), "%y%m%d%H%M%S")

  # parse: checks for valide OU, DE, COC, AOC UIDs
  d <- d2Parser(file = path,
                type = type,
                invalidData = TRUE,
                idScheme = idScheme,
                dataElementIdScheme = dataElementIdScheme,
                orgUnitIdScheme = orgUnitIdScheme)


  ## check for expected period
  invalidPeriod <- d[d$period != expectedPeriod, ]

  if(nrow(invalidPeriod) != 0){
    write.csv(invalidPeriod, paste0(path, '_invalidPeriod', ts, '.csv'), na="")
  }

  # check data element cadence
  dePeriodIssues <- datimvalidation::checkDataElementCadence(d)

  if(any(class(dePeriodIssues) == "data.frame") && nrow(dePeriodIssues) != 0){
    write.csv(dePeriodIssues, paste0(path, '_dePeriodIssues', ts, '.csv'), na="")
    print("Data elements in wrong period encountered. Printing out summaries.")
  }


  # check for duplicates
  duplicates <- getExactDuplicates(d)

  if(any(class(duplicates) == "data.frame")){
    if(nrow(duplicates) > 0){
      print("Duplicates encountered. Printing out summaries.")
      write.csv(duplicates, paste0(path, '_duplicates', ts, '.csv'), na="")
    }
  }

  # data element/coc pair validity
  # Get MER Results category option combo uid -> join and filter
  # getValidDataElements(datasets)
  de_disags <- checkDataElementDisaggValidity(d, ds)

  if(any(class(de_disags) == "data.frame")){
    if(nrow(de_disags > 0)){
      print("Invalid data element/coc pairs encountered. Printing out summaries.")
      write.csv(de_disags, paste0(path, '_invalid_de_coc', ts, '.csv'), na="")
      de_disags2 <- unique(de_disags[, c("dataElement", "categoryOptionCombo")])
      write.csv(de_disags2, paste0(path, '_invalid_de_coc_uniques', ts, '.csv'), na="")
    }
  }

  # data element/org unit check
  de_ou <- checkDataElementOrgunitValidity(data=d, datasets=ds)

  if(any(class(de_ou) == "data.frame") && nrow(de_ou) > 0){
    print("Invalid data element/org unit pairs encountered. Printing out summaries.")
    write.csv(de_ou, paste0(path, '_invalid_de_ou', ts, '.csv'), na="")
  }

  # Negative values
  negativeValues <- checkNegativeValues(d)

  if(any(class(negativeValues) == "data.frame") && nrow(negativeValues) > 0){
    print("Negative values encountered. Printing out summaries.")
    write.csv(negativeValues, paste0(path, '_negativeValues', ts, '.csv'), na="")
  }

  # value type compliance
  valueTypeCompliance <- checkValueTypeCompliance(d)

  if(any(class(valueTypeCompliance) == "data.frame") && nrow(valueTypeCompliance) > 0){
    print("Value type compliance issues encountered. Printing out summaries.")
    write.csv(valueTypeCompliance, paste0(path, '_valueTypeCompliance', ts, '.csv'), na="")
  }

  # mechanism validity
  mechanismValidity <- checkMechanismValidity(d)

  if(any(class(mechanismValidity) == "data.frame") && nrow(mechanismValidity) > 0){
    print("Mechanism validity issues encountered. Printing out summaries.")
    write.csv(mechanismValidity, paste0(path, '_mechanismValidity', ts, '.csv'), na="")
  }

  #validation rules
  vr_violations <- validateData(data=d, return_violations_only=TRUE, parallel=FALSE)

  if(any(class(vr_violations) == "data.frame") && nrow(vr_violations) > 0){
    print("Validation rule violations encountered. Printing out summaries.")
    write.csv(vr_violations, paste0(path, '_vr_', ts, '.csv'), na="")
  }
