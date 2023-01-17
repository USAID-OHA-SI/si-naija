# Libraries

  install.packages("devtools")
  devtools::install_github("pepfar-datim/datim-validation", force=TRUE)
  devtools::install_github("pepfar-datim/datimutils", force=TRUE)

  library(datimvalidation)
  library(datimutils)
  library(sqldf)
  library(dplyr)
  library(magrittr)

  # clearCache(force=TRUE)
  # rm(list=ls())

# Credentials

  #secrets <- {"dhis": { "baseurl": "https://dev-de.datim.org/", "username": "admin", "password": "district" } }

  # secrets <- list(
  #   "dhis" = list(
  #     "baseurl" = "https://dev-de.datim.org/",
  #     "username" = "admin",
  #     "password" = "district"
  #     )
  #   )

  dir_secrets <- "../../DATIM/.credentials"

  list.files(dir_secrets, full.names = TRUE)

  file_secrets = file.path(dir_secrets, "secret.json")

  loginToDATIM(file_secrets)

# Inputs files

  dir_files <- "./Data/Datim-Imports/FY22Q4"

  flat_files <- list.files(path = dir_files,
             #pattern = "^FY22Q4_DATIM_Import.*.csv$",
             full.names = TRUE)

  flat_files <- list.files(path = dir_files,
                           pattern = "^FY22Q4_DATIM_Import.*.csv$",
                           full.names = TRUE)

  path = flat_files[1]
  type <- "csv"
  idScheme <- "id"
  dataElementIdScheme <- "id"
  orgUnitIdScheme <- "id"
  expectedPeriod <- "2022Q3"

# Process files

  ts <- format(Sys.time(), "%Y-%m-%d %H%M%S")

  # parse: checks for valida OU, DE, COC, AOC UIDs
  d <- d2Parser(file = path,
                type = type,
                invalidData = TRUE,
                idScheme = idScheme,
                dataElementIdScheme = dataElementIdScheme,
                orgUnitIdScheme = orgUnitIdScheme)

  class(d)

  ## File Metadata
  d$info

  d$info$filename
  d$info$datastream
  d$info$datasets

  ## File Data
  d$data$parsed %>% glimpse()
  d$data$import %>% glimpse()


  invalidPeriod <- d$data$import[d$data$import$period != expectedPeriod, ]

  invalidPeriod <- d %>%
    extract2("data") %>%
    extract2("import") %>%
    filter(period != expectedPeriod)

  ## Run all Validations

  d_vals <- datimvalidation::runValidation(d)

  d_vals$info$messages
  d_vals$info$has_error
  d_vals$data$parsed
  d_vals$data$import

  # OR Step at the time

  #Exact duplicates
  d <- getExactDuplicates(d)

  #Check orgunits are within users hierarchy
  d <- checkOrgunitsInHierarchy(d,
                                userOrgUnit = d$info$organisationUnit,
                                d2session = d2session)

  #Check data element cadence
  d <- checkDataElementCadence(d, d2session)

  # Check Data element disagg validity

  d <- checkDataElementDisaggValidity(d,
                                      datasets = datasets,
                                      d2session = d2session)

  #Check Data element orgunit validity

  d <- checkDataElementOrgunitValidity(d,
                                       datasets = datasets,
                                       d2session = d2session)

  #Check mechanism validity

  d <- checkMechanismValidity(d, organisationUnit = d$info$organisationUnit, d2session =  d2session)

  if (d$info$datastream %in% c("RESULTS", "TARGETS")) {
    #Negative values
    d <- checkNegativeValues(d, d2session = d2session)
    #Value type compliance
    d <- checkValueTypeCompliance(d, d2session = d2session)
    #Validation rules
    d <- checkValidationRules(d, d2session = d2session)
  }





## OLD SCRIPTS

  ## check for expected period
  invalidPeriod <- df[df$period != expectedPeriod, ]

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
  duplicates <- datimvalidation::getExactDuplicates(d)

  if(any(class(duplicates) == "data.frame")){
    if(nrow(duplicates) > 0){
      print("Duplicates encountered. Printing out summaries.")
      write.csv(duplicates, paste0(path, '_duplicates', ts, '.csv'), na="")
    }
  }

  # data element/coc pair validity
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
  de_ou <- checkDataElementOrgunitValidity(d=df, datasets=ds)

  if(any(class(de_ou) == "data.frame") && nrow(de_ou) > 0){
    print("Invalid data element/org unit pairs encountered. Printing out summaries.")
    write.csv(de_ou, paste0(path, '_invalid_de_ou', ts, '.csv'), na="")
  }

  # Negative values
  negativeValues <- checkNegativeValues(df)

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
  vr_violations<-validateData(data=d, return_violations_only=TRUE, parallel=FALSE)
  if(any(class(vr_violations) == "data.frame") && nrow(vr_violations) > 0){
    print("Validation rule violations encountered. Printing out summaries.")
    write.csv(vr_violations, paste0(path, '_vr_', ts, '.csv'), na="")
  }