##
##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: COP23 - Targets validation for IP Workplan
##  LICENCE: MIT
##  DATE:    2023-08-08
##  UPDATED: 2023-08-09

# Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glamr)
  library(tameDP)
  library(glue)
  library(openxlsx)

  source("./Scripts/N00_Utilities.R")

# GLOBALS ----

  ## Directories

  dir_merdata <- si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_cop23 <- "../../PEPFAR/COUNTRIES/Nigeria/COPs/COP23/Tools"

  #dir_cop23 %>% open_path()

  ## Files
  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY21.*_N")

  file_cop_dp <- dir_cop23 %>%
    return_latest("Target Setting Tool_Nigeria_\\d{14}_\\d{8}_10pm WAT.*.xlsx$")

  file_psnu_im <- dir_cop23 %>%
    return_latest("PSNUxIM_N.*_\\d{8}_\\d{6}")

  #file_psnu_im %>% open_path()


# Data -----

  df_cop <- file_cop_dp %>% tame_dp()

  df_cop %>% glimpse()

  df_cop %>%
    filter(fiscal_year = psnu %in% c("Akwa Ibom", "Cross River"),
           indicator == "HTS_TST_POS")

  df_cop_psnu <- df_cop %>%
    summarise(across(targets, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnu, indicator, standardizeddisaggregate))

  ## Save look up tables
  wb <- createWorkbook()

  sheet_name <- "COP23 Targets"

  file_targets <- "COP23 Targets - IP Workplan Validations.xlsx"

  addWorksheet(wb, sheetName = sheet_name)

  writeDataTable(wb = wb,
                 sheet = sheet_name,
                 x = df_cop_psnu)

  saveWorkbook(wb = wb,
               file = file.path(".", dir_dataout, file_targets),
               overwrite = TRUE)

  file.path(".", dir_dataout, file_targets) %>% open_path()


  # df_ims <- file_psnu_im %>%
  #   tame_dp(type = "PSNUxIM", psnu_lvl = T)
  #
  # df_ims %>% glimpse()
  #
  # df_ims %>% distinct(indicator, standardizeddisaggregate)
  #
  # df_ims %>%
  #   summarise(across(targets, sum, na.rm = T),
  #             .by = c(fiscal_year, mech_code, indicator, standardizeddisaggregate))

