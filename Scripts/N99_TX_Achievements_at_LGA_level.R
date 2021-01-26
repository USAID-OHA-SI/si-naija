##  PROJECT: Performance Summary Tables
##  AUTHOR:  jdavis | USAID
##  PURPOSE: Pull Summary clinical cascade data
##  LICENCE: MIT
##  DATE:    2021-01-19


# PACKAGES -------------------------------------------------

library(tidyverse)
library(readxl)
library(glitr)
library(scales)
library(glamr)
library(gisr)
library(janitor)
library(extrafont)

# GLOBAL --------------------------------------------------

# Load configs
source("./Scripts/N00_Config.R")

# Country name
country <- "Nigeria"

# file
file_targets <- list.files(
  path = data,
  pattern = "^Site Tool_Nig.*_\\d{14}_F.*.xlsx$",
  full.names = TRUE
)

# Latest MSD PSNU x IM File
file_msd <- list.files(
  path = merdata,
  pattern = "MER_S.*_PSNU_IM_.*_\\d{8}_v.*_N.*.zip",
  full.names = TRUE
) %>%
  sort() %>%
  last()

# Latest MSD Site x IM File
file_msd_sites <- list.files(
  path = merdata,
  pattern = "MER_S.*_Site_IM_.*_\\d{8}_v.*_N.*.zip",
  full.names = TRUE
) %>%
  sort() %>%
  last()

# DATA ----------------------------------------------------

# MSD Data

# PSNU
df_msd <- file_msd %>%
  read_msd() %>%
  reshape_msd(clean = TRUE)

df_msd %>% glimpse()

# Sites
df_msd_sites <- file_msd_sites %>%
  read_msd() %>%
  reshape_msd(clean = TRUE)

df_msd_sites %>% glimpse()

# IMs
df_ims <- df_msd %>%
  distinct(mech_code, mech_name, primepartner)

df_ims %>% glimpse()

# MSD PSNU Targets
df_msd_psnu_trgts <- df_msd %>%
  clean_agency() %>%
  filter(operatingunit == country,
         #fundingagency == "USAID",
         fundingagency %in% c("USAID", "CDC"),
         indicator %in% c("TX_CURR", "TX_NEW"),
         period == "FY20",
         period_type %in% c("targets", "cumulative"),
         standardizeddisaggregate == "Total Numerator",
         fundingagency != "Dedup") %>%
  group_by(psnu, indicator, period_type) %>%
  summarise_if(is.numeric, ~sum(., na.rm = TRUE)) %>%
  ungroup()

df_msd_psnu_trgts %>% glimpse()