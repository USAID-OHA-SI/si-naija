##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: PANO Data Extraction
##  LICENCE: MIT
##  DATE:    2021-11-22
##  UPDATED: 2023-02-18

# PACKAGES ----
library(tidyverse)
library(grabr)
library(glamr)
library(grabr)

# DIRS
dir_merdata <- si_path(type = "path_msd")

# PANO ACCESS ----
user <- pano_user()
pass <- pano_pwd()

sess <- pano_session(username = user, password = pass)

# OU
cntry = "Nigeria"

# Extract all global & Country Specific MSD
pano_extract_msds(operatingunit = cntry,
                  archive = TRUE,
                  dest_path = dir_merdata,
                  username = user,
                  password = pass)

# Specific Runs

# CURRENT RUN OPTIONS ----
msd_v <- "final"
fy <- 2022
qtr <- 4
org_level <- "psnu"

# OU
cntry <- "Nigeria"

# DATA ----
items <- pano_extract(item = "mer",
                      version = msd_v,
                      fiscal_year = fy,
                      quarter = qtr,
                      username = user,
                      password = pass,
                      unpack = TRUE)

# DOWNLOAD GLOBAL DATASETS ----

items %>%
  filter(type == "file zip_file",
         parent == "MER FY2022 Q3 Clean") %>%
  pull(path) %>%
  walk(~pano_download(item_url = .x,
                      session = sess,
                      dest = dir_merdata))


# DOWNLOAD COUNTRY DATASETS ----

# All country datasets
pano_extract_msds(operatingunit = cntry,
                  archive = TRUE,
                  dest_path = si_path())

# Site
pano_extract_msd(operatingunit = cntry,
                 version = msd_v,
                 fiscal_year = fy,
                 quarter = qtr,
                 level = "site",
                 dest_path = dir_merdata)

# PSNU
pano_extract_msd(operatingunit = cntry,
                 version = msd_v,
                 fiscal_year = fy,
                 quarter = qtr,
                 level = "psnu",
                 dest_path = dir_merdata)
