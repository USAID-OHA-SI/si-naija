##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: PANO Data Extraction
##  LICENCE: MIT
##  DATE:    2021-11-22

# PACKAGES ----
library(tidyverse)
library(glamr)
library(grabr)

# DIRS
dir_merdata <- si_path(type = "path_msd")

# PANO ACCESS ----
user <- pano_user()
pass <- pano_pwd()

sess <- pano_session(username = user, password = pass)

return_latest(si_path(), "MSD_PSNU_IM") %>%
  gophr::read_msd()

# OU
cntry = "Nigeria"

# Extract all global & Country Specific MDS
pano_extract_msds(operatingunit = cntry,
                  archive = TRUE,
                  dest_path = dir_merdata,
                  username = user,
                  password = pass)

# Specific Runs

# CURRENT RUN OPTIONS ----
msd_v <- "clean"
fy <- 2022
qtr <- 3
org_level <- "psnu"

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
