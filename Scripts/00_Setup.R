## INSTRUCTIONS ON HOW TO SETUP THIS PROJECT

## Packages:
## Utilities functions
## remotes::install_github("USAID-OHA-SI/glamr", build_vignettes = TRUE)
##
## MSD Import and Munging
## remotes::install_github("USAID-OHA-SI/gophr", build_vignettes = TRUE)

## Folder Structure
## glamr::si_setup()
## All R Scripts go under Scripts folder
## Raw data go under Data folder
## Clean data go under Dataout
## Graphics and Images go under Graphics / Images folders
##
## Use glamr::set_paths() to setup all non-project specific (global) data paths

## Exclusions
## No data should be push to the repo
## Use this to exclude (Should be already already set)
#
# No data
# *.csv
# *.txt
# *.rds
# *.xlsx
# *.xls
# *.zip
# *.png
# *.twbx
# *.pptx
#
# #nothing from these folders
# AI/*
#   GIS/*
#   Images/*
#   Graphics/*