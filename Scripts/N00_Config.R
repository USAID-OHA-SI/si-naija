# DIR - Global

# merdata <- glamr::si_path("path_msd")
# rasdata <- glamr::si_path("path_raster")
# shpdata <- glamr::si_path("path_vector")

dir_geodata <- si_path("path_vector")
dir_terr <- si_path("path_raster")
dir_merdata <- si_path("path_msd")

# DIR - project

# data <- "./Data"
# dataout <- "./Dataout"
# images <- "./Images"
# graphics <- "./Graphics"

dir_data <- "Data"
dir_dataout <- "Dataout"
dir_gis <- "GIS"
dir_graphics <- "Graphics"

# Country name
country <- "Nigeria"

# IP/IM Shortnames

# mutate(
#   mech_name = case_when(
#     # USAID
#     mech_name == "Meeting Targets and Maintaining Epidemic Control (EpiC)" ~ "EpiC",
#     mech_name == "STRENGHTENING INTERGRATED DELIVERY OF HIV/AIDS SERVICES(SIDHAS)" ~ "SIDHAS",
#     mech_name == "SHARP Task Order 1" ~ "SHARP TO1",
#     mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 2" ~ "SHARP TO2",
#     mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 3" ~ "SHARP TO3",
#     mech_name == "Reaching Impact, Saturation and Epidemic Control (RISE)" ~ "RISE",
#     mech_name == "Integrated Child Health and Social Services Award (ICHSSA 1)" ~ "ICHSSA 1",
#     mech_name == "Integrated Child Health and Social Services Award (ICHSSA 2)" ~ "ICHSSA 2",
#     mech_name == "Integrated Child Health and Social Services Award (ICHSSA 3)" ~ "ICHSSA 3",
#     mech_name == "Integrated Child Health and Social Services Award (ICHSSA 4)" ~ "ICHSSA 4",
#     # CDC
#     mech_name == "Partnering Effectively to end AIDS through Results and Learning (PEARL)_2097" ~ "PEARL",
#     mech_name == "Global Action towards HIV Epidemic Control in Subnational units in Nigeria (4GATES PROJECT)_2100" ~ "4GATES",
#     mech_name == "ACTION to Control HIV Epidemic through Evidence (ACHIEVE)_2099" ~ "ACHIEVE",
#     mech_name == "Improving Comprehensive AIDS Response Enhanced for Sustainability (iCARES)_2098" ~ "iCARES",
#     TRUE ~ mech_name
#   ),
#   primepartner = case_when(
#     # USAID
#     primepartner == "Family Health International" ~ "FHI360",
#     primepartner == "Chemonics International, Inc." ~ "Chemonics",
#     primepartner == "JHPIEGO CORPORATION" ~ "JHPIEGO",
#     primepartner == "SOCIETY FOR FAMILY HEALTH" ~ "SFH",
#     primepartner == "HEARTLAND ALLIANCE LTD-GTE" ~ "HAN",
#     primepartner == "CENTER FOR CLINICAL CARE AND CLINICAL RESEARCH LTD GTE" ~ "C4C3R",
#     primepartner == "ASSOCIATION FOR REPRODUCTIVE AND FAMILY HEALTH" ~ "A4RnFH",
#     primepartner == "PRO-HEALTH INTERNATIONAL" ~ "ProHI",
#     # CDC
#     primepartner == "APIN PUBLIC HEALTH INITIATIVES LTD/GTE" ~ "APHI",
#     primepartner == "INSTITUTE OF HUMAN VIROLOGY" ~ "IHVN",
#     primepartner == "CATHOLIC CARITAS FOUNDATION OF NIGERIA" ~ "CCFN",
#     primepartner == "CENTRE FOR INTEGRATED HEALTH PROGRAMS" ~ "CIHP",
#     TRUE ~ primepartner
#   ),
#   mech_label = paste0(primepartner, " ", mech_name)
# )