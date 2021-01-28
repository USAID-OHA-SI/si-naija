##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: HTS_TST
##  LICENCE: MIT
##  DATE:    2021-01-28


# PACKAGES -------------------------------------------------

library(tidyverse)
library(readxl)
library(glitr)
library(tidytext)
library(scales)
library(glamr)
library(sf)
library(raster)
library(gisr)
library(janitor)
library(extrafont)
library(ICPIutilities)

# GLOBAL --------------------------------------------------

# Load configs
source("./Scripts/N00_Config.R")

# Country name
cntry <- "Nigeria"


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

# Geodata
file_shp <- list.files(
  path = shpdata,
  pattern = "VcPepfarPolygons.shp$",
  recursive = TRUE,
  full.names = TRUE
) %>%
  sort() %>%
  last()


# DATA ----


# PSNU
df_msd <- file_msd %>%
  read_msd() %>%
  reshape_msd(clean = TRUE)


df_msd %>%
  filter(str_detect(indicator, "^HTS_")) %>%
  distinct(indicator)

df_msd %>%
  filter(indicator == "HTS_TST") %>%
  distinct(indicator, standardizeddisaggregate)

df_msd %>%
  filter(indicator == "HTS_TST",
         standardizeddisaggregate == "Total Numerator",
         period_type %in% c("cumulative", "targets")) %>%
  distinct(period, period_type, indicator) %>%
  prinf()


# HTS_TST
df_hts_tst <- df_msd %>%
  clean_agency() %>%
  filter(indicator == "HTS_TST",
         standardizeddisaggregate == "Total Numerator",
         period_type %in% c("cumulative", "targets")) %>%
  group_by(fundingagency, period, period_type) %>%
  summarise_at(vars(val), sum, na.rm = T) %>%
  ungroup() %>%
  pivot_wider(names_from = period_type, values_from = val) %>%
  mutate(achieve = cumulative / targets * 100,
         label_results = if_else(is.na(cumulative),
                                 paste0(period, " (NA)"),
                                 paste0(period, " (", comma(cumulative, 1), ")")),
         label_achieve = if_else(is.na(achieve),
                                 paste0(period, " (NA)"),
                                 paste0(period, " (", round(achieve), "%)")))

df_hts_tst %>% glimpse()


# VIZ ----

## HTS_TST - USAID
df_hts_tst %>%
  filter(fundingagency == "USAID") %>%
  ggplot(aes(reorder(label_results, targets), targets,
             fill = targets,
             label = ifelse(is.na(achieve), "", paste0(round(achieve), "%")))) +
  geom_col(show.legend = F) +
  geom_text(size = 3, color = usaid_darkgrey, hjust = 0) +
  scale_fill_si(discrete = F, reverse = T) +
  scale_y_continuous(position = "right", labels = comma) +
  coord_flip() +
  labs(x = "", y = "",
       title = "USAID | HTS_TST Achievements (FY18 - FY20)",
       subtitle = "FY19 & 20 have lower targets but achievements have been consistent",
       caption = paste0("Bars are sorted by targets, results appended to Fiscal Years\nData source: MSD FY20Q4c, OHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"))) +
  si_style_xgrid()

ggsave(file.path(graphics, paste0("Nigeria - USAID - HTS_TST Historical Achievements - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
       plot = last_plot(), scale = 1.2, dpi = 310,
       width = 10, height = 7, units = "in")

## HTS_TST - USAID
df_hts_tst %>%
  filter(fundingagency == "CDC") %>%
  ggplot(aes(reorder(label_results, targets), targets,
             fill = targets,
             label = ifelse(is.na(achieve), "", paste0(round(achieve), "%")))) +
  geom_col(show.legend = F) +
  geom_text(size = 3, color = usaid_darkgrey, hjust = 0) +
  scale_fill_si(discrete = F, reverse = T) +
  scale_y_continuous(position = "right", labels = comma) +
  coord_flip() +
  labs(x = "", y = "",
       title = "CDC | HTS_TST Achievements (FY18 - FY20)",
       subtitle = "FY18 & 20 have been the best years in terms of achievements",
       caption = paste0("Bars are sorted by targets, results appended to Fiscal Years\nData source: MSD FY20Q4c, OHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"))) +
  si_style_xgrid()

ggsave(file.path(graphics, paste0("Nigeria - CDC - HTS_TST Historical Achievements - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
       plot = last_plot(), scale = 1.2, dpi = 310,
       width = 10, height = 7, units = "in")

## HTS_TST - ALL
df_hts_tst %>%
  group_by(period) %>%
  summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(achieve = cumulative / targets * 100,
         label_results = if_else(
           cumulative == 0,
           paste0(period, " (NA)"),
           paste0(period, " (", comma(cumulative, 1), ")")),
         label_achieve = if_else(
           achieve == 0,
           paste0(period, " (NA)"),
           paste0(period, " (", round(achieve), "%)"))
         ) %>%
  ggplot(aes(reorder(label_results, targets), targets,
           fill = targets,
           label = ifelse(achieve == 0, "", paste0(round(achieve), "%")))) +
  geom_col(show.legend = F) +
  geom_text(size = 3, color = usaid_darkgrey, hjust = 0) +
  scale_fill_si(discrete = F, reverse = T) +
  scale_y_continuous(position = "right", labels = comma) +
  coord_flip() +
  labs(x = "", y = "",
       title = "USAID | HTS_TST Achievements (FY18 - FY20)",
       subtitle = "As an OU, FY18 & 20 have been the best years in terms of achievements",
       caption = paste0("Bars are sorted by targets, results appended to Fiscal Years\nData source: MSD FY20Q4c, OHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"))) +
  si_style_xgrid()

ggsave(file.path(graphics, paste0("Nigeria - ALL - HTS_TST Historical Achievements - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
       plot = last_plot(), scale = 1.2, dpi = 310,
       width = 10, height = 7, units = "in")



