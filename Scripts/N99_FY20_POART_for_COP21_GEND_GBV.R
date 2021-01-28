##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: GEND_GBV Sexual Violence Cases
##  LICENCE: MIT
##  DATE:    2021-01-27

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
  filter(str_detect(indicator, "^GEND")) %>%
  distinct(indicator)

df_msd %>%
  filter(str_detect(indicator, "^GEND")) %>%
  distinct(otherdisaggregate)

df_msd %>%
  filter(str_detect(period, "^FY20"),
         period_type == "results",
         indicator == "GEND_GBV",
         standardizeddisaggregate == "Age/Sex/ViolenceType"
  ) %>%
  distinct(period)

# filter by results and period
df_msd %>%
  filter(period == "FY20Q4",
         period_type == "results",
         indicator == "GEND_GBV",
         standardizeddisaggregate == "Age/Sex/ViolenceType"
         ) %>%
  count(fundingagency, snu1, standardizeddisaggregate, trendscoarse, sex, otherdisaggregate) %>%
  filter(fundingagency == "USAID") %>%
  arrange(snu1, desc(n)) %>%
  View()

# Sexual violence
df_sexv <- df_msd %>%
  filter(period_type == "results",
         indicator == "GEND_GBV_SexualViolence",
         period_type == "results"
  ) %>%
  group_by(fundingagency, period, snu1) %>%
  summarise(val = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(fundingagency, period, desc(val)) %>%
  group_by(fundingagency, period) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  clean_agency() %>%
  mutate(fundingagency = factor(fundingagency,
                                levels = c("USAID", "CDC", "DOD")),
         period = str_remove(period, "Q4"),
         snu1 = str_remove(snu1, " Nigeria"),
         snu1 = paste0(snu1, " (", comma(val, 1), ")"))


# VIZ

  ## GEND_GBV - USAID
  df_sexv %>%
    filter(fundingagency == "USAID",
           order <= 10) %>%
    ggplot(aes(reorder_within(snu1, val, period), val, fill = val)) +
    geom_col(show.legend = F) +
    facet_wrap(~ period, scales = "free") +
    scale_x_reordered() +
    scale_fill_si(palette = "burnt_siennas", discrete = F, reverse = T) +
    coord_flip() +
    labs(x = "", y = "",
         title = "USAID | GEND_GBV Sexual Violence",
         subtitle = "Top 10 states with the highest sexual violence cases",
         caption = paste0("USAID - Data source: MSD FY20Q4c\nOHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"))) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          plot.title = )

  ggsave(file.path(graphics, paste0("Nigeria - USAID - GEND_GBV Sexual Violence - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")

  ## GEND_GBV - CDC
  df_sexv %>%
    filter(fundingagency == "CDC",
           order <= 10) %>%
    ggplot(aes(reorder_within(snu1, val, period), val, fill = val)) +
    geom_col(show.legend = F) +
    facet_wrap(~ period, scales = "free") +
    scale_x_reordered() +
    scale_fill_si(palette = "burnt_siennas", discrete = F, reverse = T) +
    coord_flip() +
    labs(x = "", y = "",
         title = "CDC | GEND_GBV Sexual Violence",
         subtitle = "Top 10 states with the highest sexual violence cases",
         caption = paste0("USAID - Data source: MSD FY20Q4c\nOHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"))) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          plot.title = )

  ggsave(file.path(graphics, paste0("Nigeria - CDC - GEND_GBV Sexual Violence - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")

  ## GEND_GBV - CDC
  df_sexv %>%
    group_by(period, snu1) %>%
    summarise(val = sum(val, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(period, desc(val)) %>%
    group_by(period) %>%
    mutate(order = row_number()) %>%
    ungroup() %>%
    filter(order <= 10) %>%
    ggplot(aes(reorder_within(snu1, val, period), val, fill = val)) +
    geom_col(show.legend = F) +
    facet_wrap(~ period, scales = "free") +
    scale_x_reordered() +
    scale_fill_si(palette = "burnt_siennas", discrete = F, reverse = T) +
    coord_flip() +
    labs(x = "", y = "",
         title = "NIGERIA | GEND_GBV Sexual Violence",
         subtitle = "Top 10 states with the highest sexual violence cases",
         caption = paste0("USAID - Data source: MSD FY20Q4c\nOHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"))) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          plot.title = )

  ggsave(file.path(graphics, paste0("Nigeria - ALL - GEND_GBV Sexual Violence - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")



