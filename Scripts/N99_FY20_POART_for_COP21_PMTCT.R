##  PROJECT: SI Support for Nigeria
##  AUTHOR:  B.Kagniniwa | USAID
##  PURPOSE: PMTCT
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

# FUNCTIONS ----

#' @title Plot PMTCT Achievements
#' @param agency Funding Agency
#' @param ind    Indicator
#'
plot_pmtct_ach <- function(agency, ind, pd = "FY20Q4") {

  # params
  agency <- {{agency}}
  ind <- {{ind}}
  pd <- {{pd}}

  print(paste0(agency, " - ", ind))

  #plot
  df <- df_pmtct %>%
    filter(fundingagency == agency,
           indicator == ind,
           period == pd,
           !is.na(results))

  print(nrow(df))

  if (nrow(df) == 0) {
    print("Indicator not present")

    return(NULL)
  }

  viz <- df %>%
    ggplot(aes(reorder_within(label_results, targets, period), targets,
               fill = targets,
               label = ifelse(is.na(achieve), "", paste0(round(achieve), "%")))) +
    geom_col(show.legend = F) +
    geom_text(size = 3, color = usaid_darkgrey, hjust = 0) +
    scale_fill_si(palette = "scooters", discrete = F, reverse = T) +
    scale_y_continuous(position = "right", labels = comma) +
    scale_x_reordered() +
    coord_flip(clip = "off") +
    facet_wrap(~period, scales = "free") +
    labs(x = "", y = "",
         title = paste0(agency, " | ", ind, " Achievements (FY18 - FY20)"),
         caption = paste0("Data source: MSD FY20Q4c, OHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"),
                          "\nNote: bars represent targets, results appended to state names, and achievements used as labels")) +
    si_style_xgrid() +
    theme(strip.placement = "outsite")

  print(viz)

  ggsave(file.path(graphics, paste0("Nigeria - ",
                                    agency, " - ", ind,
                                    " - ", pd,
                                    " - Historical Achievements - ",
                                    format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")
}

df_pmtct %>%
  filter(fundingagency == "USAID",
         indicator == "PMTCT_EID",
         period == "FY20Q4",
         !is.na(results))

#' @title Plot PMTCT Achievements
#' @param agency Funding Agency
#' @param ind    Indicator
#'
plot_pmtct_rst <- function(agency, ind, pd = "FY20Q4") {

  # params
  agency <- {{agency}}
  ind <- {{ind}}
  pd <- {{pd}}

  print(paste0(agency, " - ", ind))

  #plot
  df <- df_pmtct %>%
    filter(fundingagency == agency,
           indicator == ind,
           period == pd,
           !is.na(cumulative)) %>%
    mutate(snu1 = str_remove(snu1, " Nigeria"))

  print(nrow(df))

  if (nrow(df) == 0) {
    print("Indicator not present")

    return(NULL)
  }

  viz <- df %>%
    ggplot(aes(reorder_within(snu1, cumulative, period), cumulative,
               fill = cumulative, label = cumulative)) +
    geom_col(show.legend = F) +
    geom_text(size = 3, color = usaid_darkgrey, hjust = 0) +
    scale_fill_si(palette = "scooters", discrete = F, reverse = T) +
    scale_y_continuous(position = "right", labels = comma) +
    scale_x_reordered() +
    coord_flip(clip = "off") +
    facet_wrap(~period, scales = "free") +
    labs(x = "", y = "",
         title = paste0(agency, " | ", ind, " Results (FY18 - FY20)"),
         caption = paste0("Data source: MSD FY20Q4c, OHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"),
                          "\nNote: bars represent targets, results appended to state names, and achievements used as labels")) +
    si_style_xgrid() +
    theme(strip.placement = "outsite")

  print(viz)

  ggsave(file.path(graphics, paste0("Nigeria - ",
                                    agency, " - ", ind,
                                    " - ", pd,
                                    " Historical Results - ",
                                    format(Sys.Date(), "%Y-%m-%d"), ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")
}



# DATA ----


# PSNU
df_msd <- file_msd %>%
  read_msd() %>%
  reshape_msd(clean = TRUE)


df_msd %>%
  filter(str_detect(indicator, "^PMTCT")) %>%
  distinct(indicator)

inds <- c("PMTCT_EID",
          "PMTCT_FO",
          "PMTCT_HEI_POS",
          "PMTCT_STAT",
          "PMTCT_ART")

df_msd %>%
  filter(indicator %in% inds) %>%
  distinct(indicator, standardizeddisaggregate) %>%
  arrange(indicator, standardizeddisaggregate)

df_msd %>%
  filter(fundingagency == "USAID", indicator %in% inds) %>%
  distinct(indicator, standardizeddisaggregate) %>%
  arrange(indicator, standardizeddisaggregate)

df_msd %>%
  filter(fundingagency == "USAID", indicator %in% inds) %>%
  distinct(indicator, standardizeddisaggregate, period) %>%
  arrange(indicator, standardizeddisaggregate) %>% prinf()

df_msd %>%
  filter(indicator %in% inds,
         standardizeddisaggregate == "Total Numerator",
         period_type %in% c("cumulative", "targets")) %>%
  distinct(period, period_type, indicator) %>%
  prinf()


# PMTCT
df_pmtct <- df_msd %>%
  clean_agency() %>%
  filter(indicator %in% inds,
         standardizeddisaggregate == "Total Numerator",
         period_type %in% c("cumulative", "targets"),
         fundingagency != "DEDUP") %>%
  group_by(fundingagency, snu1, indicator, period, period_type) %>%
  summarise_at(vars(val), sum, na.rm = T) %>%
  ungroup() %>%
  pivot_wider(names_from = period_type, values_from = val) %>%
  mutate(achieve = cumulative / targets * 100,
         label_results = if_else(is.na(cumulative),
                                 paste0(snu1, " (NA)"),
                                 paste0(snu1, " (", comma(cumulative, 1), ")")),
         label_achieve = if_else(is.na(achieve),
                                 paste0(snu1, " (NA)"),
                                 paste0(snu1, " (", round(achieve), "%)")))

df_pmtct %>% glimpse()

df_pmtct %>% View()

# export for SI
df_pmtct_export <- df_msd %>%
  clean_agency() %>%
  filter(indicator %in% inds,
         standardizeddisaggregate == "Total Numerator",
         #period_type %in% c("cumulative", "targets"),
         fundingagency != "DEDUP") %>%
  group_by(fundingagency, snu1, indicator, period, period_type) %>%
  summarise_at(vars(val), sum, na.rm = T) %>%
  ungroup() %>%
  mutate(rep_period = paste0(period, "_", period_type)) %>%
  pivot_wider(id_cols = fundingagency:indicator,
              names_from = rep_period, values_from = val) %>%
  relocate(FY19_cumulative, .before = FY19_targets) %>%
  relocate(FY19Q3_results, .before = FY19Q4_results)

write_csv(x = df_pmtct_export,
          file = file.path(dataout,
                           paste0(country,
                                  " - FY18to20 PMTCT data tbl - ",
                                  format(Sys.Date(), "%Y%m%d"),
                                  ".csv")), na = "")


df_pmtct %>%
  filter(fundingagency == "CDC",
         period != "FY21",
         #period != "Q4FY20",
         is.na(targets)) %>%
  distinct(indicator)

# COUNTRY - PMTCT
df_cntry_pmtct <- df_msd %>%
  clean_agency() %>%
  filter(indicator %in% inds,
         standardizeddisaggregate == "Total Numerator",
         period_type %in% c("cumulative", "targets"),
         fundingagency != "DEDUP") %>%
  group_by(fundingagency, indicator, period, period_type) %>%
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

df_cntry_pmtct %>% glimpse()


# VIZ ----

# By Agency

agency <- "USAID"
agency <- "CDC"

# % Ach
df_pmtct %>%
  filter(fundingagency == agency,
         period != "FY21",
         !is.na(targets)) %>%
  distinct(indicator) %>%
  pull(indicator) %>%
  map(.x, .f = ~ plot_pmtct_ach(agency = agency, ind = .x))

# Results
df_pmtct %>%
  filter(fundingagency == agency,
         period != "FY21",
         is.na(targets)) %>%
  distinct(indicator) %>%
  pull(indicator) %>%
  map(.x, .f = ~ plot_pmtct_rst(agency = agency, ind = .x))


## CNTRY
df_pmtct %>%
  filter(indicator == "PMTCT_EID",
         period %in% c("FY20", "FY20Q4")) %>% #view()
  ggplot(aes(reorder_within(snu1, results, period),
             cumulative,
             fill = results,
             label = results)) +
  geom_col(show.legend = F) +
  geom_text(size = 3, color = usaid_darkgrey, hjust = 0) +
  scale_fill_si(palette = "scooters", discrete = F, reverse = T) +
  scale_y_continuous(position = "right", labels = comma) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~period, scales = "free") +
  labs(x = "", y = "",
       title = "USAID | PMTCT_FO Results (FY18 - FY20)",
       caption = paste0("Data source: MSD FY20Q4c, OHA/SIEI - Produced on ", format(Sys.Date(), "%Y-%m-%d"),
                        "\nNote: bars represent targets, results appended to state names, and achievements used as labels")) +
  si_style_xgrid() +
  theme(strip.placement = "outsite")

ggsave(file.path(graphics, paste0("Nigeria - USAID - PMTCT_ART Historical Achievements - ", format(Sys.Date(), "%Y-%m-%d"), ".png")),
       plot = last_plot(), scale = 1.2, dpi = 310,
       width = 10, height = 7, units = "in")




