##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: KP Testing Results
##  LICENCE: MIT
##  DATE:    2021-05-20

# DEPENDENCIES ----

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(glitr)
  library(glamr)
  library(gisr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(tidytext)
  library(glue)
  library(here)
  library(ICPIutilities)
  library(gt)

# SETUP ----

  # Directories
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"

  dir_merdata <- si_path("path_msd")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_.*_\\d{8}_v\\d{1}_1_N.*.zip$")

  file_genie <- dir_data %>%
    file.path("Genie") %>%
    return_latest(pattern = "HTS_.*0521.txt$")

# LOAD DATA ----

  ## FY21 Targets
  df_psnu <- file_psnu_im %>% read_msd()
  #df_psnu <- file_genie %>% read_msd()

  df_psnu <- df_psnu %>% clean_agency()

  df_psnu %>% glimpse()


# MUNGING

  df_psnu %>%
    distinct(fundingagency)

  df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE),
           str_detect(indicator, "^HTS_.*"),
           str_detect(str_to_lower(standardizeddisaggregate), "keypop")
           ) %>%
    distinct(indicator, standardizeddisaggregate,
             categoryoptioncomboname, otherdisaggregate, statushiv) %>%
    arrange(indicator, standardizeddisaggregate) %>%
    gt()

  df_psnu %>%
    filter(fiscal_year == 2021,
           indicator == "HTS_TST_POS",
           str_detect(str_to_lower(standardizeddisaggregate), "keypop")) %>%
    distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>%
    arrange(indicator, standardizeddisaggregate) %>%
    gt()

  df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(indicator, "^HTS_.*"),
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE)
    ) %>%
    group_by(fiscal_year, fundingagency, snu1uid, snu1, indicator, otherdisaggregate) %>%
    summarise(across(targets:cumulative, sum, na.rm = TRUE)) %>%
    ungroup() %>%
    view()

  # Period
  pd = "qtr1"
  #pd = "qtr2"
  #pd = "cumulative"

  # HTS_TST OU Achievement
  df_hts_keypop_ou <- df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE),
           indicator == "HTS_TST_POS",
           standardizeddisaggregate == "KeyPop/Result") %>%
    group_by(fundingagency) %>%
    summarise(across(c(targets, {{pd}}), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achieve = !!sym(pd) / targets) %>%
    group_by() %>%
    mutate(ou_achieve = sum(!!sym(pd), na.rm = TRUE) / sum(targets, na.rm = TRUE)) %>%
    ungroup()

  ou_results <- df_hts_keypop_ou %>%
    pull(!!sym(pd)) %>%
    sum() %>%
    comma()

  ou_targets <- df_hts_keypop_ou %>%
    pull(targets) %>%
    sum() %>%
    comma()

  ou_achieve <- df_hts_keypop_ou %>%
    pull(ou_achieve) %>%
    first() %>%
    percent(1)

  usaid_achieve <- df_hts_keypop_ou %>%
    filter(fundingagency == "USAID") %>%
    pull(achieve) %>%
    percent(1)

  cdc_achieve <- df_hts_keypop_ou %>%
    filter(fundingagency == "CDC") %>%
    pull(achieve) %>%
    percent(1)

  dod_achieve <- df_hts_keypop_ou %>%
    filter(fundingagency == "DOD") %>%
    pull(achieve) %>%
    percent(1)

  # HTS_TST Achievements by states
  df_hts_keypop <- df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE),
           fundingagency != "DOD",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate == "KeyPop/Result") %>%
    mutate(otherdisaggregate = case_when(
      str_detect(otherdisaggregate, "prisons") ~ "Prisons/E. Settings",
      TRUE ~ otherdisaggregate
    )) %>%
    group_by(fundingagency, snu1uid, snu1) %>%
    summarise(across(c(targets, {{pd}}), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achieve = !!sym(pd) / targets,
           snu1 = paste0(snu1, " (", percent(achieve, 1), ")\nR=", comma(!!sym(pd), 1), " T=", comma(targets, 1)))

  # HTS_TST Achievements by states / groups
  df_hts_keypop_groups <- df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE),
           fundingagency != "DOD",
           indicator == "HTS_TST_POS",
           standardizeddisaggregate == "KeyPop/Result") %>%
    mutate(otherdisaggregate = case_when(
      str_detect(otherdisaggregate, "prisons") ~ "Prisons/E. Settings",
      TRUE ~ otherdisaggregate
    )) %>%
    group_by(fundingagency, snu1uid, snu1, otherdisaggregate) %>%
    summarise(across(c(targets, !!sym(pd)), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achieve = !!sym(pd) / targets,
           snu1 = paste0(snu1,
                         " (", percent(achieve, 1),
                         ")\nR=", comma(!!sym(pd), 1),
                         " T=", comma(targets, 1)))

# VIZ

  # title pd variable
  t_pd <- "Q1"
  #t_pd <- "Q2"
  #t_pd <- "Cummulative"

  # State achieve
  ggplot(data = df_hts_keypop,
         aes(x = reorder_within(snu1, achieve, fundingagency),
             y = achieve, fill = fundingagency)) +
    geom_col(show.legend = F) +
    geom_hline(yintercept = 0, color = grey50k) +
    scale_x_reordered() +
    scale_y_continuous(labels = percent, position = "right") +
    scale_fill_manual(values = c(usaid_lightblue, usaid_blue)) +
    coord_flip() +
    facet_wrap(~fundingagency, scales = "free") +
    labs(x = "", y = "",
         title = glue("NIGERIA - FY21{t_pd} - HTS_TST_POS KeyPop Achievement"),
         subtitle = glue("Country achievement is <span style='color:red'>{ou_achieve}</span> ({ou_results} / {ou_targets}) with <span style='color:#a7c6ed'>CDC = {cdc_achieve}</span>, <span style='color:#6c6463'>DOD = {dod_achieve}</span>, <span style='color:#002a6c'>USAID = {usaid_achieve}</span>"),
         caption = glue("OHA/SIEI - FY21Q2i MSD, {Sys.Date()}\nHTS_TST_POS achievement for KeyPop")) +
    si_style_nolines() +
    theme(axis.ticks.x = element_line(color = grey50k),
          plot.subtitle = element_markdown(face = "bold"),
          strip.placement = "outside",
          strip.text = element_text(face = "bold"))

  ggsave(here(dir_graphics,
              paste0(glue("Nigeria - FY21{t_pd} - HTS_TST_POS KeyPop Achivement by State - "),
                     Sys.Date(),
                     ".png")),
         plot = last_plot(),
         scale = 1.2,
         dpi = 310,
         width = 10,
         height = 7,
         units = "in")

  # State achieve
  ggplot(data = df_hts_keypop_groups,
         aes(x = reorder_within(snu1, achieve, fundingagency),
             y = achieve, fill = fundingagency)) +
    geom_col(show.legend = F) +
    geom_hline(yintercept = 0, color = grey50k) +
    scale_x_reordered() +
    scale_y_continuous(labels = percent) +
    scale_fill_manual(values = c(usaid_lightblue, usaid_blue)) +
    coord_flip() +
    facet_wrap(fundingagency ~ otherdisaggregate,
               nrow = 2, scales = "free") +
    labs(x = "", y = "",
         title = glue("NIGERIA - FY21{t_pd} - HTS_TST_POS KeyPop Achievement"),
         subtitle = glue("Country achievement is <span style='color:red'>{ou_achieve}</span> ({ou_results} / {ou_targets}) with <span style='color:#a7c6ed'>CDC = {cdc_achieve}</span>, <span style='color:#6c6463'>DOD = {dod_achieve}</span>, <span style='color:#002a6c'>USAID = {usaid_achieve}</span>"),
         caption = glue("OHA/SIEI - FY21Q1c MSD, {Sys.Date()}\nHTS_TST_POS achievement for KeyPop")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          plot.subtitle = element_markdown(face = "bold"))

  ggsave(here(dir_graphics,
              paste0(glue("Nigeria - FY21{t_pd} - HTS_TST_POS KeyPop Achivement by State and KeyPop Groups - "),
                     Sys.Date(),
                     ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")



