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

# LOAD DATA ----

  ## FY21 Targets
  df_psnu <- file_psnu_im %>%
    read_msd() %>%
    clean_agency()

  df_psnu %>% glimpse()


# MUNGING

  df_psnu %>%
    distinct(fundingagency)

  df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE),
           str_detect(indicator, "^HTS_TST.*"),
           str_detect(str_to_lower(standardizeddisaggregate), "keypop")
           ) %>%
    distinct(indicator, standardizeddisaggregate,
             categoryoptioncomboname, otherdisaggregate, statushiv) %>%
    arrange(indicator, standardizeddisaggregate) %>%
    gt()

  df_psnu %>%
    filter(fiscal_year == 2021,
           indicator == "HTS_TST",
           str_detect(str_to_lower(standardizeddisaggregate), "keypop")
    ) %>%
    distinct(indicator, standardizeddisaggregate, otherdisaggregate, statushiv) %>%
    arrange(indicator, standardizeddisaggregate) %>%
    gt()

  df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE),
           indicator == "HTS_TST",
           standardizeddisaggregate == "KeyPop/Result") %>%
    group_by(fundingagency, indicator, statushiv, otherdisaggregate) %>%
    summarise(across(c(targets, {{pd}}), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    view()

  # Period
  #pd = "qtr1"
  #pd = "qtr2"
  pd = "cumulative"

  # HTS_TST OU Achievement
  df_hts_ou <- df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE),
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "KeyPop/Result") %>%
    group_by(fundingagency, indicator) %>%
    summarise(across(c(targets:cumulative), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_longer(cols = qtr1:cumulative,
                 names_to = "metrics",
                 values_to = "results") %>%
    rowwise() %>%
    mutate(achieve = results / targets) %>%
    ungroup() %>%
    group_by(fundingagency, metrics) %>%
    mutate(
      yield = results[indicator == 'HTS_TST_POS'] / results[indicator == 'HTS_TST']
    ) %>%
    ungroup() %>%
    group_by(indicator, metrics) %>%
    mutate(
      ou_results = sum(results),
      ou_targets = sum(targets),
      ou_achieve = ou_results / ou_targets
    ) %>%
    group_by(metrics) %>%
    mutate(
      ou_pos = sum(results[indicator == 'HTS_TST_POS']),
      ou_yield = sum(results[indicator == 'HTS_TST_POS']) / sum(results[indicator == 'HTS_TST'])
    ) %>%
    ungroup()

  # Nigeria
  ou_results <- df_hts_ou %>%
    filter(indicator == "HTS_TST", metrics == pd) %>%
    pull(ou_results) %>%
    first() %>%
    comma()

  ou_targets <- df_hts_ou %>%
    filter(indicator == "HTS_TST", metrics == pd) %>%
    pull(ou_targets) %>%
    first() %>%
    comma()

  ou_achieve <- df_hts_ou %>%
    filter(indicator == "HTS_TST", metrics == pd) %>%
    pull(ou_achieve) %>%
    first() %>%
    percent(1)

  ou_pos <- df_hts_ou %>%
    filter(indicator == "HTS_TST", metrics == pd) %>%
    pull(ou_pos) %>%
    first() %>%
    comma(1)

  ou_yield <- df_hts_ou %>%
    filter(indicator == "HTS_TST", metrics == pd) %>%
    pull(ou_yield) %>%
    first() %>%
    percent(1)

  # USAID
  usaid_achieve <- df_hts_ou %>%
    filter(fundingagency == "USAID",
           indicator == "HTS_TST",
           metrics == pd) %>%
    pull(achieve) %>%
    percent(1)

  usaid_yield <- df_hts_ou %>%
    filter(fundingagency == "USAID",
           indicator == "HTS_TST",
           metrics == pd) %>%
    pull(yield) %>%
    percent(1)

  # CDC
  cdc_achieve <- df_hts_ou %>%
    filter(fundingagency == "CDC",
           indicator == "HTS_TST",
           metrics == pd) %>%
    pull(achieve) %>%
    percent(1)

  cdc_yield <- df_hts_ou %>%
    filter(fundingagency == "CDC",
           indicator == "HTS_TST",
           metrics == pd) %>%
    pull(yield) %>%
    percent(1)

  # DOD
  dod_achieve <- df_hts_ou %>%
    filter(fundingagency == "DOD",
           indicator == "HTS_TST",
           metrics == pd) %>%
    pull(achieve) %>%
    percent(1)

  dod_yield <- df_hts_ou %>%
    filter(fundingagency == "DOD",
           indicator == "HTS_TST",
           metrics == pd) %>%
    pull(yield) %>%
    percent(1)

  # HTS_TST Achievements by states
  df_hts_keypop <- df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE),
           fundingagency != "DOD",
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "KeyPop/Result") %>%
    mutate(otherdisaggregate = case_when(
      str_detect(otherdisaggregate, "prisons") ~ "Prisons/E. Settings",
      TRUE ~ otherdisaggregate
    )) %>%
    group_by(fundingagency, indicator, snu1uid, snu1) %>%
    summarise(across(c(targets:cumulative), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_longer(cols = qtr1:cumulative,
                 names_to = "metrics",
                 values_to = "results") %>%
    rowwise() %>%
    mutate(achieve = results / targets) %>%
    ungroup() %>%
    group_by(fundingagency, snu1uid, snu1, metrics) %>%
    mutate(
      pos = results[indicator == 'HTS_TST_POS'],
      yield = results[indicator == 'HTS_TST_POS'] / results[indicator == 'HTS_TST']
    ) %>%
    ungroup() %>%
    filter(!metrics %in% c("qtr3", "qtr4"),
           indicator == 'HTS_TST') %>%
    mutate(
      #snu1 = paste0(snu1, " (", percent(achieve, 1), ") \nT=", comma(targets, 1), " R=", comma(results, 1), "\nPositive=", comma(pos, 1)),
      #snu1 = paste0(snu1, " (", percent(yield, 1), ") \nT=", comma(targets, 1), " R=", comma(results, 1), "\nPositive=", comma(pos, 1))
      snu1 = paste0(snu1, " (", percent(achieve, 1), ") \nT=", comma(targets, 1), " R=", comma(results, 1), "\nY=", percent(yield, 1), " Positive=", comma(pos, 1))
    )



  # HTS_TST Achievements by states / groups
  df_hts_keypop_groups <- df_psnu %>%
    filter(fiscal_year == 2021,
           str_detect(str_to_lower(fundingagency), "dedup", negate = TRUE),
           fundingagency != "DOD",
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "KeyPop/Result") %>%
    mutate(otherdisaggregate = case_when(
      str_detect(otherdisaggregate, "prisons") ~ "Prisons/E. Settings",
      TRUE ~ otherdisaggregate
    )) %>%
    group_by(fundingagency, indicator, snu1uid, snu1, otherdisaggregate) %>%
    summarise(across(c(targets:cumulative), sum, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_longer(cols = qtr1:cumulative,
                 names_to = "metrics",
                 values_to = "results") %>%
    rowwise() %>%
    mutate(achieve = results / targets) %>%
    ungroup() %>% #view()
    group_by(fundingagency, snu1uid, snu1, otherdisaggregate, metrics) %>%
    mutate(
      n_ind = n_distinct(indicator)
    ) %>%
    filter(n_ind > 1) %>%
    mutate(
      pos = results[indicator == 'HTS_TST_POS'],
      yield = results[indicator == 'HTS_TST_POS'] / results[indicator == 'HTS_TST']
    ) %>%
    ungroup() %>%
    filter(!metrics %in% c("qtr3", "qtr4"),
           indicator == 'HTS_TST') %>%
    mutate(
      #snu1 = paste0(snu1, " (", percent(achieve, 1), ") \nT=", comma(targets, 1), " R=", comma(results, 1), "\nPositive=", comma(pos, 1)),
      #snu1 = paste0(snu1, " (", percent(yield, 1), ") \nT=", comma(targets, 1), " R=", comma(results, 1), "\nPositive=", comma(pos, 1))
      #snu1 = paste0(snu1, " (", percent(achieve, 1), ") \nT=", comma(targets, 1), " R=", comma(results, 1), "\nY=", percent(yield, 1), " Positive=", comma(pos, 1))
      snu1 = paste0(snu1, " (", percent(achieve, 1), "\nY=", percent(yield, 1), " Positive=", comma(pos, 1)))


# VIZ

  # title pd variable
  #t_pd <- "Q1"
  t_pd <- "Q2"
  #t_pd <- "Cummulative"

  # State achieve
  ggplot(
    #data = df_hts_keypop %>% filter(metrics == "qtr1"),
    data = df_hts_keypop %>% filter(metrics == "cumulative"),
    aes(x = reorder_within(snu1, achieve, fundingagency),
        y = achieve, fill = fundingagency, group = metrics,
        label = percent(achieve, 1))) +
    geom_col(show.legend = F) +
    geom_hline(yintercept = 0, color = grey50k) +
    geom_text(color = grey70k, hjust = 0, nudge_y = 0) +
    scale_x_reordered() +
    scale_y_continuous(labels = percent, position = "right") +
    scale_fill_manual(values = c(usaid_lightblue, usaid_blue)) +
    expand_limits(y = 5) +
    coord_flip() +
    facet_wrap(~fundingagency, scales = "free") +
    labs(x = "", y = "",
         title = glue("NIGERIA - FY21{t_pd} - HTS_TST KeyPop Achievement"),
         subtitle = glue("Country achievement is <span style='color:red'>{ou_achieve}</span> ({ou_results} / {ou_targets}) with <span style='color:#a7c6ed'>CDC = {cdc_achieve}</span>, <span style='color:#6c6463'>DOD = {dod_achieve}</span>, <span style='color:#002a6c'>USAID = {usaid_achieve}</span>",
                         "<br>Yield is <span style='color:red'>{ou_yield}</span> ({ou_pos} / {ou_results}) with <span style='color:#a7c6ed'>CDC = {cdc_yield}</span>, <span style='color:#6c6463'>DOD = {dod_yield}</span>, <span style='color:#002a6c'>USAID = {usaid_yield}</span>",
                         "<br>States are sorted by HTS_TST % Achievements. T = Targets, R = Results, Y = Yield"),
         caption = glue("OHA/SIEI - FY21Q2i MSD, {Sys.Date()}\nHTS_TST Achievement & Yield for KeyPop\nAchievement = Results / Targets\nYield = Result (HTS_TST_POS) / Results (HTS_TST)")) +
    si_style_nolines() +
    theme(
      axis.text.x = element_blank(),
      plot.subtitle = element_markdown(face = "bold"),
      strip.placement = "outside",
      strip.text = element_text(face = "bold")
    )

  ggsave(here(dir_graphics,
              paste0(glue("Nigeria - FY21{t_pd} - HTS_TST KeyPop Achivement by State - "),
                     Sys.Date(),
                     ".png")),
         plot = last_plot(),
         scale = 1.2,
         dpi = 310,
         width = 10,
         height = 7,
         units = "in")

  # State achieve
  ggplot(
    #data = df_hts_keypop_groups %>% filter(metrics == "qtr1"),
    data = df_hts_keypop_groups %>% filter(metrics == "cumulative"),
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
         title = glue("NIGERIA - FY21{t_pd} - HTS_TST KeyPop Achievement"),
         subtitle = glue("Country achievement is <span style='color:red'>{ou_achieve}</span> ({ou_results} / {ou_targets}) with <span style='color:#a7c6ed'>CDC = {cdc_achieve}</span>, <span style='color:#6c6463'>DOD = {dod_achieve}</span>, <span style='color:#002a6c'>USAID = {usaid_achieve}</span>",
                         "<br>Yield is <span style='color:red'>{ou_yield}</span> ({ou_pos} / {ou_results}) with <span style='color:#a7c6ed'>CDC = {cdc_yield}</span>, <span style='color:#6c6463'>DOD = {dod_yield}</span>, <span style='color:#002a6c'>USAID = {usaid_yield}</span>",
                         "<br>States are sorted by HTS_TST % Achievements. T = Targets, R = Results, Y = Yield"),
         caption = glue("OHA/SIEI - FY21Q1c MSD, {Sys.Date()}\nHTS_TST_POS achievement for KeyPop\nAchievement = Results / Targets\nYield = Result (HTS_TST_POS) / Results (HTS_TST)")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          plot.subtitle = element_markdown(face = "bold"))

  ggsave(here(dir_graphics,
              paste0(glue("Nigeria - FY21{t_pd} - HTS_TST KeyPop Achivement by State and KeyPop Groups - "),
                     Sys.Date(),
                     ".png")),
         plot = last_plot(), scale = 1.2, dpi = 310,
         width = 10, height = 7, units = "in")



