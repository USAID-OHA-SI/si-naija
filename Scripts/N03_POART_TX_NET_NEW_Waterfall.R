##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Utility functions
##  LICENCE: MIT
##  DATE:    2021-10-07
##

## Libraries ----

  library(tidyverse)
  library(gophr)
  library(glamr)
  library(glitr)
  library(ggtext)
  library(scales)
  library(extrafont)
  library(glue)

  source("./Scripts/N00_Utilities.R")
  source("./Scripts/N00_Viz_Utilities.R")

## GLOBALS ----

  dir_merdata <- glamr::si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"

  cntry <- "Nigeria"

## DATA ----

  # MSDs

  # df_sites <- glamr::return_latest(
  #   folderpath = glamr::si_path(),
  #   pattern = "Site_IM_.*_Nigeria") %>%
  #   gophr::read_msd()

  # OU - Nigeria
  df_psnu <- glamr::return_latest(
      folderpath = dir_merdata,
      pattern = "PSNU_IM_.*_Nigeria") %>%
    read_msd()

  # Global
  df_global <- glamr::return_latest(
    folderpath = dir_merdata,
    pattern = "OU_IM_FY20-23_.*_1.zip$") %>%
    read_msd()

## Fiscal Years ----

  prev_fy <- df_psnu %>% identifypd(pd_type = "year", pd_prior = T)
  curr_fy <- df_psnu %>% identifypd(pd_type = "year", pd_prior = F)

  curr_pd <- df_psnu %>% identifypd()
  prev_pd <- df_psnu %>% identifypd(pd_type = "full", pd_prior = T)

  hist_pds <- df_psnu %>% identify_pds(pd_end = curr_pd, len = 2)

  ## TX Indicators

  tx_inds <- c(
    'TX_CURR',
    'TX_NEW',
    'TX_RTT',
    'TX_ML',
    'TX_NET_NEW')

  df_psnu <- df_psnu %>%
    filter(indicator %in% tx_inds)

  df_global <- df_global %>%
    filter(indicator %in% tx_inds,
           funding_agency == "USAID")

  ## TX_ML - Other Disaggs Categories
  ##
  # 1 No Contact Outcome - Died
  # 2 No Contact Outcome - Interruption in Treatment <3 Mon~
  # 3 No Contact Outcome - Interruption in Treatment 3+ Mon~
  # 4 No Contact Outcome - Refused Stopped Treatment
  # 5 No Contact Outcome - Transferred Out


# VIZ ----

  tx_options <- c(
    NA,    # Group all TX_ML options
    "ml",  # Group ITT <3 & 3+
    "iit"  # Unpack all TX_ML Disaggs
  )

  # Test
  df_tx_ml <- df_psnu %>%
    clean_agency() %>%
    tx_nocontact(rep_pd = curr_pd,
                 #unpack = NA,
                 #unpack = "ml",
                 unpack = "iit",
                 funding_agency) %>%
    filter(funding_agency == "USAID") %>%
    tx_ml_colors()

  df_tx_ml <- df_psnu %>%
    clean_agency() %>%
    tx_nocontact(rep_pd = curr_pd,
                 #unpack = NA,
                 #unpack = "ml",
                 unpack = "iit") %>%
    tx_ml_colors()

  df_tx_ml %>%
    tx_ml_bars(lsize = 5) +
    theme(text = element_text(face = "bold"),
          plot.margin = unit(c(0,0,0,0), "in"),
          strip.text = element_blank(),
          #axis.text.x = element_blank()
          ) #+
    theme_transparent()

  # Test - for ScoreCard
  df_tx_ml_states <- df_psnu %>%
    clean_agency() %>%
    tx_nocontact(rep_pd = curr_pd,
                 unpack = NA,
                 #unpack = "ml",
                 #unpack = "iit",
                 funding_agency, psnu)%>%
    filter(funding_agency == "USAID") %>%
    tx_ml_colors()

  df_tx_ml_states %>%
    mutate(label2_value = case_when(
      indicator == "TX_CURR" ~ paste0(comma(value), "\n", comma(value[indicator=="TX_NET_NEW"]), " NN"),
      TRUE ~ comma(value)
    ))

  viz_tx_ml <- df_tx_ml_states %>%
    filter(psnu == "Akwa Ibom") %>%
    tx_ml_bars(lsize = 10, psnu) +
    scale_y_continuous(expand = c(.1, .1)) +
    theme(text = element_text(face = "bold"),
          plot.margin = unit(c(0,0,0,0), "in"),
          strip.text = element_blank(),
          axis.text.x = element_blank(),
          ) +
    theme_transparent()

  ggsave("./Graphics/ScoreCard_TX_ML_IkwaIbom.png",
         plot = viz_tx_ml,
         width = 3.5, height = 1.4, units = "in", dpi = 400,
         bg = "transparent")


  # USAID/Nigeria Data
  df_usaid <- df_psnu %>%
    clean_agency() %>%
    filter(funding_agency == "USAID")

  # Plot OU/USAID TX_NET_NEW
  tx_options %>%
    map(function(.x){

      print(.x)

      df_ml <- df_usaid %>%
        tx_nocontact(.data = .,
                     rep_pd = curr_pd,
                     unpack = .x,
                     funding_agency) %>%
        tx_ml_colors()

      view(df_ml)

      currs <- df_ml %>%
        filter(indicator %in% c("TX_CURR_LAG1", "TX_CURR")) %>%
        pull(value)

      nn <- df_ml %>%
        filter(indicator == "TX_NET_NEW") %>%
        pull(value)

      new <- df_ml %>%
        filter(indicator == "TX_NEW") %>%
        pull(value)

      rtt <- df_ml %>%
        filter(indicator == "TX_RTT") %>%
        pull(value)

      ml <- df_ml %>%
        filter(str_detect(indicator, "TX_ML")) %>%
        pull(value) %>%
        sum()

      viz <- df_ml %>%
        tx_ml_bars(.data = .,
                   lsize = 6,
                   funding_agency) +
        labs(title = glue("TX_NET_NEW WATERFALL ANALYSIS (USAID)"),
             subtitle = glue("In {curr_pd}, **TX_CURR** went from **{comma(first(currs))}** to **{comma(last(currs))}**, with a **NET NEW** of **{comma(nn)}**<br/>The agency was able to gain **{comma(new)}** and retain **{comma(rtt)}**, while loosing **{comma(ml)}**")) +
        theme(strip.text = element_blank(),
              #plot.margin = margin(l = 0, unit = "cm"),
              plot.title.position = "plot",
              plot.title = element_markdown(margin = margin(l = 20, b = 6)),
              plot.subtitle = element_markdown(margin = margin(l = 20, b = 10)))

      print(viz)

      si_save(
        filename = file.path(
          dir_graphics,
          glue("{curr_pd} - {str_to_upper(cntry)} - TX_NET_NEW Waterfall - {ifelse(is.na(.x), 'ALL ML', str_to_upper(.x))} - {format(Sys.Date(), '%Y%m%d')}.png")),
        plot = viz,
        #width = 7,
        #height = 7,
        scale = 1.4)
    })


  # USAID/Global Data

  df_global %>%
    tx_nocontact(.data = .,
                 rep_pd = curr_pd,
                 unpack = NA) %>%
    tx_ml_colors()

  df_global %>%
    tx_nocontact(.data = .,
                 rep_pd = curr_pd,
                 unpack = "ml") %>%
    tx_ml_colors()

  df_global %>%
    tx_nocontact(.data = .,
                 rep_pd = curr_pd,
                 unpack = "iit") %>%
    tx_ml_colors()



  # Initial Summary
  df_global %>%
    filter(indicator %in% inds[inds != "TX_ML"] &
             standardizeddisaggregate == "Total Numerator" |
             indicator == "TX_ML" &
             standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus") %>%
    reshape_msd() %>%
    filter(period == curr_pd | (period == prev_pd & indicator == "TX_CURR")) %>%
    mutate(
      indicator = case_when(
        indicator == "TX_CURR" & period == prev_pd ~ "TX_CURR_LAG1",
        TRUE ~ indicator)) %>%
    select(-c(period, period_type)) %>%
    group_by_at(all_of(cols_tx_grp)) %>%
    summarise(across(all_of("value"), sum, na.rm = TRUE), .groups = "drop")

  # Plot OU/USAID TX_NET_NEW
  tx_options %>%
    map(function(.x){

      print(.x)

      df_ml <- df_global %>%
        tx_nocontact(.data = .,
                     rep_pd = curr_pd,
                     unpack = .x) %>%
        tx_ml_colors()

      #view(df_ml)

      currs <- df_ml %>%
        filter(indicator %in% c("TX_CURR_LAG1", "TX_CURR")) %>%
        pull(value)

      nn <- df_ml %>%
        filter(indicator == "TX_NET_NEW") %>%
        pull(value)

      new <- df_ml %>%
        filter(indicator == "TX_NEW") %>%
        pull(value)

      rtt <- df_ml %>%
        filter(indicator == "TX_RTT") %>%
        pull(value)

      ml <- df_ml %>%
        filter(str_detect(indicator, "TX_ML")) %>%
        pull(value) %>%
        sum()

      viz <- df_ml %>%
        tx_ml_bars(.data = .,
                   lsize = 6) +
        labs(title = "", subtitle = "") +
        # labs(title = glue("TX_NET_NEW WATERFALL ANALYSIS (USAID)"),
        #      subtitle = glue("In {curr_pd}, **TX_CURR** went from **{comma(first(currs))}** to **{comma(last(currs))}**, with a **NET NEW** of **{comma(nn)}**<br/>The agency was able to gain **{comma(new)}** and retain **{comma(rtt)}**, while loosing **{comma(ml)}**")) +
        theme(strip.text = element_blank(),
              #plot.margin = margin(l = 0, unit = "cm"),
              plot.title.position = "plot",
              plot.title = element_markdown(margin = margin(l = 20, b = 6)),
              plot.subtitle = element_markdown(margin = margin(l = 20, b = 10)))

      print(viz)

      si_save(
        filename = file.path(
          dir_graphics,
          glue("{curr_pd} - USAID - TX_NET_NEW Waterfall - {ifelse(is.na(.x), 'ALL ML', str_to_upper(.x))} - {format(Sys.Date(), '%Y%m%d')}.png")),
        plot = viz,
        #width = 7,
        #height = 7,
        scale = 1.4)
    })


