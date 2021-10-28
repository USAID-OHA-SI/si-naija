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

  df_psnu <- glamr::return_latest(
      folderpath = dir_merdata,
      pattern = "PSNU_IM_.*_Nigeria"
    ) %>%
    gophr::read_msd()

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
                 unpack = NA,
                 #unpack = "ml",
                 #unpack = "iit",
                 fundingagency)%>%
    filter(fundingagency == "USAID") %>%
    tx_ml_colors()

  df_tx_ml %>% tx_ml_bars(fundingagency)

  # USAID Data
  df_usaid <- df_psnu %>%
    clean_agency() %>%
    filter(fundingagency == "USAID")

  # Plot OU/USAID TX_NET_NEW
  tx_options %>%
    map(function(.x){
      print(.x)

      df_ml <- df_usaid %>%
        tx_nocontact(rep_pd = curr_pd,
                     unpack = .x,
                     fundingagency) %>%
        tx_ml_colors()

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
        tx_ml_bars(fundingagency) +
        labs(title = glue("TX_NET_NEW WATERFALL ANALYSIS (USAID)"),
             subtitle = glue("In {curr_pd}, **TX_CURR** went from **{comma(first(currs))}** to **{comma(last(currs))}**, with a **NET NEW** of **{comma(nn)}**<br/>The agency was able to gain **{comma(new)}** and retain **{comma(rtt)}**, but end up loosing **{comma(ml)}**")) +
        theme(strip.text = element_blank(),
              #plot.margin = margin(l = 0, unit = "cm"),
              plot.title.position = "plot",
              plot.title = element_markdown(margin = margin(l = 20, b = 6)),
              plot.subtitle = element_markdown(margin = margin(l = 20, b = 10)))

      print(viz)
      #ggsave()

      si_save(
        filename = file.path(
          dir_graphics,
          glue("{curr_pd} - {str_to_upper(cntry)} - TX_NET_NEW Waterfall - {ifelse(is.na(.x), 'ALL ML', str_to_upper(.x))} - {format(Sys.Date(), '%Y%m%d')}.png")),
        plot = viz,
        #width = 7,
        #height = 7,
        scale = 1.4)
    })

