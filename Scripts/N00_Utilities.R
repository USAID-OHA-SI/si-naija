##
##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Utility functions
##  LICENCE: MIT
##  DATE:    2021-07-12
##

## Libraries ----
  library(tidyverse)
  library(gophr)
  library(glamr)
  library(glitr)
  library(scales)

## FUNCTIONS ----

#' @title Summarise Indicators Values
#'
#' @param df
#' @param inds
#' @param disags
#' @param values
#' @param ...
#'
sum_indicator <- function(df,
                          inds = 'TX_CURR',
                          disags = 'Total Numerator',
                          sum_vars = 'cumulative',
                          ...) {

  # df %>%
  #   filter(indicator %in% inds,
  #          standardizeddisaggregate %in% disags) %>%
  #   group_by(...) %>%
  #   summarise(across(all_of(values), sum, na.rm = TRUE)) %>%
  #   ungroup()

  df %>%
    filter(indicator %in% inds,
           standardizeddisaggregate %in% disags) %>%
    sum_group(sum_vars = sum_vars, keep_group = FALSE, ...)
}

#' @title Sum Group Values
#'
#' @param df
#' @param sum_vars
#' @param ...
#'
sum_group <- function(df,
                      sum_vars = 'value',
                      keep_group = FALSE,
                      ...) {

  # Mutate
  if (keep_group == TRUE) {

    sum_rst <- paste0(sum_vars, "_ttl")

    df <- df %>%
      group_by(...) %>%
      mutate({{sum_rst}} := sum(!!sym(sum_vars), na.rm = TRUE)) %>%
      #mutate_at(.vars = vars(all_of(sum_rst)),.funs = ~ sum(!!sym(sum_vars), na.rm = TRUE)) %>%
      ungroup()

    return(df)
  }

  # Summarise
  df %>%
    group_by(...) %>%
    summarise(across(all_of(sum_vars), sum, na.rm = TRUE)) %>%
    ungroup()

}


#' @title TX_ML Disaggs
#'
tx_nocontact <- function(.data,
                         sum_var = 'cumulative',
                         ...) {

  .data %>%
    sum_indicator(inds = 'TX_ML',
                  disags = 'Age/Sex/ARTNoContactReason/HIVStatus',
                  sum_vars = sum_var, ...) %>%
    rename(indicator = otherdisaggregate,
           #value = {{sum_var}}
           ) %>%
    mutate(indicator = str_remove(indicator, "No Contact Outcome - "),
           indicator = str_replace(indicator, "Interruption in Treatment", "IIT"),
           indicator = str_replace(indicator, " Treatment", ""),
           indicator = case_when(
             indicator == 'Transferred Out' ~ 'TX_ML_XFRED_OUT',
             indicator == 'IIT <3 Months' ~ 'TX_ML_IIT_LT3M',
             indicator == 'IIT 3+ Months' ~ 'TX_ML_IIT_3MPLUS',
             indicator == 'Refused Stopped' ~ 'TX_ML_REF_STOPPED',
             indicator == 'Died' ~ 'TX_ML_DIED',
             TRUE ~ 'TX_ML_OTHER'
           ))
}


#' @title Clean Mechs
#'
clean_mechs <- function(.data) {

  .data %>%
    mutate(
      mech_name = case_when(
        # USAID
        mech_name == "Meeting Targets and Maintaining Epidemic Control (EpiC)" ~ "EpiC",
        mech_name == "STRENGHTENING INTERGRATED DELIVERY OF HIV/AIDS SERVICES(SIDHAS)" ~ "SIDHAS",
        mech_name == "SHARP Task Order 1" ~ "SHARP TO1",
        mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 2" ~ "SHARP TO2",
        mech_name == "Strategic HIV/AIDS Response Program (SHARP) Task Order 3" ~ "SHARP TO3",
        mech_name == "Reaching Impact, Saturation and Epidemic Control (RISE)" ~ "RISE",
        mech_name == "Care and Treatment in Sustained Support (CaTSS)" ~ "CaTSS",
        mech_name == "KP CARE 1" ~ "KP CARE 1",
        mech_name == "KP CARE 2" ~ "KP CARE 2",
        mech_name == "MSH - Prevention Organisation Systems AIDS Care and Treatment(MSH -ProACT)" ~ "ProACT",
        mech_name == "Integrated MARPs HIV Prevention Program (IMHIPP)" ~ "IMHIPP",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 1)" ~ "ICHSSA 1",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 2)" ~ "ICHSSA 2",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 3)" ~ "ICHSSA 3",
        mech_name == "Integrated Child Health and Social Services Award (ICHSSA 4)" ~ "ICHSSA 4",
        # CDC
        mech_name == "Partnering Effectively to end AIDS through Results and Learning (PEARL)_2097" ~ "PEARL",
        mech_name == "Global Action towards HIV Epidemic Control in Subnational units in Nigeria (4GATES PROJECT)_2100" ~ "4GATES",
        mech_name == "ACTION to Control HIV Epidemic through Evidence (ACHIEVE)_2099" ~ "ACHIEVE",
        mech_name == "Improving Comprehensive AIDS Response Enhanced for Sustainability (iCARES)_2098" ~ "iCARES",
        TRUE ~ mech_name
      )
    )
}


#' @title Clean Partners
#'
clean_partners <- function(.data) {

  .data %>%
    mutate(
      primepartner = case_when(
        # USAID
        primepartner == "Family Health International" ~ "FHI 360",
        primepartner == "Chemonics International, Inc." ~ "Chemonics",
        primepartner == "JHPIEGO CORPORATION" ~ "JHPIEGO",
        primepartner == "SOCIETY FOR FAMILY HEALTH" ~ "SFH",
        primepartner == "HEARTLAND ALLIANCE LTD-GTE" ~ "HEARTLAND ALLIANCE",
        primepartner == "Heartland Alliance International, LLC" ~ "Heartland Alliance",
        primepartner == "CENTER FOR CLINICAL CARE AND CLINICAL RESEARCH LTD GTE" ~ "C4C3R",
        primepartner == "ASSOCIATION FOR REPRODUCTIVE AND FAMILY HEALTH" ~ "A4RFH",
        primepartner == "PRO-HEALTH INTERNATIONAL" ~ "ProHI",
        primepartner == "Management Sciences For Health, Inc." ~ "MHS",
        # CDC
        primepartner == "APIN PUBLIC HEALTH INITIATIVES LTD/GTE" ~ "APHI",
        primepartner == "INSTITUTE OF HUMAN VIROLOGY" ~ "IHVN",
        primepartner == "CATHOLIC CARITAS FOUNDATION OF NIGERIA" ~ "CCFN",
        primepartner == "CENTRE FOR INTEGRATED HEALTH PROGRAMS" ~ "CIHP",
        TRUE ~ primepartner
      )
    )
}


#' @title Create labels for partners
#'
partners_label <- function(.data) {
  .data %>%
    clean_mechs() %>%
    clean_partners() %>%
    mutate(partner = paste0(primepartner, " - ", mech_name))
}


#' @title Identify MSD Consecutive Periods
#'
#'
identify_pds <- function(df_msd,
                         pd_end = "FY21Q3",
                         len = 6,
                         pds_type = "qtr") {

  # Current fiscal year
  curr_fy <- df_msd %>% identifypd(pd_type = "year")

  # All fiscal years (up to curr fy)
  curr_fys <- df_msd %>%
    distinct(fiscal_year) %>%
    arrange(desc(fiscal_year)) %>%
    pull()

  curr_fys <- curr_fys[curr_fys <= curr_fy]

  # Quarters
  qtrs <- 1:4 %>% paste0("Q", .)

  # All Periods
  curr_pds <- curr_fys %>%
    str_sub(3, 4) %>%
    paste0("FY", .) %>%
    map(function(.x) {
      .x %>%
        paste0(qtrs) %>%
        rev()
    }) %>% unlist()

  idx_end <- which(curr_pds == pd_end)
  idx_start = idx_end + (len - 1)

  pds <- curr_pds[idx_end:idx_start]

  if (pds_type == "year") {
    yrs <- pds %>%
      str_sub(1, 4) %>%
      str_replace("FY", "20") %>%
      unique() %>%
      as.integer()

    return(yrs)
  }

  return(pds)
}



## DATA ----

  df_psnu <- glamr::return_latest(
    folderpath = glamr::si_path(),
    pattern = "PSNU_IM_.*_Nigeria") %>%
  gophr::read_msd()

  df_psnu %>% glimpse()

  ## Fiscal Years
  prev_fy <- df_psnu %>% identifypd(pd_type = "year", pd_prior = T)
  curr_fy <- df_psnu %>% identifypd(pd_type = "year", pd_prior = F)

  curr_pd <- df_psnu %>% identifypd()
  prev_pd <- df_psnu %>% identifypd(pd_type = "full", pd_prior = T)

  hist_pds <- df_psnu %>% identify_pds(pd_end = curr_pd, len = 2)

  ## Indicators
  tx_inds <- c(
    'TX_CURR',
    'TX_NEW',
    'TX_RTT',
    'TX_ML',
    'TX_NET_NEW')

  df_psnu <- df_psnu %>%
    filter(indicator %in% tx_inds)

  ## TX_ML Categories

  df_psnu %>%
    filter(fiscal_year == curr_fy,
           indicator == 'TX_ML',
           standardizeddisaggregate %in%
             c('Total Numerator', 'Age/Sex/ARTNoContactReason/HIVStatus')) %>%
    distinct(otherdisaggregate) %>%
    arrange(otherdisaggregate)

  # 1 No Contact Outcome - Died
  # 2 No Contact Outcome - Interruption in Treatment <3 Mon~
  # 3 No Contact Outcome - Interruption in Treatment 3+ Mon~
  # 4 No Contact Outcome - Refused Stopped Treatment
  # 5 No Contact Outcome - Transferred Out
  # 6 NA

  tx_ind_levels1 <- c(
    'TX_CURR_LAG1',
    'TX_NEW',
    'TX_RTT',
    'TX_CURR_ADJ',
    'TX_ML',
    'TX_NET_NEW',
    'TX_CURR')

  tx_ind_levels2 <- c(
    'TX_CURR_LAG1',
    'TX_NEW',
    'TX_RTT',
    'TX_CURR_ADJ',
    'TX_ML',
    'TX_ML_XFRED_OUT',
    'TX_ML_IIT_LT3M',
    'TX_ML_ITT_3MPLUS',
    'TX_ML_REF_STOPPED',
    'TX_ML_DIED',
    'TX_NET_NEW',
    'TX_CURR')

  tx_ind_labels1 <- case_when(
      tx_ind_levels1 == "TX_CURR" ~ curr_pd,
      tx_ind_levels1 == "TX_CURR_LAG1" ~ prev_pd,
      tx_ind_levels1 == "TX_CURR_ADJ" ~ "???",
      TRUE ~ ""
    )

  tx_ind_labels2 <- case_when(
    tx_ind_levels2 == "TX_CURR" ~ curr_pd,
    tx_ind_levels2 == "TX_CURR_LAG1" ~ prev_pd,
    tx_ind_levels2 == "TX_CURR_ADJ" ~ "???",
    TRUE ~ ""
  )


  # Reshape TX_ML Disags
  df_psnu %>%
    filter(fiscal_year == curr_fy) %>%
    tx_nocontact(sum_var = 'cumulative',
                 fiscal_year, fundingagency,
                 operatingunit, otherdisaggregate)

  df_psnu %>%
    filter(fiscal_year == curr_fy) %>%
    tx_nocontact(sum_var = 'cumulative',
                 fiscal_year, fundingagency,
                 operatingunit, psnu, otherdisaggregate)


  # TX_CURR, NET_NEW, RTT & TX_ML

  df_tx <- df_psnu %>%
    filter(indicator %in% tx_inds) %>%
    sum_indicator(inds = tx_inds,
                  disags = 'Total Numerator',
                  sum_vars = paste0("qtr", 1:4),
                  fiscal_year, fundingagency, operatingunit, indicator) %>%
    reshape_msd() %>%
    select(-period_type) %>%
    filter(period == curr_pd | period == prev_pd & indicator == "TX_CURR") %>%
    mutate(
      indicator = case_when(
        indicator == "TX_CURR" & period == prev_pd ~ "TX_CURR_LAG1",
        TRUE ~ indicator),
      indicator = factor(indicator, levels = tx_ind_levels1)) %>%
    select(-period)

  df_tx <- df_tx %>%
    filter(indicator %in% tx_ind_levels1[1:3]) %>%
    group_by_at(vars(-c(indicator, value))) %>%
    summarise(across(value, sum, na.rm = T)) %>%
    ungroup() %>%
    mutate(indicator = "TX_CURR_ADJ") %>%
    bind_rows(df_tx, .) %>%
    mutate(indicator = factor(indicator, levels = tx_ind_levels1, ordered = T)) %>%
    group_by_at(vars(-c(indicator, value))) %>%
    mutate(
      index = row_number(),
      label_change = case_when(
        str_detect(indicator, "NEW|RTT|ML") ~ str_remove(indicator, "TX_"),
        TRUE ~ NA_character_
      ),
      label_tx = case_when(
        indicator == "TX_CURR" ~ curr_pd,
        indicator == "TX_CURR_LAG1" ~ prev_pd,
        indicator == "TX_CURR_ADJ" ~ "???",
        TRUE ~ NA_character_
      ))


  df_tx_viz <- df_tx %>%
    group_by_at(vars(-c(indicator, value, index, label_change, label_tx))) %>%
    arrange(indicator) %>%
    mutate(
      index = row_number(),
      ymin = case_when(
        str_detect(indicator, "TX_CURR") ~ 0,
        indicator == "TX_NEW" ~ lag(value, 1),
        indicator == "TX_RTT" ~ lag(value, 1) + value,
        indicator == "TX_NET_NEW" ~ first(value),
        str_detect(indicator, "TX_ML") ~ lag(value, 1) - value,
        TRUE ~ value
      ),
      ymax = case_when(
        str_detect(indicator, "TX_CURR") ~ value,
        indicator == "TX_NEW" ~ ymin + value,
        indicator == "TX_RTT" ~ ymin + value,
        indicator == "TX_NET_NEW" ~ ymin + value,
        str_detect(indicator, "TX_ML") ~ ymin + value,
        TRUE ~ value
      )) %>%
    ungroup()



# VIZ ----

  w <- .96/2

  df_tx_viz %>%
    filter(fundingagency != "DOD") %>%
    ggplot() +
    geom_rect(aes(xmin = as.integer(indicator) - w,
                  xmax = as.integer(indicator) + w,
                  ymin = ymin,
                  ymax = ymax)) +
    geom_text(aes(x = indicator, y = ymax, label = comma(value)), vjust = -1) +
    geom_text(aes(x = indicator, y = ymin, label = label_change), vjust = 1) +
    scale_x_discrete(limits = tx_ind_labels1) +
    facet_wrap(~fundingagency)



## OLD ----

  df_tx_waterfall <- df_tx %>%
    group_by(fiscal_year, fundingagency, operatingunit) %>%
    mutate(value_base = case_when(
      indicator == 'TX_CURR_LAG1' ~ 0,
      indicator == 'TX_CURR' ~ 0,
      indicator == 'TX_NEW' ~ value[indicator == 'TX_CURR_LAG1'],
      indicator == 'TX_RTT' ~ value[indicator == 'TX_CURR_LAG1'] + value[indicator == 'TX_NEW'],
      indicator == 'TX_ML' ~ (value[indicator == 'TX_CURR_LAG1'] + value[indicator == 'TX_NEW'] + value[indicator == 'TX_RTT']) - value[indicator == 'TX_ML'],
      #indicator == 'TX_NET_NEW' ~ value[indicator == 'TX_CURR_LAG1'],
      TRUE ~ 0
    )) %>%
    ungroup()

  df_tx_waterfall <- df_tx_waterfall %>%
    filter(indicator == 'TX_RTT') %>%
    rowwise() %>%
    mutate(indicator = 'TX_CURR_ADJ',
           value = value + value_base,
           value_base = 0) %>%
    ungroup() %>%
    bind_rows(df_tx_waterfall)

  df_tx_waterfall <- df_tx_waterfall %>%
    mutate(indicator = ordered(indicator, levels = ind_levels)) %>%
    #filter(fundingagency != "DOD") %>%
    filter(fundingagency == "USAID") %>%
    pivot_longer(cols = starts_with("value"),
                 names_to = "metric", values_to = "value") %>%
    mutate(metric_color = case_when(
      metric == 'value' & indicator == 'TX_CURR_ADJ' ~ grey30k,
      metric == 'value' & str_detect(indicator, 'TX_CURR') ~ scooter,
      metric == 'value' & indicator %in% c('TX_NEW', 'TX_RTT') ~ genoa_light,
      metric == 'value' & indicator %in% c('TX_ML') ~ old_rose,
      metric == 'value_base' & value > 0 ~ trolley_grey_light
    ))

  # Labels
  df_tx_curr_labels <- df_tx_waterfall %>%
    filter(str_detect(indicator, "TX_CURR"),
           metric == 'value')

  df_tx_other_labels <- df_tx_waterfall %>%
    sum_group(sum_vars = 'value', keep_group = TRUE,
              fiscal_year, fundingagency, operatingunit, indicator) %>%
    filter(!str_detect(indicator, "TX_CURR"),
           metric == 'value') %>%
    mutate(label = case_when(
      str_detect(indicator, "ML") ~ paste0("-", comma(value)),
      TRUE ~ paste0("+", comma(value))))

  # VIZ ----
  df_tx_waterfall %>%
    ggplot(aes(x = indicator)) +
    geom_col(aes(y = value, fill = metric_color)) +
    geom_hline(yintercept = 0, color = grey50k) +
    geom_text(data = df_tx_curr_labels,
              aes(x = indicator, y = value,
                  label = comma(value, 1)),
              vjust = 1.5, color = grey10k) +
    geom_text(data = df_tx_other_labels,
              aes(x = indicator,
                  y = value_ttl,
                  label = label),
                  vjust = -1, color = grey70k) +
    scale_fill_identity() +
    #scale_y_continuous(position = "right", labels = comma) +
    facet_wrap(~fundingagency) +
    labs(x = "", y = "") +
    si_style_nolines()



































df <- df_psnu %>%
  filter(fiscal_year %in% c(prev_fy, curr_fy),
         indicator %in% c('TX_CURR', 'TX_PVLS')) %>%
  mutate(indicator = paste0(indicator, "_", numeratordenom)) %>%
  group_by(fiscal_year, operatingunit, fundingagency, psnu, mech_name,
           standardizeddisaggregate, sex, trendsfine, indicator) %>%
  summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%
  ungroup() %>%
  reshape_msd("long")

df %>%
  filter(indicator == 'TX_CURR_N') %>%
  group_by(fundingagency, psnu, mech_name,
           standardizeddisaggregate, sex, trendsfine, indicator) %>%
  mutate(TX_CURR_LAG2 = lag(value, 2, order_by = period),
         TX_CURR_LAG1 = lag(value, 1, order_by = period)) %>%
  #select(-TX_CURR) %>%
  ungroup() %>%
  gather(indicator, value = val, TX_PVLS_D:TX_CURR_LAG1, na.rm = TRUE)

df_psnu %>%
  filter(indicator == 'TX_CURR') %>%
  distinct(standardizeddisaggregate)

df_psnu %>%
  filter(indicator == "TX_CURR",
       standardizeddisaggregate %in% c("Total Numerator", "KeyPop/HIVStatus"),
       fundingagency == "USAID",
       str_detect(psnu, "_Military", negate = TRUE),
       fiscal_year == 2021) %>%
  count(operatingunit, psnuuid, psnu, standardizeddisaggregate,
        wt = cumulative,
        sort = T) %>%
  spread(standardizeddisaggregate, n, fill = 0)%>%
  mutate(kp_share = `KeyPop/HIVStatus` / `Total Numerator`)


df_psnu %>%
  filter(indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
         str_detect(psnu, "_Military", negate = TRUE),
         fiscal_year == 2021) %>%
  group_by(operatingunit, countryname, snu1, psnu, psnuuid) %>%
  summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  rename_with(~ glue("tx_curr_2021_{.x}"), .cols = c(cumulative, targets))




#df_psnu %>% glimpse()

df_psnu %>%
  filter(indicator %in% 'TX_CURR',
         standardizeddisaggregate %in% 'Total Numerator')

df_psnu %>%
  sum_indicator(inds = c('TX_CURR', 'TX_NEW'),
                disags = 'Total Numerator',
                values = c('cumulative', 'targets'),
                fiscal_year, operatingunit, fundingagency, psnu, indicator)





df_psnu %>%
  sum_this_by(this = 'cumulative', fiscal_year, operatingunit, fundingagency, psnu)
