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
  library(extrafont)

## FUNCTIONS ----

#' @title Summarize Indicators Values
#'
#' @param df
#' @param inds
#' @param disags
#' @param sum_vars
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
    summarise(across(all_of(sum_vars), sum, na.rm = TRUE), .groups = "drop")
}

#' @title Ellipsis
#'
test_ellipsis <- function(...) {
  args <- list(...)
  print(class(args))
  print(length(args))
  print(args %>% unlist)
  print(c(...))
}

#' @title Convert Ellipsis to Vector
#'
#' @param ... Additional and varing number of function parmeters
#'
ellipsis_args <- function(...) {

  #args <- list(...)

  args <- as.character(substitute(...()))

  return(args)
}


#' @title Unpack TX_ML Disaggs
#'
unpack_tx_ml <- function(.data,
                         sum_vars = 'cumulative',
                         unpack_iit = TRUE,
                         ...) {

  # Ellipsis parameters
  cols_grp <- ellipsis_args(...)

  # Check cols availability
  cols_req <- c("indicator",
                "standardizeddisaggregate",
                "otherdisaggregate")

  if (all(cols_req %ni% names(.data))) {
    print(cols_req)
    stop("Missing required columns from the dataset")
  }

  if ("indicator" %ni% cols_grp) {
    print("Adding `indicator` in group variables")

    cols_grp <- c(cols_grp, "indicator")
  }

  # Indicators
  ind <- "TX_ML"
  disag <- "Age/Sex/ARTNoContactReason/HIVStatus"

  if (ind %ni% .data $indicator | disag %ni% .data$standardizeddisaggregate) {
    print(paste0(ind, " :: ", disag))
    stop("Missing required values from the dataset's indicator or disaggs")
  }

  # Filter and Summarize TX_ML data
  df_tx_ml <- .data %>%
    filter(indicator == ind, standardizeddisaggregate == disag) %>%
    mutate(indicator = otherdisaggregate,
           indicator = str_remove(indicator, "No Contact Outcome - "),
           indicator = str_replace(indicator, "Interruption in Treatment", "IIT"),
           indicator = str_replace(indicator, " Treatment", ""),
           indicator = case_when(
             indicator == 'Transferred Out' ~ 'TX_ML_XFRED_OUT',
             indicator == 'IIT <3 Months' ~ 'TX_ML_IIT_LT3M',
             indicator == 'IIT 3+ Months' ~ 'TX_ML_IIT_3MPLUS',
             indicator == 'Refused Stopped' ~ 'TX_ML_REF_STOPPED',
             indicator == 'Died' ~ 'TX_ML_DIED',
             TRUE ~ 'TX_ML_OTHER'
           )) %>%
    #group_by(...) %>%
    group_by_at(vars(all_of(cols_grp))) %>%
    summarise(across(all_of(sum_vars), sum, na.rm = TRUE), .groups = "drop")

  # Group IIT <3m & 3m+ into 1 category
  if (!unpack_iit) {

    cols <- setdiff(names(df_tx_ml), c(sum_vars, "indicator"))

    print(cols)

    df_tx_ml <- df_tx_ml %>%
      filter(str_detect(indicator, "IIT")) %>%
      group_by_at(all_of(cols)) %>%
      summarise(across(all_of(sum_vars), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(indicator = "TX_ML_IIT") %>%
      bind_rows(df_tx_ml, .) %>%
      filter(str_detect(indicator, "TX_ML_IIT_.*", negate = TRUE))
  }

  return(df_tx_ml)
}

# df_psnu %>%
#   filter(indicator %in% tx_inds) %>%
#   unpack_tx_ml(sum_vars = "cumulative",
#                unpack_iit = T,
#                fiscal_year, fundingagency, operatingunit, psnu) %>%
#   prinf()


#' @title Summarize TX_CURR with No Contacts
#'
#'
tx_nocontact <- function(.data,
                         rep_pd = "FY21Q3",
                         unpack = "ml",
                         ...) {

  # ... arguments
  cols_grp <- ellipsis_args(...)

  if (!"indicator" %in% cols_grp) {
    cols_grp <- c(cols_grp, "indicator")
  }

  cols_tx_grp <- c(cols_grp, "standardizeddisaggregate", "otherdisaggregate")

  cols_tx_ml <- c(cols_grp, "value")


  ## Indicators
  inds <- c(
    'TX_CURR',
    'TX_NEW',
    'TX_RTT',
    'TX_ML',
    'TX_NET_NEW')

  ind_levels1 <- c(
    'TX_CURR_LAG1',
    'TX_NEW',
    'TX_RTT',
    'TX_CURR_ADJ',
    'TX_ML',
    'TX_CURR',
    'TX_NET_NEW')

  ind_levels2 <- c(
    'TX_CURR_LAG1',
    'TX_NEW',
    'TX_RTT',
    'TX_CURR_ADJ',
    'TX_ML_IIT',
    'TX_ML_XFRED_OUT',
    'TX_ML_REF_STOPPED',
    'TX_ML_DIED',
    'TX_CURR',
    'TX_NET_NEW')

  ind_levels3 <- c(
    'TX_CURR_LAG1',
    'TX_NEW',
    'TX_RTT',
    'TX_CURR_ADJ',
    'TX_ML_IIT_LT3M',
    'TX_ML_IIT_3MPLUS',
    'TX_ML_XFRED_OUT',
    'TX_ML_REF_STOPPED',
    'TX_ML_DIED',
    'TX_CURR',
    'TX_NET_NEW')

  # Periods: curr + prev
  hist_pds <- .data %>% identify_pds(pd_end = rep_pd, len = 2)

  curr_pd <- hist_pds[1]
  prev_pd <- hist_pds[2]

  # TX_CURR, NET_NEW, RTT & TX_ML
  df_tx <- NULL

  # Filter based on unpacking method
  if (is.null(unpack)) {
    df_tx <- df_psnu %>%
      filter(indicator %in% inds &
               standardizeddisaggregate == 'Total Numerator')
  } else {
    df_tx <- df_psnu %>%
      filter(indicator %in% inds[inds != "TX_ML"] &
               standardizeddisaggregate == "Age/Sex/HIVStatus" |
             indicator == "TX_ML" &
               standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus")
  }

  # Initial Summary
  df_tx <- df_tx %>%
    reshape_msd() %>%
    filter(period == curr_pd | (period == prev_pd & indicator == "TX_CURR")) %>%
    mutate(
      indicator = case_when(
        indicator == "TX_CURR" & period == prev_pd ~ "TX_CURR_LAG1",
        TRUE ~ indicator)) %>%
    select(-c(period, period_type)) %>%
    group_by_at(all_of(cols_tx_grp)) %>%
    summarise(across(all_of("value"), sum, na.rm = TRUE), .groups = "drop")

  # Unpack ML
  if (is.null(unpack)) {

    ind_levels <- ind_levels1

    df_tx <- df_tx %>%
      group_by_at(all_of(cols_grp)) %>%
      summarise(across(all_of("value"), sum, na.rm = TRUE), .groups = "drop")

  } else if (unpack == "ml") {

    ind_levels <- ind_levels2

    df_tx_ml <- df_tx %>%
      unpack_tx_ml(sum_vars = "value", unpack_iit = F, ...)

    df_tx <- df_tx %>%
      filter(indicator != "TX_ML") %>%
      select(all_of(cols_tx_ml)) %>%
      bind_rows(df_tx_ml)

  } else if (unpack == "iit") {

    ind_levels <- ind_levels3

    df_tx_ml <- df_tx %>%
      unpack_tx_ml(sum_vars = "value", unpack_iit = T, ...)

    df_tx <- df_tx %>%
      filter(indicator != "TX_ML") %>%
      select(all_of(cols_tx_ml)) %>%
      bind_rows(df_tx_ml)

  } else {
    stop("PARAM - Invalid `unpack` option")
  }

  # Labels => for scale_x_manual()
  ind_labels <- case_when(
    ind_levels == "TX_CURR" ~ curr_pd,
    ind_levels == "TX_CURR_LAG1" ~ prev_pd,
    TRUE ~ NA_character_
  )

  # summarize and adjust
  df_tx <- df_tx %>%
    filter(indicator %in% ind_levels[1:3]) %>%
    group_by_at(vars(-c(indicator, value))) %>%
    summarise(across(value, sum, na.rm = T), .groups = "drop") %>%
    mutate(indicator = "TX_CURR_ADJ") %>%
    bind_rows(df_tx, .) %>%
    mutate(indicator = factor(indicator, levels = ind_levels, ordered = T)) #%>%
    # group_by_at(vars(-c(indicator, value))) %>%
    # mutate(
    #   label_value = case_when(
    #     str_detect(indicator, "TX_ML") ~ -1 * value,
    #     TRUE ~ value
    #   ),
    #   label_changes = case_when(
    #     str_detect(indicator, "NEW|RTT") ~ str_remove(indicator, "TX_"),
    #     indicator == "TX_ML" ~ str_remove(indicator, "TX_"),
    #     str_detect(indicator, "XFR") ~ "XFR-OUT",
    #     str_detect(indicator, "REF") ~ "R-STOP",
    #     str_detect(indicator, "REF") ~ "R-STOP",
    #     str_detect(indicator, "ITT_LT3M") ~ "ITT <3mo",
    #     str_detect(indicator, "ITT_3MPLUS") ~ "ITT 3mo+",
    #     str_detect(indicator, "ADJ") ~ "ADJ",
    #     str_detect(indicator, "TX_ML_.*") ~ str_remove(indicator, "TX_ML_"),
    #     TRUE ~ ""
    #   ),
    #   label_stages = case_when(
    #     indicator == "TX_CURR" ~ curr_pd,
    #     indicator == "TX_CURR_LAG1" ~ prev_pd,
    #     TRUE ~ ""
    #   ),
    #   label = case_when(
    #     !is.na(label_changes) ~ paste0(label_changes, "\n", comma(value)),
    #     TRUE ~ comma(value)
    #   ))

  return(df_tx)
}

#' @title Prep TX_ML data for Plot
#'
tx_ml_colors <- function(.data) {

  cols <- .data %>%
    select(!(indicator:last_col())) %>%
    names()

  .data %>%
    group_by_at(vars(-c(indicator, value))) %>%
    mutate(
      label_value = case_when(
        str_detect(indicator, "TX_ML") ~ -1 * value,
        TRUE ~ value
      ),
      label_changes = case_when(
        str_detect(indicator, "NEW|RTT") ~ str_remove(indicator, "TX_"),
        indicator == "TX_ML" ~ str_remove(indicator, "TX_"),
        str_detect(indicator, "XFR") ~ "XFR-OUT",
        str_detect(indicator, "REF") ~ "R-STOP",
        str_detect(indicator, "REF") ~ "R-STOP",
        str_detect(indicator, "ITT_LT3M") ~ "ITT <3mo",
        str_detect(indicator, "ITT_3MPLUS") ~ "ITT 3mo+",
        str_detect(indicator, "ADJ") ~ "ADJ",
        str_detect(indicator, "TX_ML_.*") ~ str_remove(indicator, "TX_ML_"),
        TRUE ~ ""
      ),
      label_stages = case_when(
        indicator == "TX_CURR" ~ curr_pd,
        indicator == "TX_CURR_LAG1" ~ prev_pd,
        TRUE ~ ""
      ),
      label = case_when(
        !is.na(label_changes) ~ paste0(label_changes, "\n", comma(value)),
        TRUE ~ comma(value)
      )) %>%
    ungroup() %>%
    group_by_at(all_of(cols)) %>%
    arrange(indicator) %>%
    mutate(
      ymin = case_when(
        indicator == "TX_CURR_ADJ" ~ value,
        str_detect(indicator, "TX_CURR") ~ 0,
        indicator == "TX_NEW" ~ lag(value, 1),
        indicator == "TX_RTT" ~ lag(cumsum(value), 1),
        str_detect(indicator, "TX_ML") ~ value[indicator == "TX_CURR_ADJ"] - cumsum(ifelse(str_detect(indicator, "TX_ML"), value, 0)),
        indicator == "TX_NET_NEW" ~ value[indicator == "TX_CURR_LAG1"],
        TRUE ~ value
      ),
      ymax = case_when(
        indicator == "TX_CURR_ADJ" ~ value,
        TRUE ~ ymin + value
      )) %>%
    ungroup() %>%
    mutate(
      color = case_when(
        indicator %in% c('TX_CURR_LAG1', 'TX_CURR') ~ scooter,
        indicator %in% c("TX_NEW", "TX_RTT") ~ genoa,
        indicator == "TX_CURR_ADJ" ~ grey30k,
        indicator == "TX_NET_NEW" ~ genoa_light,
        TRUE ~ burnt_sienna # for all the TX_ML
      ))
}


#' @title Plot TX_ML data
#'
tx_ml_bars <- function(.data, ...) {

  # Bar width
  w <- .96/2

  # Split data
  df_viz <- .data %>%
    filter(indicator != "TX_NET_NEW")

  df_nn <- .data %>%
    filter(indicator == "TX_NET_NEW")

  # limits
  n_ind <- df_viz %>%
    distinct(indicator) %>%
    pull() %>%
    length()

  xlim_labels <- df_viz %>%
    distinct(label_stages) %>%
    filter(label_stages != "") %>%
    pull(label_stages) %>%
    paste0("TX_CURR\n(", ., ")")

  xlim_labels <- xlim_labels %>%
    append(x = .,
           values = rep("", times = n_ind - length(xlim_labels)),
           after = 1)

  # Plot bar chart
  viz <- ggplot(data = df_viz) +
    geom_rect(aes(xmin = as.integer(indicator) - w,
                  xmax = as.integer(indicator) + w,
                  ymin = 0,
                  ymax = ymax,
                  fill = trolley_grey_light)) +
    geom_rect(aes(xmin = as.integer(indicator) - w,
                  xmax = as.integer(indicator) + w,
                  ymin = ymin,
                  ymax = ymax,
                  fill = color)) +
    geom_text(aes(x = as.integer(indicator),
                  y = ymax,
                  label = comma(label_value)),
              color = usaid_black,
              vjust = -1) +
    geom_text(aes(x = as.integer(indicator),
                  y = ymin,
                  label = label_changes),
              color = usaid_black,
              vjust = 1.4) +
    geom_segment(data = df_nn,
                 aes(x = as.integer(indicator) - 1 - w,
                     xend = as.integer(indicator) - 1 + w,
                     y = ymin,
                     yend = ymin),
                 size = 1,
                 color = grey10k) +
    geom_curve(data = df_nn,
               aes(x = as.integer(indicator) - 1 - (w/2),
                   xend = as.integer(indicator) - 1 - (w/2),
                   y = ymin - value,
                   yend = ymin + (value / 2)),
               arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
               curvature = -0.5,
               #angle = 90,
               size = 1,
               color = grey10k) +
    geom_text(data = df_nn,
              aes(x = as.integer(indicator) - 1,
                  y = ymin - value,
                  label = paste0("NN\n", comma(value))),
              color = grey10k) +
    scale_fill_identity() +
    scale_x_discrete(limits = xlim_labels) +
    facet_wrap(vars(...)) +
    labs(x = "", y = "") +
    si_style_nolines() +
    theme(axis.text.y = element_blank())

  return(viz)
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

  # MSDs
  df_sites <- glamr::return_latest(
    folderpath = glamr::si_path(),
    pattern = "Site_IM_.*_Nigeria") %>%
    gophr::read_msd()

  df_sites %>% glimpse()

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

  ## TX_ML - Other Disaggs Categories
  # 1 No Contact Outcome - Died
  # 2 No Contact Outcome - Interruption in Treatment <3 Mon~
  # 3 No Contact Outcome - Interruption in Treatment 3+ Mon~
  # 4 No Contact Outcome - Refused Stopped Treatment
  # 5 No Contact Outcome - Transferred Out






# VIZ ----

  df_test <- df_psnu %>%
    tx_nocontact(rep_pd = curr_pd,
                 #unpack = NULL,
                 #unpack = "ml",
                 unpack = "iit",
                 fundingagency) %>% clean_agency()

  df_test <- df_test %>%
    tx_ml_colors()

  df_test %>%
    filter(fundingagency != "DOD") %>%
    tx_ml_bars(fundingagency)

