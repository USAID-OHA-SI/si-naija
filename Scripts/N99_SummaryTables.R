## PROJECT:  SI Support for Nigeria
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Nigeria/PEPFAR Health Zones
## Date:     2021-01-19

# LIBRARIES -------------------------------

  library(tidyverse)
  library(cowplot)  # Use ggdraw & draw_plot
  library(gt)       # Pretty tables
  library(vroom)
  library(janitor)
  library(glamr)
  library(glitr)
  library(scales)
  library(RColorBrewer)
  library(here)
  library(patchwork) # use of inset_element
  library(extrafont)
  library(glue)

# GLOBAL ------------------------------------

  # Global data
  dir_merdata <- glamr::si_path("path_msd")
  dir_raster <- glamr::si_path("path_raster")
  dir_vector <- glamr::si_path("path_vector")

  # Local data & outputs
  data <- "./Data"
  dataout <- "./Dataout"
  images <- "./Images"
  graphics <- "./Graphics"

  # Country name
  country <- "Nigeria"

  agency <- "USAID"

  RColorBrewer::display.brewer.all()

  pal <- brewer.pal(5, "Spectral")[2:5]


# FUNCTIONS ---------------------------------

  # Cleanup metrics
  #' @param df
  #' @return Cleaned df
  #'
  clean_metrics <- function(df) {

    df %>%
      mutate(
        fy2019cumulative = case_when(
          !indicator %in% c("positivity",
                            "retention",
                            "linkage",
                            "coverage",
                            "suppression") ~ fy2019cumulative
        ),
        fy2019_targets = case_when(
          !indicator %in% c("positivity",
                            "retention",
                            "linkage",
                            "coverage",
                            "suppression") ~ fy2019_targets
        ),
        fy2020cumulative = case_when(
          !indicator %in% c("positivity",
                            "retention",
                            "linkage",
                            "coverage",
                            "suppression") ~ fy2020cumulative
        ),
        fy2020_targets = case_when(
          !indicator %in% c("positivity",
                            "retention",
                            "linkage",
                            "coverage",
                            "suppression") ~ fy2020_targets
        ),
        fy2019achieve = case_when(
          str_detect(indicator, "[:upper:]") & !is.na(fy2019cumulative) &
            !is.na(fy2019_targets) ~
            round(fy2019cumulative / fy2019_targets * 100, 2)
        ),
        fy2020achieve = case_when(
          str_detect(indicator, "[:upper:]") & !is.na(fy2020cumulative) &
            !is.na(fy2020_targets) ~
            round(fy2020cumulative / fy2020_targets * 100, 2)
        ),
        fy2020gap = case_when(
          !is.na(fy2020cumulative) &
            !is.na(fy2020_targets) ~
            (fy2020_targets - fy2020cumulative)
        ),
        fy2020gap = case_when(
          !is.na(fy2020gap) & fy2020gap > 0 ~ fy2020gap
        )
      )
  }


  #' Create Summary Table
  #'
  #' @param df
  #' @param
  #' @return gt table as a view
  #'
  summary_table <- function(df,
                            rep_agency = "USAID",
                            rep_country = "Nigeria",
                            prev_fy = "2019",
                            curr_fy = "2020") {

    # Beautifier table
    df %>%
      #filter(fundingagency == rep_agency) %>%
      #dplyr::select(-fundingagency) %>%
      gt(rowname_col = "indicator") %>%
      tab_header(
        title = paste0(str_to_upper(rep_country), " - ", rep_agency, "/PEPFAR Programs Overview"),
        subtitle = paste0("FY",
                          str_sub(prev_fy, 3, 4), " & ",
                          str_sub(curr_fy, 3, 4), " Performance Summary")
      ) %>%
      tab_spanner(
        label = prev_fy,
        columns = starts_with(paste0("fy", prev_fy))
      ) %>%
      tab_spanner(
        label = curr_fy,
        columns = starts_with(paste0("fy", curr_fy))
      ) %>%
      # Reformat Column namess
      cols_label(
        fy2019q1 = "Q1",
        fy2019q2 = "Q2",
        fy2019q3 = "Q3",
        fy2019q4 = "Q4",
        fy2019cumulative = "Cumulative",
        fy2019_targets = "Targets",
        fy2019achieve = "Achievement",
        fy2020q1 = "Q1",
        fy2020q2 = "Q2",
        fy2020q3 = "Q3",
        fy2020q4 = "Q4",
        fy2020cumulative = "Cumulative",
        fy2020_targets = "Targets",
        fy2020achieve = "Achievement",
        fy2020gap = "Gap"
      ) %>%
      # Append percentage signe
      text_transform(
        locations = list(
          cells_body(columns = cols_ach),
          cells_body(columns = cols_qtr, rows = rows_ach),
          cells_body(columns = c(cols_cum, cols_trgt), rows = rows_ach[2:3])
        ),
        fn = function(x) {
          case_when(
            is.na(x) | x != "" ~ paste0(x, "%"),
            TRUE ~ ""
          )
        }
      ) %>%
      # Format numeric/missing values
      fmt_number(everything(), decimals = 0) %>%
      fmt_missing(everything(), missing_text = "") %>%
      # Borders
      tab_style(
        style = cell_borders(
          sides = c("top", "bottom"),
          weight = px(1),
          color = grey70k
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        )
      ) %>%
      # Bold and italic text
      tab_style(
        style = cell_text(
          weight = "bold",
          style = "italic"
        ),
        locations = cells_body(
          columns = c(cols_cum, cols_trgt, cols_gap),
          rows = everything()
        )
      ) %>%
      # Achievement lt75
      tab_style(
        style = cell_text(
          color = usaid_red,
          weight = "bold",
          style = "italic"
        ),
        locations = list(
          cells_body(
            columns = cols_ach %>% first(),
            rows = rows_ach1_lt75
          ),
          cells_body(
            columns = cols_ach %>% last(),
            rows = rows_ach2_lt75
          )
        )
      ) %>%
      # Achievement 75 - 90
      tab_style(
        style = cell_text(
          color = si_orange,
          weight = "bold",
          style = "italic"
        ),
        locations = list(
          cells_body(
            columns = cols_ach %>% first(),
            rows = rows_ach1_7590
          ),
          cells_body(
            columns = cols_ach %>% last(),
            rows = rows_ach2_7590
          )
        )
      ) %>%
      # Achievement >= 90
      tab_style(
        style = cell_text(
          color = wapo_lgreen,
          weight = "bold",
          style = "italic"
        ),
        locations = list(
          cells_body(
            columns = cols_ach %>% first(),
            rows = rows_ach1_gt90
          ),
          cells_body(
            columns = cols_ach %>% last(),
            rows = rows_ach2_gt90
          )
        )
      ) %>%
      # Hightlight FY20 Gaps
      tab_style(
        style = cell_text(
          color = usaid_red,
          style = "italic"
        ),
        locations = cells_body(
          columns = cols_gap,
          rows = rows_gap
        )
      ) %>%
      # Hightlight everyother rows
      tab_style(
        style = cell_fill(
          color = usaid_lightblue,
          alpha = .2
        ),
        locations = cells_body(
          columns = everything(),
          rows = rows[c(TRUE, FALSE)] #rows_ach
        )
      ) %>%
      tab_source_note(
        source_note = paste0("Data Source: FY20Q4c MSD, ",
                             "Produced by OHA/SIEI/SI on ",
                             format(Sys.Date(), "%Y-%m-%d"))
      )
  }

# DATA --------------------------------------------------

  # Country MSD Summary
  df_cntry_sum <- list.files(
      path = dataout,
      pattern = "^Nig.* - Overview_tbl_q4_\\d{8}.csv$",
      full.names = TRUE
    ) %>%
    sort() %>%
    last() %>%
    vroom()

  df_cntry_sum %>% glimpse()

  #View(df_cntry_sum)


  # SNU1 MSD Summary
  df_snu1_sum <- list.files(
      path = dataout,
      pattern = "^Nig.* - Overview_snu1_tbl_q4_\\d{8}.csv$",
      full.names = TRUE
    ) %>%
    sort() %>%
    last() %>%
    vroom()


  # SNU1 MSD Summary
  df_psnu_sum <- list.files(
      path = dataout,
      pattern = "^Nig.* - Overview_psnu_tbl_q4_\\d{8}.csv$",
      full.names = TRUE
    ) %>%
    sort() %>%
    last() %>%
    vroom() %>%
    dplyr::select(-psnuuid)

  df_psnu_sum %>% glimpse()

# VIZ ----------------------------------------------------

  ## Country Table -----------------

  # Summarise data

  df <- df_cntry_sum %>%
    filter(fundingagency == "USAID") %>%
    dplyr::select(-fundingagency) %>%
    clean_metrics()

  ## Columns -------------

  #  Cols - All original
  cols <- df %>% names()

  # Cols - Reporting Periods
  cols_qtr <- df %>%
    dplyr::select(matches("^fy\\d{4}q\\d{1}$")) %>%
    names()

  # Cols - Targets
  cols_trgt <- df %>%
    dplyr::select(ends_with("targets")) %>%
    names()

  # Cols - Cumulative
  cols_cum <- df %>%
    dplyr::select(ends_with("cumulative")) %>%
    names()

  # Cols - Achieve
  cols_ach <- df %>%
    dplyr::select(ends_with("achieve")) %>%
    names()

  # Cols - Gap
  cols_gap <- df %>%
    dplyr::select(ends_with("gap")) %>%
    names()

  ## Rows --------------------------------------

  # Rowsnames
  rows <- df %>%
    rownames() %>%
    as.integer()

  # rows - values
  rows_results <- df %>%
    mutate(row_id = row_number()) %>%
    filter(str_detect(indicator, "[:upper:]")) %>%
    pull(row_id)

  # rows - calculated percentages
  rows_ach <- df %>%
    mutate(row_id = row_number()) %>%
    filter(!str_detect(indicator, "[:upper:]")
    ) %>%
    pull(row_id)

  # Rows - Gap
  rows_gap <- df %>%
    mutate(row_id = row_number()) %>%
    filter(!is.na(fy2020gap)) %>%
    pull(row_id)

  # Rows - FY19 Achievement
  rows_ach1_lt75 <- df %>%
    mutate(row_id = row_number()) %>%
    filter(fy2019achieve < 75) %>%
    pull(row_id)

  rows_ach1_7590 <- df %>%
    mutate(row_id = row_number()) %>%
    filter(fy2019achieve >= 75 & fy2019achieve < 90) %>%
    pull(row_id)

  rows_ach1_gt90 <- df %>%
    mutate(row_id = row_number()) %>%
    filter(fy2019achieve >= 90) %>%
    pull(row_id)

  # Rows - FY20 Achievement
  rows_ach2_lt75 <- df %>%
    mutate(row_id = row_number()) %>%
    filter(fy2020achieve < 75) %>%
    pull(row_id)

  rows_ach2_7590 <- df %>%
    mutate(row_id = row_number()) %>%
    filter(fy2020achieve >= 75 & fy2020achieve < 90) %>%
    pull(row_id)

  rows_ach2_gt90 <- df %>%
    mutate(row_id = row_number()) %>%
    filter(fy2020achieve >= 90) %>%
    pull(row_id)


  # ViZ original data
  df %>% gt()

  # gt table
  df %>% summary_table()

# Beautifier table
df %>%
  #gt() %>%
  gt(rowname_col = "indicator") %>%
  tab_header(
    title = "DRC - PEPFAR Programs Performance Overview",
    subtitle = "FY19 & 20, USAID Only Performance Summary"
  ) %>%
  tab_spanner(
    label = "2019",
    columns = starts_with("fy2019")
  ) %>%
  tab_spanner(
    label = "2020",
    columns = starts_with("fy2020")
  ) %>%
  # Reformat Column namess
  cols_label(
    fy2019q1 = "Q1",
    fy2019q2 = "Q2",
    fy2019q3 = "Q3",
    fy2019q4 = "Q4",
    fy2019cumulative = "Cumulative",
    fy2019_targets = "Targets",
    fy2019achieve = "Achievement",
    fy2020q1 = "Q1",
    fy2020q2 = "Q2",
    fy2020q3 = "Q3",
    fy2020q4 = "Q4",
    fy2020cumulative = "Cumulative",
    fy2020_targets = "Targets",
    fy2020achieve = "Achievement",
    fy2020gap = "Gap"
  ) %>%
  # Append percentage signe
  text_transform(
    locations = list(
      cells_body(columns = cols_ach),
      cells_body(columns = cols_qtr, rows = rows_ach),
      cells_body(columns = c(cols_cum, cols_trgt), rows = rows_ach[2:3])
    ),
    fn = function(x) {
      case_when(
        is.na(x) | x != "" ~ paste0(x, "%"),
        TRUE ~ ""
      )
    }
  ) %>%
  # Format numeric/missing values
  fmt_number(everything(), decimals = 0) %>%
  fmt_missing(everything(), missing_text = "") %>%
  # Borders
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      weight = px(1),
      color = grey50k
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  # Bold and italic text
  tab_style(
    style = cell_text(
      weight = "bold",
      style = "italic"
    ),
    locations = cells_body(
      columns = c(cols_cum, cols_trgt, cols_gap),
      rows = everything()
    )
  ) %>%
  # Achievement lt75
  tab_style(
    style = cell_text(
      color = usaid_red,
      weight = "bold",
      style = "italic"
    ),
    locations = list(
      cells_body(
        columns = cols_ach %>% first(),
        rows = rows_ach1_lt75
      ),
      cells_body(
        columns = cols_ach %>% last(),
        rows = rows_ach2_lt75
      )
    )
  ) %>%
  # Achievement 75 - 90
  tab_style(
    style = cell_text(
      color = si_orange,
      weight = "bold",
      style = "italic"
    ),
    locations = list(
      cells_body(
        columns = cols_ach %>% first(),
        rows = rows_ach1_7590
      ),
      cells_body(
        columns = cols_ach %>% last(),
        rows = rows_ach2_7590
      )
    )
  ) %>%
  # Achievement >= 90
  tab_style(
    style = cell_text(
      color = wapo_lgreen,
      weight = "bold",
      style = "italic"
    ),
    locations = list(
      cells_body(
        columns = cols_ach %>% first(),
        rows = rows_ach1_gt90
      ),
      cells_body(
        columns = cols_ach %>% last(),
        rows = rows_ach2_gt90
      )
    )
  ) %>%
  # Hightlight FY20 Gaps
  tab_style(
    style = cell_text(
      color = USAID_red,
      style = "italic"
    ),
    locations = cells_body(
      columns = cols_gap,
      rows = rows_gap
    )
  ) %>%
  # Hightlight everyother rows
  tab_style(
    style = cell_fill(
      color = USAID_ltblue,
      alpha = .2
    ),
    locations = cells_body(
      columns = everything(),
      rows = rows[c(TRUE, FALSE)] #rows_ach
    )
  ) %>%
  tab_source_note(
    source_note = paste0("Data Source: FY20Q4i MSD, ",
                         "Produced by OHA/SIEI/SI on ",
                         format(Sys.Date(), "%Y-%m-%d"))
  )



