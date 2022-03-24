##  PROJECT: SI Support for Nigeria
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: COP22 / FY23 Targets Allocation
##  LICENCE: MIT
##  DATE:    2022-03-17
##  UPDATED: 2022-03-17

## Libraries ----

  library(tidyverse)
  library(readxl)
  library(gophr)
  library(glamr)
  library(tameDP)
  library(janitor)
  library(glue)
  library(gt)
  library(gtExtras)

  library(datapackr)
  library(datimutils)
  library(datimvalidation)

  source("./Scripts/N00_Utilities.R")

## GLOBALS ----

  # Dirs ----

  dir_merdata <- si_path("path_msd")
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_graphics <- "Graphics"
  dir_cop21 <- "../../PEPFAR/COUNTRIES/Nigeria/COPs/COP21"
  dir_cop22 <- "../../PEPFAR/COUNTRIES/Nigeria/COPs/COP22"

  #dir_cop21 %>% open_path()

  # Files ----

  file_site_im <- dir_merdata %>%
    glamr::return_latest(pattern = "Site_IM_FY20.*_N")

  file_psnu_im <- dir_merdata %>%
    glamr::return_latest(pattern = "PSNU_IM_FY20.*_N")

  file_opu_dp <- dir_cop21 %>%
    return_latest("^OPU Data Pack_.*_\\d{8} _rev.xlsx$")

  file_cop21_dp <- dir_cop21 %>%
    return_latest("COP21 Data Pack .*.xlsx$")

  file_cop22_dp <- dir_cop22 %>%
    return_latest("Nigeria_datapack.*.xlsx$", recursive = T)

  # Params ----

  cntry <- "Nigeria"
  ou_uid <- get_ouuid(cntry)

  agency <- "USAID"

  curr_pd <- file_psnu_im %>% identify_pd()

  curr_fy <- curr_pd %>%
    str_sub(1, 4) %>%
    str_replace("FY", "20") %>%
    as.numeric()

  inds <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS_N")


# FUNCTION ----


# Load Data ----

  # MSD - PSNU x IM ----

  df_psnu <- file_psnu_im %>% read_msd()

  # MSD - Sites x IM ----

  df_sites <- file_site_im %>% read_msd()


# MUNGING ----

  df_sites %>% glimpse()
  df_sites %>% distinct(fundingagency)
  df_sites %>% distinct(psnu) %>% prinf()

  df_psnu %>% glimpse()
  df_psnu %>% distinct(fundingagency)

  # USAID/Military Sites => These are community work reported above psnu
  df_sites %>%
    filter(fundingagency == agency,
           str_detect(psnu, "_Mil")) %>%
    distinct(fundingagency, psnu, community,
             sitename, facility, sitetype)

  # Military related data
  df_usaid_mil <- df_sites %>%
    filter(fundingagency == agency,
           str_detect(psnu, "_Mil"))

  # Mechanisms
  df_mechs <- df_psnu %>%
    filter(fundingagency != "Dedup", is.na(cumulative)) %>%
    select(fiscal_year, fundingagency, psnu, mech_code, mech_name, primepartner) %>%
    distinct() %>%
    clean_agency() %>%
    update_mechs() %>%
    partners_label()

  df_im_cov <- df_psnu %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup",
           !(fundingagency == agency & str_detect(psnu, "_Mil"))) %>%
    distinct(fundingagency, psnu, mech_code) %>%
    clean_agency() %>%
    arrange(psnu) %>%
    group_by(psnu) %>%
    mutate(n_mech = n_distinct(mech_code)) %>%
    ungroup() %>%
    pivot_wider(names_from = mech_code,
                values_from = fundingagency,
                names_sort = TRUE)

  im_closing <- c("81857",
                  "81857",
                  "18655",
                  "81856",
                  "18655",
                  "100222")

  im_incoming <- c("160521",
                   "160522",
                   "160524",
                   "160525",
                   "160523",
                   "160527")


# VIZ ----

  # USAID Mechs - list of States
  df_mechs %>%
    filter(fundingagency == agency) %>%
    gt()

  # IM Coverage ----
  plot_ims_cov <-
    df_mechs %>%
    filter(fundingagency == agency, primepartner != "TBD") %>%
    mutate(
      mech_code = case_when(
        mech_code %in% im_closing ~ paste0(mech_code, "*"),
        mech_code %in% im_incoming ~ paste0(mech_code, "**"),
        TRUE ~ mech_code
    )) %>%
    select(-c(partner)) %>%
    group_by(fiscal_year, mech_code, primepartner, mech_name) %>%
    summarise(psnus = paste0(sort(psnu), collapse = ", "), .groups = "drop") %>%
    arrange(mech_code, primepartner) %>%
    mutate(id = row_number()) %>%
    relocate(id, .before = 1) %>%
    gt(groupname_col = "fiscal_year") %>%
    cols_label(id = "",
               mech_code = "CODE",
               mech_name = "MECHANISM",
               primepartner = "PARTNER",
               psnus = "STATES") %>%
    tab_header(title = md("**USAID/NIGERIA - IMPLEMENTING MECHANISMS**")) %>%
    fmt_missing(columns = everything(), missing_text = "....") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(side = c("top", "bottom"), color = grey70k, weight = px(2))
      ),
      locations = cells_column_labels(
        columns = everything()
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_fill(color = grey10k),
        cell_borders(side = "right", color = grey70k, weight = px(2))
      ),
      locations = cells_body(
        columns = "mech_code",
        rows = everything()
      )
    ) %>%
    tab_style(
      style = cell_text(color = usaid_red),
      locations = cells_body(
        columns = mech_code,
        rows = gt::matches("\\d*$")
      )
    ) %>%
    tab_source_note(source_note = "*  Mechanisms closing out") %>%
    tab_source_note(source_note = "** Mechanisms starting this year")

  gtsave(data = plot_ims_cov,
         filename = paste0(dir_graphics, "/Nigeria - Mechanisms State Coverage.png"),
         zoom = .7)

  gtsave(data = plot_ims_cov,
         filename = paste0(dir_graphics, "/Nigeria - Mechanisms State Coverage.pdf"),
         zoom = .7)

  ## Mechs - State Coverage ----
  plot_psnu_cov <-
    df_mechs %>%
      filter(fundingagency == agency,
             primepartner != "TBD",
             fiscal_year == curr_fy) %>%
      mutate(
        mech_code = case_when(
          mech_code %in% im_closing ~ paste0("<b>", mech_code, "*</b>"),
          mech_code %in% im_incoming ~ paste0("<b>", mech_code, "**</b>"),
          TRUE ~ paste0("<b>", mech_code, "</b>")
        ),
        partner = paste0(mech_code, " - ", partner)) %>%
      select(-c(primepartner, mech_name)) %>%
      arrange(desc(fundingagency), partner, psnu) %>%
      group_by(psnu) %>%
      summarise(partners = paste0(partner, collapse = ";  "), .groups = "drop") %>%
      arrange(psnu) %>%
      mutate(id = row_number()) %>%
      relocate(id, .before = 1) %>%
      gt() %>%
      cols_label(id = "",
                 psnu = "STATE",
                 partners = "MECHANISMS") %>%
      tab_header(title = md(paste0("**USAID/NIGERIA - ",
                                   curr_fy, " IMPLEMENTING MECHANISMS**"))) %>%
      fmt_missing(columns = everything(), missing_text = "....") %>%
      fmt_markdown(columns = partners, rows = everything()) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_borders(side = c("top", "bottom"), color = grey70k, weight = px(2))
        ),
        locations = cells_column_labels(
          columns = everything()
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = grey10k),
          cell_borders(side = "right", color = grey70k, weight = px(2))
        ),
        locations = cells_body(
          columns = psnu,
          rows = everything()
        )
      ) %>%
      tab_source_note(source_note = "*  Mechanisms closing out") %>%
      tab_source_note(source_note = "** Mechanisms starting this year")

  gtsave(data = plot_psnu_cov,
         filename = paste0(dir_graphics, "/Nigeria - ", curr_fy, " States Mechanisms Coverage.png"),
         zoom = .7)

  gtsave(data = plot_psnu_cov,
         filename = paste0(dir_graphics, "/Nigeria - ", curr_fy, " States Mechanisms Coverage.pdf"),
         zoom = .7)


  # IM List ----
  plot_ims <- df_mechs %>%
    filter(primepartner != "TBD", fiscal_year == curr_fy) %>%
    mutate(partner = paste0(mech_code, " - ", partner)) %>%
    select(-c(primepartner, mech_name)) %>%
    arrange(desc(fundingagency), psnu, partner) %>%
    pivot_wider(names_from = fiscal_year,
                names_sort = TRUE,
                values_from = partner) %>%
    mutate(
      mech_code = case_when(
        mech_code %in% im_closing ~ paste0("<b>", mech_code, "*</b>"),
        mech_code %in% im_incoming ~ paste0("<b>", mech_code, "**</b>"),
        TRUE ~ paste0("<b>", mech_code, "</b>")
      )
    ) %>%
    group_by(fundingagency, psnu) %>%
    gt() %>%
    cols_label(mech_code = "MECHANISM") %>%
    tab_header(title = md("**USAID/NIGERIA - LIST of MECHANISMS**")) %>%
    fmt_missing(columns = everything(), missing_text = "....") %>%
    fmt_markdown(columns = mech_code, rows = everything()) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(side = c("top", "bottom"), color = grey70k, weight = px(2))
      ),
      locations = cells_column_labels(
        columns = everything()
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(side = "right", color = grey70k, weight = px(2))
      ),
      locations = cells_body(
        columns = "mech_code",
        rows = everything()
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = grey10k),
        cell_text(style = "italic", weight = "bold")
      ),
      locations = cells_body(
        columns = c(fundingagency, mech_code),
        rows = everything()
      )
    ) %>%
    tab_source_note(source_note = "*  Mechanisms closing out") %>%
    tab_source_note(source_note = "** Mechanisms starting this year")

  gtsave(data = plot_ims,
         filename = paste0(dir_graphics, "/Nigeria - List of All Mechanisms.png"),
         zoom = .7)

  gtsave(data = plot_ims,
         filename = paste0(dir_graphics, "/Nigeria - List of All Mechanisms.pdf"),
         zoom = .7)






