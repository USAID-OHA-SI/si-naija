# PROJECT: si-naija
# PURPOSE: GHSD - AHD Results
# AUTHOR: Baboyma Kagniniwa | USAID/GH - Office of HIV-AIDS
# LICENSE: MIT
# REF. ID: a029c07b
# CREATED: 2024-12-09
# UPDATED: 2024-12-09
# NOTES:

# Libraries ====

  library(tidyverse)
  library(readxl)
  library(glamr)
  library(gophr)
  library(glitr)
  library(tidytext)
  library(scales)
  library(systemfonts)
  library(glue)
  library(janitor)

# Set paths  ====

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

# Params

  ref_id <- "a029c07b"
  ou <-  "Nigeria"
  cntry <- ou
  agency <- "USAID"

# FILES

  file_ahd <- dir_data %>%
    return_latest(".*AHD & IIT Charts.xlsx")

  file_ahd %>% open_path()

# Functions  ====

  facet_label <- function(.ref = df_ahd_cr_sum, .age) {
    print(.age)

    .lbl <- .ref %>%
      filter(AgeCoarse %in% .age) %>%
      mutate(AgeCoarse = factor(AgeCoarse, levels = .age, ordered = T)) %>%
      arrange(AgeCoarse) %>%
      mutate(labels = paste0(comma(Value), " [", Prop, " TX_NEW]")) %>%
      pull(labels)

    .lbl
  }

  df_ahd_cr_sum %>%
    filter(AgeCoarse %in% c("Clients: <15", "Clients: 15+", "Clients: All")) %>%
    mutate(AgeCoarse = factor(AgeCoarse, levels = c("Clients: <15", "Clients: 15+", "Clients: All"), ordered = T)) %>%
    arrange(AgeCoarse) %>%
    mutate(labels = paste0(comma(Value), " [", Prop, " TX_NEW]")) %>%
    pull(labels)

# LOAD DATA ====

  file_ahd %>% excel_sheets()

  df_ahd <- file_ahd %>%
    read_excel(sheet = 1)

# MUNGE ====

  df_ahd %>% glimpse()

  df_ahd <- df_ahd %>%
    summarise(Clients = sum(Clients, na.rm = T),
              .by = c(Disease, Indicator)) %>%
    mutate(AgeCoarse = "All") %>%
    bind_rows(df_ahd, .)


  df_ahd <- df_ahd %>%
    group_by(Disease, AgeCoarse) %>%
    mutate(Target = lag(Clients, 1),
           Prop = percent(Clients / Target, .1)) %>%
    ungroup()



# VIZ ====

  crag <- "Cryptococcal Antigen"
  csf <- "Cerebro-Spinal Fluid"

  ind_levels <- c("TX_NEW", "AHD",
                  "CrAg_TST", "CrAg_POS",
                  "CSF_TST")

  ind_labels <- c("TX_NEW",
                  "Advanced HIV Disease",
                  "Serum CrAg TEST", "Serum CrAg POS",
                  "CSF CrAg TEST")

  df_ahd_cr_sum <- df_ahd %>%
    filter(Disease == "Cryptococcal Meningitis",
           Indicator %in% c("TX_NEW", "AHD")
    ) %>%
    select(-c(Target, Prop)) %>%
    mutate(AgeCoarse = paste0("Clients: ", AgeCoarse)) %>%
    summarise(Value = Clients[Indicator == "AHD"],
              Prop = percent(Clients[Indicator == "AHD"] / Clients[Indicator == "TX_NEW"], .1),
              .by = c(Disease, AgeCoarse))

  df_ahd_cr <- df_ahd %>%
    filter(Disease == "Cryptococcal Meningitis",
           Indicator != "TX_NEW",
           Indicator != "AHD"
           ) %>%
    mutate(indicator = factor(Indicator,
                              levels = ind_levels,
                              labels = ind_labels,
                              ordered = T),
           AgeCoarse = paste0("Clients: ", AgeCoarse))

  viz_ahd_cr <- df_ahd_cr %>%
    arrange(AgeCoarse) %>%
    ggplot(aes(y = reorder(Indicator, Target))) +
    geom_col(aes(x = Target), fill = trolley_grey_light) +
    geom_col(aes(x = Clients), fill = burnt_sienna) +
    geom_text(aes(x = Clients,
                  label = paste0(comma(Clients),
                                 ifelse((!is.na(Prop) & Prop != "0.0%"),
                                        paste0(" [", Prop, "]"),
                                        ""))),
              hjust = -.3, size = 5, color = grey90k) +
    geom_vline(xintercept = 0, color = grey80k) +
    scale_x_continuous(labels = comma, position = "top") +
    facet_wrap(vars(AgeCoarse), #vars(factor(AgeCoarse, c("Clients: All", "Clients: 15+", "Clients: <15"))),
                       scales = "free_x", ncol = 1,
               labeller = labeller(AgeCoarse = ~paste0(.x, ", AHD: ", facet_label(df_ahd_cr_sum, .x)))) +
    labs(
      x = "", y = "",
      caption = glue::glue("Source: USAID/Nigeria - LAMIS+, FY24Q3, Produced on {curr_date()}. Ref. {ref_id}"),
    )+
    si_style_xgrid() +
    theme(
      strip.placement = "outside",
      strip.text = element_text(size = 12, face = "bold", hjust = 0.04),
      strip.clip = "off"
    )

  viz_ahd_cr

  ggsave(plot = viz_ahd_cr,
         filename = file.path(dir_graphics, "Nigeria - FY24Q3 AHD Monitoring.png"),
         scale = 1.3,
         dpi = 320,
         width = 10,
         height = 6)


# EXPORT ====

