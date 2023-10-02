##  PROJECT: SI-Naija
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: Time Series ART Coverage Maps
##  REF. ID: 0402fa14
##  LICENCE: MIT
##  DATE:    2023-02-22
##  UPDATE:  2023-02-22

# LIBRARIES ----

  library(tidyverse)
  library(readxl)
  library(glamr)
  library(gophr)
  library(grabr)
  library(glitr)
  library(gisr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(ggtext)
  library(ggrepel)
  library(patchwork)

# NOTES ----

# DISCLAIMERS ----

# GLOBAL PARAMS ----

  dir_mer <- si_path(type = "path_msd")
  dir_geo <- si_path(type = "path_vector")
  dir_ras <- si_path(type = "path_raster")

  dir_data <- "./Data"

  ## Files

  ## ART Cov file
  file_art_cov <- dir_data %>%
    file.path("COP23") %>%
    return_latest(pattern = "^Demographic.*.xlsx$")

  ## Shapefile path
  file_shp <- dir_geo %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

  ## Options
  cntry <- "Nigeria"

  ouuid <- get_ouuid(cntry)

  curr_fy <- 2023

# FUNCTIONS ----

# DATA IMPORT ----

  ## SPATIAL DATA ----
  terr <- gisr::get_raster(path = dir_ras)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # Orgunits ----
  # df_attrs <- get_attributes(country = cntry)
  # df_attrs <- extract_locations(country = cntry,
  #                               add_geom = FALSE)

  #baseurl = "https://datim.org/"
  baseurl = "https://final.datim.org/"

  orgs_url <- baseurl %>%
    paste0("api/organisationUnits?fields=id,name,path,level") %>%
    paste0("&filter=path:like:", ouuid) %>%
    paste0("&paging=false&format=json")

  df_attrs <- orgs_url %>%
    httr::GET(httr::authenticate(datim_user(), datim_pwd())) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = T) %>%
    purrr::pluck("organisationUnits") %>%
    tibble::as_tibble()

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_attrs, by = c("uid" = "id")) %>%
    filter(!is.na(name))

  spdf_pepfar %>%
    sf::st_drop_geometry() %>% #glimpse()
    distinct(level)

  adm_cntry <- spdf_pepfar %>% filter(level == 3)
  adm_psnus <- spdf_pepfar %>% filter(level == 4)
  adm_comms <- spdf_pepfar %>% filter(level == 5)

  # Program Data

  df_cov <- file_art_cov %>%
    read_excel(sheet = "Sheet5", skip = 2)

# MUNGING ----

  df_cov <- df_cov %>%
    rename(state = State) %>%
    pivot_longer(cols = !state,
                 names_to = "age",
                 values_to = "coverage") %>%
    filter(!is.na(coverage)) %>%
    separate_wider_delim(
      cols = age,
      names = c("age", "group"),
      delim = "...")

  #age_grps <- c("0-9", "10-14", "15-24", "25-49", "50+")
  age_grps <- df_cov %>%
    distinct(age) %>%
    pull()

  age_hist <- df_cov %>%
    filter(state == first(unique(.$state))) %>%
    nrow()

  fy_len <- age_hist / length(age_grps)

  periods <- seq(0, fy_len-1) %>%
    map_chr(~paste0("FY", str_sub(curr_fy + .x, 3, 4), " Q4"))


  df_cov <- df_cov %>%
    group_by(state, age) %>%
    mutate(group = row_number(),
           period = periods[group])

  spdf_cov <- adm_psnus %>%
    left_join(df_cov, by = c("name" = "state")) %>%
    mutate(
      benchmark = case_when(
        coverage >= .81 ~ "Above",
        coverage < .81 ~ "Below",
        TRUE ~ NA_character_
      ),
      text_color = case_when(
        benchmark == "Above" ~ grey10k,
        benchmark == "Below" ~ grey10k,
        TRUE ~ usaid_black
      ),
      art_color = case_when(
        is.na(coverage) ~ grey10k,
        coverage < .7 ~ usaid_red,
        coverage >= .7 & coverage < .81 ~ golden_sand,
        coverage >= .81 & coverage < .95 ~ genoa_light,
        coverage >= .95 ~ genoa,
        TRUE ~ grey10k
      )
    )

# VIZ ----

  scales::show_col(c(grey10k, usaid_red, golden_sand, genoa_light, genoa))

  #sub_keys <- "<span style='display:table; padding:5px; color:#e07653'>Hello</span>"
  #sub_keys <- "<span style='display:block; padding:5px; background-color:red; color:black'>Hello</span>"
  sub_keys <- "<span style='color:{usaid_red}'><70%</span> <span style='color:{golden_sand}'>70-81%</span> <span style='color:{genoa_light}'>81-95%</span> <span style='color:{genoa}'>95%+</span>\n"

  # Produce basemap
  basemap <- terrain_map(countries = adm_cntry,
                         adm0 = adm_cntry,
                         adm1 = adm_psnus,
                         mask = TRUE,
                         terr = terr)

  # Art Sat map
  periods %>%
    walk(function(.period) {

      print(.period)

      keys <- "{.period}: <span style='color:{usaid_red}'><70%</span> | <span style='color:{golden_sand}'>70-81%</span> | <span style='color:{genoa_light}'>81-95%</span> | <span style='color:{genoa}'>95%+</span>\n"

      art_map <- basemap +
        geom_sf(data = spdf_cov %>% filter(period == .period),
                aes(fill = art_color),
                lwd = .3,
                color = grey20k,
                show.legend = "point") +
        geom_sf(data = adm_cntry,
                colour = grey10k,
                fill = NA,
                size = 1.5) +
        geom_sf(data = adm_cntry,
                colour = grey90k,
                fill = NA,
                size = .3) +
        geom_sf_text(data = spdf_cov %>% filter(period == .period),
                     aes(label = paste0(str_replace(name, " ", "\n"),
                                        "\n",
                                        ifelse(is.na(coverage), "TBC",
                                               percent(coverage, 1))),
                         color = text_color),
                     size = 2, fontface = "bold") +
        scale_fill_identity(guide = 'legend',
                            labels = c("<70%", "70-81%", "81-95%", "95%+"),
                            breaks = c(0, .7, .81, .95)) +
        scale_color_identity() +
        facet_wrap(~age, ncol = 3) +
        labs(subtitle = glue::glue(keys)) +
        theme(plot.subtitle = element_markdown(size = 14, face = "bold", hjust = 0.05))

      #art_map

      # Export Map
      si_save(
        filename = file.path(
          "./Graphics",
          paste0("NIGERIA - ", .period, " ART Saturation by Age Group - ",
                 curr_date("%Y%m%d"),
                 ".png")),
        plot = art_map,
        width = 10,
        height = 5.6)
      })






