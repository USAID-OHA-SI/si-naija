# PURPOSE: SI Support for Nigeria
# AUTHOR:  Baboyma Kagniniwa | USAID/OHA/SIEI/SI
# PURPOSE: Spatial Autocorrelation
# REF ID:  7279b524
# LICENSE: MIT
# DATE:    2023-09-01
# UPDATE:  2023-09-01
# NOTES:   Sample code for "getting the data right"

# Libraries ====

  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gisr)
  library(gophr)
  library(grabr)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(janitor)
  library(glue)
  library(sfExtras)
  library(spdep)


# LOCALS & SETUP ====

  # Set paths

  dir_data   <- "Data"
  dir_dataout <- "Dataout"
  dir_images  <- "Images"
  dir_graphics  <- "Graphics"

  dir_mer <- glamr::si_path("path_msd")
  dir_ras <- glamr::si_path("path_raster")
  dir_shp <- glamr::si_path("path_vector")
  dir_cntry <- file.path("../../PEPFAR/COUNTRIES/country")

  # Set Params

  ref_id <- "7279b524"
  agency <- "USAID"
  cntry <- "Nigeria"

  cntry_uid <- get_ouuid(operatingunit = cntry)

  # Files

  file_nat <- si_path() %>% return_latest("NAT_SUBNAT_FY21")
  file_psnu <- si_path() %>% return_latest(glue("PSNU_IM_FY21.*_{cntry}"))
  file_site <- si_path() %>% return_latest(glue("Site_IM_FY21.*_{cntry}"))

  get_metadata(file_psnu)

  meta <- metadata

  # Pop & PLHIV Indicators
  inds_hiv <- c("POP_EST", "PLHIV", "TX_CURR_SUBNAT")
  disag_hiv <- c("Age/Sex", "Age/Sex/HIVStatus")

# Functions  =====



# LOAD DATA =====

  # Shapefiles
  sfdf_pepfar <- get_vcpolygons(path = dir_shp)

  ## PLHIV

  df_msd_nat <- file_nat %>%
    read_psd() %>%
    filter(operatingunit == cntry,
           indicator %in% inds_hiv,
           standardizeddisaggregate %in% disag_hiv)

  ## Program Results

  df_msd_psnu <- file_psnu %>%
    read_psd() %>%
    filter(operatingunit == cntry,
           indicator == "TX_CURR",
           standardizeddisaggregate == "Age/Sex/HIVStatus")

  df_msd_psnu %>%
    distinct(fiscal_year, ageasentered, age_2018, age_2019) %>%
    arrange(fiscal_year, ageasentered)

  df_msd_psnu %>%
    filter(!is.na(cumulative)) %>%
    distinct(fiscal_year, ageasentered) %>%
    arrange(fiscal_year, ageasentered) %>%
    # group_by(fiscal_year) %>%
    # mutate(id = row_number()) %>%
    mutate(id = ageasentered) %>%
    ungroup() %>%
    pivot_wider(id_cols = id,
                names_from = fiscal_year,
                values_from = ageasentered)


  # df_msd_site <- file_site %>%
  #   read_psd() %>%
  #   filter(operatingunit == cntry,
  #          indicator == "TX_CURR",
  #          standardizeddisaggregate == "Age/Sex/HIVStatus")


# MUNGE =====

  # Boundaries

  df_psnus <- df_msd_nat %>%
    filter(str_detect(psnu, "_Mil", negate = T)) %>%
    distinct(psnuuid, psnu)

  sfdf_cntry <- sfdf_pepfar %>%
    filter(uid == cntry_uid)

  sfdf_psnu <- sfdf_pepfar %>%
    left_join(df_psnus, by = c("uid" = "psnuuid")) %>%
    filter(!is.na(psnu))

  # POPs

  df_pops <- df_msd_nat %>%
    summarise(across(targets, \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, psnuuid, psnu, indicator)) %>%
    pivot_longer(cols = targets,
                 names_to = "period",
                 values_to = "value") %>%
    mutate(period_type = "Context",
           period = paste0("FY", str_sub(fiscal_year, 3, 4))) %>%
    select(period, period_type, psnuuid, psnu, indicator, value) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    clean_names() %>%
    filter(period != "FY24")

  df_pops %>% distinct(period)

  sfdf_pops <- df_pops %>%
    filter(period == meta$curr_fy_lab) %>%
    left_join(sfdf_psnu, ., by = c("uid" = "psnuuid", "psnu")) %>%
    st_transform(crs = st_crs(3857))

  spdf_pops <- df_pops %>%
    filter(period == meta$curr_fy_lab) %>%
    left_join(sfdf_psnu, ., by = c("uid" = "psnuuid", "psnu")) %>%
    st_transform(crs = st_crs(3857)) %>%
    #smoothr::smooth() %>%
    as("Spatial")

# Analysis

  # Spatial Autocorrelation
  ## Todo:
  ### 1) Identify variable & plot current distribution
  ### 2) Define neighbor: contiguous areas, distance bands & k nearest
  ### 3) Identify neighbors
  ### 4) Summarize values for each neighborhood cluster (mean) ~ lagging value (X lag)
  ### 5) Plot relationship btw Var & Var-lag + Moran's I Coef (slope : Least Square Line)
  ### 6) Evaluate relationship btw Var & Var-lag
  ### 7) Determine the significance of Moran's I Coef value: Analytical (assumptions) or Monte Carlo test
  ### 8) P-Value(pseudo)
  ### 9) Visualize Hot / Cold spots

# VIZ =====

  sfdf_pops %>% gview()

  names(sfdf_pops)

  tmap::tm_shape(sfdf_pops) +
    tmap::tm_polygons(style = "quantile",
                      col = "pop_est",
                      #convert2density = T,
                      title = "POP. Estimates",
                      lty = "dashed",
                      border.col = grey30k)

  sfdf_centers <- sfdf_pops %>%
    st_centroid_coords() %>%
    as_tibble() %>%
    st_as_sf(coords = c("longitude", "latitude"),
             crs = st_crs(sfdf_pops))

  spdf_centers <- sfdf_centers %>% as("Spatial")

  qbreaks <- quantile(sfdf_pops$pop_est, probs = seq(0, 1, by = .2))

  sfdf_pops %>%
    ggplot(aes(fill = pop_est)) +
    geom_sf(color = grey30k) +
    geom_sf(data = sfdf_centers, size = 5, shape = 21, fill = grey90k, color = grey10k) +
    geom_sf_text(data = sfdf_pops, aes(label = psnu)) +
    scale_fill_si(palette = "genoas",
                  name = "POP. EST",
                  breaks = breaks_extended(10),
                  labels = comma_format(scale = 1 / 1000000, suffix = "M")) +
    guides(fill = guide_colorsteps(show.limits = T)) +
    labs(x = "", y = "") +
    si_style_map() +
    theme(legend.direction = "horizontal",
          legend.key.width = unit(.2, "npc"),
          legend.title = element_blank())

  sfdf_pops %>%
    ggplot(aes(x = pop_est)) +
    geom_histogram(color = grey10k, fill = genoa,
                   binwidth = 1000000) +
    scale_x_continuous(label = comma_format(scale = 1/1000000, suffix = "M"),
                       breaks = seq(0, max(sfdf_pops$pop_est) + 1000000, 2000000)) +
    scale_y_continuous(breaks = seq(0, 10, 2)) +
    si_style_ygrid()


  # Define neighboring polygons
  #psnu_nb <- poly2nb(pl = sfdf_pops, queen = TRUE)
  psnu_nb <- poly2nb(pl = spdf_pops, queen = TRUE)

  class(psnu_nb)

  # Check # of neighbors for the 1st polygon
  psnu_nb[[1]]
  spdf_pops$psnu[1]
  spdf_pops$psnu[psnu_nb[[1]]]

  # Get the summary of the neighboring
  summary(psnu_nb)

  spdf_pops$pop_est[1]

  # Assign weights to neighbors
  lw <- nb2listw(neighbours = psnu_nb, style="W", zero.policy = TRUE)

  listw2U(lw)

  lw$weights[1]


  # Calculate the avg neighbor pop - lag
  pop_lag <- lag.listw(lw, spdf_pops$pop_est)
  pop <- spdf_pops$pop_est

  pops <- tibble(
    psnu = spdf_pops$psnu,
    pop = pop,
    pop_lag
  )

  # Simple Regression Model
  M <- lm(pops$pop_lag ~ pops$pop)

  M

  m_int <- coef(M)[1]
  m_slope <- coef(M)[2]

  # Morgan's I Coefficient is the slope
  MI <- m_slope

  # Plot data data
  plot(pop_lag ~ pop, pch=20, asp=1, las=1, cex=2)


  pops %>%
    ggplot(aes(x = pop, y = pop_lag)) +
      geom_point(size = 10, shape = 21, fill = old_rose, color = grey10k) +
      geom_abline(intercept = m_int, slope = m_slope, color = usaid_darkgrey, size = 1, lty = "dashed") +
      annotate(geom = "text",
               x = 12000000, y = 7000000,
               label = glue("MI = {round(m_slope, 2)}"),
               angle = (m_slope * 100)) +
      scale_x_continuous(label = comma, limits = c(0, 16000000)) +
      scale_y_continuous(label = comma) +
      coord_cartesian() +
      si_style()


  # Simulations
  n <- 199L

  Ir <- map_dbl(1:n, function(x) {

    print(x)

    pop_x <- sample(spdf_pops$pop_est, replace = F)

    pop_x_lag <- lag.listw(lw, pop_x)

    M.r <- lm(pop_x_lag ~ pop_x)

    coef(M.r)[2]
  })

  I.r

  hist(I.r, main = NULL, xlab = "Moran's I", las = 1)
  abline(v = m_slope, col = "red")

  ggplot() +
    geom_histogram(aes(x = I.r), binwidth = .05, color = grey10k, fill = genoa) +
    geom_vline(xintercept = m_slope, size = 1, color = usaid_red, linetype = "dashed") +
    labs(x = "Moran's I", y = "frequency") +
    si_style()

  ggplot() +
    geom_density(aes(x = I.r), binwidth = .05, color = grey10k, fill = genoa) +
    geom_vline(xintercept = m_slope, size = 1, color = usaid_red, linetype = "dashed") +
    labs(x = "Moran's I", y = "frequency") +
    si_style()

  # Pseudo P.value
  N.greater <- sum(m_slope > I.r)

  p <- min(N.greater + 1, n + 1, N.greater) / (n + 1)

  p

  # Moran's I stats - The easy way

  moran.test(spdf_pops$pop_est, lw)

  MC <- moran.mc(spdf_pops$pop_est, lw, nsim=199)

  MC$statistic
  MC$p.value
  MC$method
  MC$parameter
  MC$alternative

  plot(MC, main = "", las = 1)

  # Moran's I as a function of distance band

  # Define the search radius
  s_dist <- dnearneigh(sfdf_centers, 0, 200000)

  lw_dist <- nb2listw(s_dist, style = "W", zero.policy = TRUE)

  lw_dist

  MC_dist <- moran.mc(pop, lw_dist, nsim=199, zero.policy = TRUE)

  MC_dist

  plot(MC_dist)




