# PROJECT: si-naija
# PURPOSE: Visualization Utilities for Bivariable maps
# AUTHOR: Baboyma Kagniniwa
# LICENSE: MIT
# REF. ID: f2ee4998
# CREATED: 2024-10-01
# UPDATED: 2024-10-01
# NOTES:

# Libraries ====

  library(tidyverse)
  library(gisr)
  library(glitr)
  library(patchwork)
  library(cowplot)
  library(colorspace)
  library(rcartocolor)
  library(scales)

# Params

  ref_id <- "f2ee4998"
  ou <-  "Nigeria"
  cntry <- ou
  agency <- "USAID"


# Functions  ====

  #' @title Validate color input
  #'
  #' @param color description
  #'
  validate_color <- function(color){

    ## Default values
    col <- NULL
    msg <- ""

    ## Validations

    ## RGB Colors
    if (class(color) == "RGB") {
      message("RGB Colorspace")
      return(color)
    }

    ## color name
    if(is.character(color) & stringr::str_starts(color, "#", negate = TRUE) &
       is.na(match(tolower(color), grDevices::colors()))) {

      msg <- "Color name does not seem to be valid - use `grDevices::colors()` to confirm."
      col <- NULL
    }
    else if (is.character(color) & stringr::str_starts(color, "#", negate = TRUE) &
             !is.na(match(tolower(color), grDevices::colors())))   {
      msg <- ""
      col <- tolower(color)
    }

    ## Color hex
    if (is.character(color) & stringr::str_starts(color, "#") &
        stringr::str_detect(tolower(color), "^#([:alnum:]{3}){1,2}([:alnum:]{2})?$", negate = TRUE)) {

      msg <- "Color hex code doe not seem to be valid - use `scales::show_col()` to confirm."
      col <- NULL
    }
    else if (is.character(color) & stringr::str_starts(color, "#") &
             stringr::str_detect(tolower(color), "^#([:alnum:]{3}){1,2}([:alnum:]{2})?$", negate = F)) {
      msg <- ""
      col <- tolower(color)
    }

    ## Color index
    if (is.numeric(color) & is.na(grDevices::palette()[color])) {

      msg <- "Color index seems to be out of range - use `grDevices::palette()` to confirm."
      col <- NULL
    }
    else if (is.numeric(color) & !is.na(grDevices::palette()[color])) {
      msg <- ""
      col <- grDevices::palette()[color]
    }


    ## Outputs

    if (msg != "") message(msg)

    if (is.null(col)) {
      stop("INVALID COLOR!")
    }

    return(col)
  }


  #' @title Get RGB Codes from color
  #'
  #' @param color
  #' @param out
  #'
  color_signals <- function(color, out = c('list', 'vector')){

    pout <- match.arg(out)

    # Validate color
    ccolor <- color %>%
      validate_color()

    ## Process RGB Color classes
    if (class(ccolor) == "RGB"){
      return(rgb_signals(color, out = pout))
    }

    ## Process character color classes
    ccolor <- ccolor %>%
      grDevices::col2rgb()

    if (pout == 'list') {
      ccolor %>%
        base::t() %>%
        tibble::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
        dplyr::rename_with(~stringr::str_sub(.x, 1, 1)) %>%
        base::as.list()
    }
    else if (pout == 'vector') {
      ccolor %>%
        base::as.matrix() %>%
        tibble::as_tibble(.name_repair =  ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
        dplyr::pull()
    }
  }


  #' @title Extact R, G, B values from RGB Color object
  #'
  #' @param crgb
  #' @param out
  #'
  rgb_signals <- function(crgb, out = c("list", "vector")) {

    pout <- match.arg(out)

    ccoords <- crgb %>%
      colorspace::coords() %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      dplyr::rename_with(stringr::str_to_lower)

    if (pout == "list") {
      ccoords %>% base::as.list()
    }
    else if (pout == "vector") {
      ccoords %>%
        tidyr::pivot_longer(
          cols = dplyr::everything(),
          names_to = "name",
          values_to = "value"
        ) %>%
        dplyr::pull(value)
    }
  }


  #' @title RGB Color Parts to hex
  #'
  #'
  rgb2hex <- function(color, mvalue = 255) {
    grDevices::rgb(color$r, color$g, color$b, maxColorValue = mvalue)
  }


  #' @title Covert vector to RGB color object
  #'
  #' @param color
  #' @param mvalue
  #'
  vec2rgb <- function(color, mvalue = 255) {
    colorspace::RGB(color[1], color[2], color[3])
  }


  #' @title Covert color part to hsv
  #'
  #' @param color
  #' @param a
  #'
  vec2hsv <- function(color, a = 1) {
    colorspace::hsv(color[1], color[2], color[3], alpha = a)
  }


  #' @title Covert color parts to hex
  #'
  #' @param color
  #' @param mvalue
  #'
  vec2hex <- function(color, mvalue = 255) {
    grDevices::rgb(color[1], color[2], color[3], maxColorValue = mvalue)
  }


  #' @title Mix Colors
  #'
  #' @param col1
  #' @param col2
  #' @param a
  #' @param space
  #'
  mix_colors <- function(colA, colB, a = .5, space = "RGB") {

    colorspace::mixcolor(
      alpha = a,
      color1 = colA %>% color_signals("vector") %>% vec2rgb(),
      color2 = colB %>% color_signals("vector") %>% vec2rgb(),
      where = space
    )
  }

  #' @title Create n quantile buckets for a numerical variable
  #'
  #' @param .data
  #' @param name
  #' @param nclass
  #'
  var_quantiles <- function(.data, name, nclass = 3) {

    # create n buckets for var name
    quantiles <- .data %>%
      dplyr::pull({{name}}) %>%
      stats::quantile(probs = seq(0, 1, length.out = nclass +1), na.rm = T) %>%
      as.vector()

    return(quantiles)
  }

  #' @title Create bivariate color scale
  #'
  #' @param cols1
  #' @param cols2
  #'
  get_bivariate_colors <- function(cols1, cols2){

    # Length of colors
    n_col1 <- length(cols1)
    n_col2 <- length(cols2)

    # Transparency values base of cols1
    alphas <- seq(from = 0, to = 1, length.out = n_col1 + 2)
    alphas <- alphas[2:(length(alphas) - 1)]

    df_vars <- tidyr::expand_grid(1:n_col1, 1:n_col2) %>%
      tibble::as_tibble() %>%
      dplyr::rename(grp1 = 1, grp2 = 2) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        col1 = cols1[grp1],
        col2 = cols2[grp2],
        key = paste0(grp1, "-", grp2),
        value = mix_colors(col1, col2, a = alphas[grp1]) %>%
          rgb_signals() %>%
          vec2hex(),
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        label1 = case_when(
          grp1 == max(grp1) ~ "High",
          grp1 == min(grp1) ~ "Low",
          TRUE ~ NA_character_
        ),
        label2 = case_when(
          grp2 == max(grp2) ~ "High",
          grp2 == min(grp2) ~ "Low",
          TRUE ~ NA_character_
        ),
        label = case_when(
          grp1 == max(grp1) & grp2 == max(grp2) ~ "HH",
          grp1 == min(grp1) & grp2 == max(grp2) ~ "LH",
          grp1 == min(grp1) & grp2 == min(grp2) ~ "LL",
          grp1 == max(grp1) & grp2 == min(grp2) ~ "HL",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::relocate(col1, .after = grp1) %>%
      dplyr::arrange(desc(grp2), desc(grp1))

    return(df_vars)
  }

  #' @title Generate bivariate legend
  #'
  #' @param .colors
  #' @param out
  #'
  get_bivariate_legend <- function(.colors, out = c("both", "first", "second")) {

    pout = match.arg(out)

    if (pout == "first") {
      .legend <- .colors %>%
        dplyr::distinct(grp1, col1, label1) %>%
        ggplot2::ggplot(aes(x = 1, y = grp1, fill = col1, label = label1)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(color = grey90k, hjust = 0, fontface = "bold") +
        ggplot2::scale_fill_identity() +
        ggplot2::theme_void()
    }
    else if (pout == "second") {
      .legend <- .colors %>%
        dplyr::distinct(grp2, col2, label2) %>%
        ggplot2::ggplot(aes(x = grp2, y = 3, fill = col2, label = label2)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(color = grey90k, vjust = 0, fontface = "bold") +
        ggplot2::scale_fill_identity() +
        ggplot2::theme_void()
    }
    else if (pout == "both") {
      .legend <- .colors %>%
        ggplot2::ggplot(aes(x = grp1, y = grp2, fill = value, label = label)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(color = grey90k, fontface = "bold") +
        ggplot2::scale_fill_identity() +
        ggplot2::labs(x = "Higher -->", y = "Higher -->") +
        ggplot2::coord_fixed() +
        ggplot2::theme_void()
    }
    else {

      .legend <- .colors %>%
        ggplot2::ggplot(aes(x = grp1, y = grp2, fill = value, label = label)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(color = grey90k, fontface = "bold") +
        ggplot2::scale_fill_identity() +
        ggplot2::labs(x = "Higher -->", y = "Higher -->") +
        ggplot2::coord_fixed() +
        ggplot2::theme_void()
    }

    print(.legend)

    return(.legend)
  }

# EXAMPLES ====

  ## Carto Colors
  rcartocolor::display_carto_all()

  rcartocolor::carto_pal(3, "SunsetDark") %>% scales::show_col(ncol = 3)
  rcartocolor::carto_pal(3, "RedOr") %>% scales::show_col(ncol = 3)
  rcartocolor::carto_pal(3, "TealGrn") %>% scales::show_col(ncol = 3)

  pal1 <- c("#ffd4ac", "#d56d4b", "#923417") # burnt_siennas
  pal2 <- c("#89dacb", "#459688", "#004137") # genoas
  pal3 <- c("#ffec6f", "#eab538", "#ba8b00") # golden_sands
  pal4 <- c("#ffb5ba", "#ee636e", "#af273d") # old_roses
  pal5 <- rcartocolor::carto_pal(4, "RedOr")
  pal6 <- rcartocolor::carto_pal(5, "RedOr")
  pal7 <- rcartocolor::carto_pal(3, "TealGrn")

  df_colors <- get_bivariate_colors(cols1 = pal1, cols2 = pal2)
  df_colors3 <- get_bivariate_colors(cols1 = pal3, cols2 = pal2)
  df_colors4 <- get_bivariate_colors(cols1 = pal4, cols2 = pal2)
  df_colors5 <- get_bivariate_colors(cols1 = pal5, cols2 = pal7)
  df_colors6 <- get_bivariate_colors(cols1 = pal6, cols2 = pal7)

  df_colors %>% get_bivariate_legend("first")
  df_colors %>% get_bivariate_legend("second")
  df_colors %>% get_bivariate_legend("both")


  # (
  #   df_colors %>% get_bivariate_legend("first") +
  #     df_colors %>% get_bivariate_legend("second") +
  #     df_colors %>% get_bivariate_legend("both")
  # )
  #
  # (
  #   df_colors3 %>% get_bivariate_legend("first") +
  #     df_colors3 %>% get_bivariate_legend("second") +
  #     df_colors3 %>% get_bivariate_legend("both")
  # )
  #
  # (
  #   df_colors4 %>% get_bivariate_legend("first") +
  #     df_colors4 %>% get_bivariate_legend("second") +
  #     df_colors4 %>% get_bivariate_legend("both")
  # )
  #
  # (
  #   df_colors5 %>% get_bivariate_legend("first") +
  #     df_colors5 %>% get_bivariate_legend("second") +
  #     df_colors5 %>% get_bivariate_legend("both")
  # )
  #
  # (
  #   df_colors6 %>% get_bivariate_legend("first") +
  #     df_colors6 %>% get_bivariate_legend("second") +
  #     df_colors6 %>% get_bivariate_legend("both")
  # )

