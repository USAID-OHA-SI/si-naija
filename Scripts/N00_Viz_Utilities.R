#' @title Theme Transparent
#'
#'
theme_transparent <- function() {
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent', color = NA),
    legend.box.background = element_rect(fill='transparent', color = NA)
  )
}


#' @title Viz - fa-icons
#'
#'
fa_icons <- function(icon = "fa-users",
                     fsize = 100,
                     fcolor = "red",
                     label = NULL,
                     lsize = 20,
                     lcolor = "#212721", # usaid_black
                     loc = "bottom",
                     save_as = NULL,
                     iwidth = 1,
                     iheight = 1) {

  # Placement Guide
  df_pos <- tibble(
    pos   = c("top", "right", "bottom", "left"),
    x     = c(0,     1.25,    0,    -1.25),
    y     = c(1,     0,       -1.25,    -0),
    hjust = c(0.5,   0,       0.5,      1),
    vjust = c(0,     0.5,     1,        0.5)
  )

  df_pos_sel <- df_pos %>% filter(pos == loc)

  viz_icon <- ggplot() +
    geom_rect(aes(xmin = -.75, xmax = .75, ymin = -1, ymax = .75),
              fill = NA, color = NA) +
    geom_rect(aes(xmin = -1.25, xmax = 1.25, ymin = -1.25, ymax = 1),
              fill = NA, color = NA) +
    geom_text(aes(0, 0),
              label = fontawesome(icon),
              family = "fontawesome-webfont",
              size = fsize,
              color = fcolor)

  # Label
  if (!is.null(label)) {

    viz_icon <- viz_icon +
      geom_text(aes(df_pos_sel$x, df_pos_sel$y),
                label = label,
                size = lsize,
                color = fcolor,
                hjust = df_pos_sel$hjust,
                vjust = df_pos_sel$vjust)

  }

  #Theme
  viz_icon <- viz_icon +
    coord_equal(clip = "off") +
    theme_void() +
    theme(text = element_text(family = "Source Sans Pro")) +
    theme_transparent()

  if (!is.null(save_as)) {
    ggsave(filename = save_as,
           plot = viz_icon,
           dpi = 320,
           width = iwidth,
           height = iheight,
           bg = "transparent")
  }

  return(viz_icon)
}


#' @title Viz - fa-icons
#'
#'
fa_label <- function(icon = "fa-users",
                     fsize = 100,
                     fcolor = "red",
                     label = "USERS",
                     lsize = 20,
                     lcolor = "#212721", # usaid_black
                     loc = "bottom",
                     save_as = NULL,
                     iwidth = 1,
                     iheight = 1) {

  # Placement Guide
  df_pos <- tibble(
    pos   = c("top", "right", "bottom", "left"),
    x     = c(0,     1.25,    0,    -1.25),
    y     = c(1,     0,       -1.25,    -0),
    hjust = c(0.5,   0,       0.5,      1),
    vjust = c(0,     0.5,     1,        0.5)
  )

  df_pos_sel <- df_pos %>% filter(pos == loc)



  viz_icon <- ggplot() +
    geom_rect(aes(xmin = -.75, xmax = .75, ymin = -1, ymax = .75),
              fill = NA, color = NA) +
    geom_rect(aes(xmin = -1.25, xmax = 1.25, ymin = -1.25, ymax = 1),
              fill = NA, color = NA) +
    geom_text(aes(0, 0),
              label = fontawesome(icon),
              family = "fontawesome-webfont",
              size = fsize,
              color = fcolor)

  # Label
  if (!is.null(label)) {

    viz_icon <- viz_icon +
      geom_text(aes(df_pos_sel$x, df_pos_sel$y),
                label = label,
                size = lsize,
                color = fcolor,
                hjust = df_pos_sel$hjust,
                vjust = df_pos_sel$vjust)

  }

  #Theme
  viz_icon <- viz_icon +
    coord_equal(clip = "off") +
    theme_void() +
    theme(text = element_text(family = "Source Sans Pro")) +
    theme_transparent()

  if (!is.null(save_as)) {
    ggsave(filename = save_as,
           plot = viz_icon,
           dpi = 320,
           width = iwidth,
           height = iheight,
           bg = "transparent")
  }

  return(viz_icon)
}

#' @title Get LOGO
#'
#' @note
#'
#' @param file Prefix of filename. Use f for fill and s for stroke, eg: f_red_s_white
#'
get_logo <- function(name = "hospital",
                     fill_color = "white",
                     stroke_color = NULL,
                     stroke_width = NULL,
                     prefix = NULL,
                     path = NULL,
                     ...) {

  # Path
  if (is.null(path))
    path <- "./Images/logos"

  # Filename
  file <- paste0(name, ".png")

  if (!is.null(prefix))
    file <- paste0(prefix, "-", name, ".png")

  # Check if file exists
  img <- list.files(path, pattern = file, full.names = T)

  if (length(img) == 1)
    return(img)

  img <- file.path(path, file)

  # Save file
  fa_png(name = name,
         fill = fill_color,
         stroke = stroke_color,
         file = img,
         ...)

  return(img)
}
