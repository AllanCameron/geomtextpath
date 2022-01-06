##---------------------------------------------------------------------------##
##                                                                           ##
##  sf_helpers.R                                                             ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Lookup table for geometry types
sf_types <- c(GEOMETRY           = "other",
              POINT              = "point",
              LINESTRING         = "line",
              POLYGON            = "other",
              MULTIPOINT         = "point",
              MULTILINESTRING    = "line",
              MULTIPOLYGON       = "other",
              GEOMETRYCOLLECTION = "collection",
              CIRCULARSTRING     = "line",
              COMPOUNDCURVE      = "line",
              CURVEPOLYGON       = "other",
              MULTICURVE         = "line",
              MULTISURFACE       = "other",
              CURVE              = "line",
              SURFACE            = "other",
              POLYHEDRALSURFACE  = "other",
              TIN                = "other",
              TRIANGLE           = "other")

# Gets default graphics parameters for sf objects. This could be a static object
# but writing it as a function allows for changes in ggplot defaults over time
sf_defaults <- function() {
  defaults <- list(
    GeomPoint$default_aes,
    GeomTextpath$default_aes,
    modify_list(GeomPolygon$default_aes,
                list(fill = "grey90", colour = "grey35"))
  )

  defaults[[4]] <- modify_list(
    defaults[[3]],
    rename(GeomTextpath$default_aes,
           c(size = "point_size", fill = "point_fill"))
  )

  default_names <- unique(unlist(lapply(defaults, names)))

  lapply(setNames(default_names, default_names), function(n) {
    unlist(lapply(defaults, function(def) def[[n]] %||% NA))
  })
}

# Use S3 classes to store labels only in linestring class ----------------------

label_sf <- function(x, ...) UseMethod("label_sf")

label_sf.default <- function(x, ...) x

label_sf.sfc_LINESTRING <- function(x, label, as_textbox = FALSE) {
  attr(x, "label") <- match_labels(x, label)
  cl <- which(class(x) == "sfc_LINESTRING")
  if (!as_textbox) {
    class(x)[cl] <- "sfc_LINESTRING_labelled"
  } else {
    class(x)[cl] <- "sfc_LINESTRING_textbox"
  }
  x
}

# Does the job of actually drawing the textpaths -------------------------------

st_as_grob.sfc_LINESTRING_labelled <- function(
  x, arrow = NULL, default.units = "npc", name = NULL,
  gp = grid::gpar(), vp = NULL, textpath_vars = list(), ...) {

  label <- attr(x, "label")
  class(x)[class(x) == "sfc_LINESTRING_labelled"] <- "sfc_LINESTRING"

  if (is.null(label)) return(st_as_grob(x))
  label <- as.character(label)[1]

  if (!nzchar(label)) return(st_as_grob(x))

  is_e <- nrow_multi(unclass(x)) == 0
  if (any(is_e)) {
      gp <- gp[!is_e]
      x <- x[!is_e]
  }
  hjust          <- textpath_vars$hjust %||% 0.5
  vjust          <- textpath_vars$vjust %||% 0.5
  text_smoothing <- textpath_vars$text_smoothing %||% 0

  if (length(x)) {
      x        <- unclass(x)
      n_points <- nrow_multi(x)
      id       <- rep(seq_along(n_points), n_points)
      x        <- do.call(rbind, x)

      textpathGrob(label          = label[1],
                   x              = x[, 1],
                   y              = x[, 2],
                   id             = id,
                   arrow          = arrow,
                   hjust          = hjust,
                   vjust          = vjust,
                   default.units  = default.units,
                   name           = name,
                   gp_path        = gp,
                   gp_text        = textpath_vars$gp_text,
                   text_smoothing = text_smoothing,
                   remove_long    = TRUE,
                   vp             = vp
                   )
  } else {
    nullGrob()
  }
}

# Draws the textbox grobs
st_as_grob.sfc_LINESTRING_textbox <- function(
  x, arrow = NULL, default.units = "npc", name = NULL,
  gp = grid::gpar(), vp = NULL, textpath_vars = list(), ...) {

  label <- attr(x, "label")
  class(x)[class(x) == "sfc_LINESTRING_textbox"] <- "sfc_LINESTRING"

  if (is.null(label)) return(st_as_grob(x))

  label <- as.character(label)[1]

  if (!nzchar(label)) return(st_as_grob(x))

  is_e <- nrow_multi(unclass(x)) == 0

  if (any(is_e)) {
      gp <- gp[!is_e]
      x  <- x[!is_e]
  }
  hjust          <- textpath_vars$hjust %||% 0.5
  vjust          <- textpath_vars$vjust %||% 0.5
  text_smoothing <- textpath_vars$text_smoothing %||% 0

  if (length(x)) {
      x        <- unclass(x)
      n_points <- nrow_multi(x)
      id       <- rep(seq_along(n_points), n_points)
      x        <- do.call(rbind, x)

      textpathGrob(label          = label[1],
                   x              = x[, 1],
                   y              = x[, 2],
                   id             = id,
                   arrow          = arrow,
                   hjust          = hjust,
                   vjust          = vjust,
                   default.units  = default.units,
                   name           = name,
                   gp_path        = gp,
                   gp_text        = textpath_vars$gp_text,
                   gp_box         = textpath_vars$gp_box,
                   text_smoothing = text_smoothing,
                   remove_long    = TRUE,
                   vp             = vp,
                   as_label       = TRUE
                   )
  } else {
    nullGrob()
  }
}

# ------------------------------------------------------------------------------
# Gathers the appropriate GPs, labels linestrings and dispatches grob drawing

sf_textgrob <- function(x, lineend = "butt", linejoin = "round",
                        linemitre = 10, arrow = NULL, na.rm = TRUE,
                        as_textbox = FALSE, text_smoothing = 0) {
  # Match labels to data
  labels <- x$label %||% ""
  labels <- match_labels(x, labels)

  if ("linecolour" %in% names(x)) {
    x$linecolour <- x$linecolour %||% x$colour
  }
  if ("boxcolour" %in% names(x)) {
    x$boxcolour <- x$boxcolour %||% x$colour
  }

  # Get sf types
  type          <- sf_types[st_geometry_type(x$geometry)]
  is_point      <- type == "point"
  is_line       <- type == "line"
  is_other      <- type == "other"
  is_collection <- type == "collection"
  type_ind      <- match(type, c("point", "line", "other", "collection"))

  # Remove missing rows
  remove           <- rep_len(FALSE, nrow(x))
  remove[is_point] <- find_missing(x, GeomPoint)[is_point]
  remove[is_line]  <- find_missing(x, GeomPath)[is_line]
  remove[is_other] <- find_missing(x, GeomPolygon)[is_other]

  if (any(remove)) {
    if (!na.rm) {
      warn(paste(
        "Removed ", sum(remove), " rows containing missing values (geom_sf)."
      ))
    }
    x             <- x[!remove, , drop = FALSE]
    labels        <- labels[!remove]
    type_ind      <- type_ind[!remove]
    is_collection <- is_collection[!remove]
  }

  defaults <- sf_defaults()

  # Set gps to build grobs
  p_or_l       <- is_point | is_line
  alpha        <- x$alpha         %||% defaults$alpha[type_ind]
  col          <- x$linecolour    %||% defaults$colour[type_ind]
  col[p_or_l]  <- alpha(col[p_or_l], alpha[p_or_l])
  textcol      <- x$colour        %||% defaults$colour[type_ind]
  fill         <- alpha(x$fill    %||% defaults$fill[type_ind], alpha)
  boxcolour    <- x$boxcolour     %||% defaults$colour[type_ind]
  boxlinetype  <- x$boxlinetype   %||% defaults$linetype[type_ind]
  boxlinewidth <- (x$boxlinewidth %||% defaults$linewidth[type_ind]) * 3.779528
  boxfill      <- x$boxfill       %||% defaults$fill[type_ind]
  size         <- (x$size         %||% defaults$size[type_ind]) * .pt
  lwd          <- (x$linewidth    %||% defaults$linewidth[type_ind]) * 3.779528
  fontsize     <- size
  family       <- x$family        %||% defaults$family[type_ind]
  fontface     <- x$fontface      %||% defaults$fontface[type_ind]
  pch          <- x$shape         %||% defaults$shape[type_ind]
  lty          <- x$linetype      %||% defaults$linetype[type_ind]

  gp_line <- gpar(col       = col,
                  fill      = fill,
                  fontsize  = fontsize,
                  lwd       = lwd,
                  lty       = lty,
                  lineend   = lineend,
                  linejoin  = linejoin,
                  linemitre = linemitre)

  gp_text <- gpar(col       = textcol,
                  fontsize  = fontsize,
                  family    = family,
                  fontface  = fontface,
                  alpha     = alpha)

  gp_box  <- gpar(col       = boxcolour,
                  lty       = boxlinetype,
                  lwd       = boxlinewidth,
                  fill      = boxfill)

  # Allow extra textpath / labelpath parameters to be passed to st_as_grob
  tp_vars <- list(hjust          = x$hjust %||% 0.5,
                  vjust          = x$vjust %||% 0.5,
                  text_smoothing = text_smoothing %||% 0,
                  gp_text        = gp_text,
                  gp_box         = gp_box)
  # Build grobs
  out <- gTree()

  for (i in seq_along(x$geometry)) {
    g   <- label_sf(x$geometry[i], labels[i], as_textbox = as_textbox)
    tp_i <- tp_vars
    tp_i$gp_text <- tp_i$gp_text[i]
    tp_i$gp_box  <- tp_i$gp_box[i]
    out <- addGrob(out,
                   st_as_grob(g, pch = pch, gp = gp_line[i], arrow = arrow,
                              textpath_vars = tp_i))
  }
  out
}
