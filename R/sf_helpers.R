##---------------------------------------------------------------------------##
##                                                                           ##
##  sf_helpers.R                                                             ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 - 2022 by Allan Cameron & Teun van den Brand          ##
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

  point_aes          <- GeomPoint$default_aes
  textpath_aes       <- GeomTextpath$default_aes
  polygon_aes        <- GeomPolygon$default_aes
  polygon_aes$fill   <- "grey90"
  polygon_aes$colour <- "grey35"
  collection_aes     <- rename(textpath_aes,
                               c(size = "point_size", fill = "point_fill"))
  for (i in names(polygon_aes)) {
    collection_aes[[i]] <- polygon_aes[[i]]
  }

  defaults <- list(point_aes, textpath_aes, polygon_aes, collection_aes)

  default_names <- unique(unlist(lapply(defaults, names)))

  lapply(setNames(default_names, default_names), function(n) {
    unlist(lapply(defaults, function(def) def[[n]] %||% NA))
  })
}


# Store labels only in linestring class ----------------------

# Avoided the S3 route here because generics and methods needed to be exported.
# Dispatch is simple enough to go functional instead of OOP

label_sf <- function(x, label = "", as_textbox = FALSE) {

  if (!inherits(x, "sfc_LINESTRING")) {
    return(x)
  }
  attr(x, "label") <- match_labels(x, label)
  cl <- which(class(x) == "sfc_LINESTRING")
  if (!as_textbox) {
    class(x)[cl] <- "sfc_labelled"
  } else {
    class(x)[cl] <- "sfc_textbox"
  }
  x
}


# Does the job of actually drawing the textpaths -------------------------------

st_as_grob.sfc_labelled <- function(
  x,
  arrow         = NULL,
  default.units = "npc",
  name          = NULL,
  gp            = gpar(),
  vp            = NULL,
  textpath_vars = list(),
  ...
) {

  label <- attr(x, "label")
  class(x)[class(x) == "sfc_labelled"] <- "sfc_LINESTRING"

  if (is.null(label)) return(sf::st_as_grob(x))
  label <- as.character(label)[1]

  if (!nzchar(label)) return(sf::st_as_grob(x))

  is_e <- nrow_multi(unclass(x)) == 0
  if (any(is_e)) {
      gp <- gp[!is_e]
      x <- x[!is_e]
  }
  params         <- textpath_vars$text_params
  hjust          <- textpath_vars$hjust %||% 0.5
  vjust          <- params$offset %||% textpath_vars$vjust %||% 0.5

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
                   halign         = params$halign %||% "left",
                   gap            = params$gap %||% NA,
                   default.units  = default.units,
                   name           = name,
                   gp_path        = gp,
                   gp_text        = textpath_vars$gp_text,
                   straight       = params$straight %||% FALSE,
                   upright        = params$upright %||% TRUE,
                   text_smoothing = params$text_smoothing %||% 0,
                   padding        = params$padding %||% unit(0.05, "inch"),
                   rich           = params$rich %||% FALSE,
                   remove_long    = params$remove_long %||% TRUE,
                   vp             = vp
                   )
  } else {
    nullGrob()
  }
}


# Draws the textbox grobs

st_as_grob.sfc_textbox <- function(
  x,
  arrow         = NULL,
  default.units = "npc",
  name          = NULL,
  gp            = gpar(),
  vp            = NULL,
  textpath_vars = list(),
  ...
) {

  label <- attr(x, "label")
  class(x)[class(x) == "sfc_textbox"] <- "sfc_LINESTRING"

  if (is.null(label)) return(sf::st_as_grob(x))

  label <- as.character(label)[1]

  if (!nzchar(label)) return(sf::st_as_grob(x))

  is_e <- nrow_multi(unclass(x)) == 0

  if (any(is_e)) {
      gp <- gp[!is_e]
      x  <- x[!is_e]
  }
  params         <- textpath_vars$text_params
  hjust          <- textpath_vars$hjust %||% 0.5
  vjust          <- params$offset %||% textpath_vars$vjust %||% 0.5

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
                   halign         = params$halign %||% "left",
                   gap            = params$gap %||% NA,
                   default.units  = default.units,
                   name           = name,
                   gp_path        = gp,
                   gp_text        = textpath_vars$gp_text,
                   gp_box         = textpath_vars$gp_box,
                   straight       = params$straight %||% FALSE,
                   upright        = params$upright %||% TRUE,
                   text_smoothing = params$text_smoothing %||% 0,
                   padding        = params$padding %||% unit(0.05, "inch"),
                   rich           = params$rich %||% FALSE,
                   remove_long    = params$remove_long %||% TRUE,
                   vp             = vp,
                   as_label       = TRUE
                   )
  } else {
    nullGrob()
  }
}


# ------------------------------------------------------------------------------
# Gathers the appropriate GPs, labels linestrings and dispatches grob drawing

sf_textgrob <- function(
  x,
  lineend        = "butt",
  linejoin       = "round",
  linemitre      = 10,
  arrow          = NULL,
  na.rm          = TRUE,
  as_textbox     = FALSE,
  text_params    = static_text_params("text")
) {

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
  type          <- sf_types[sf::st_geometry_type(x$geometry)]
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
  family       <- family          %nz% "fallback"
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

  gp_text <- gpar(col           = alpha(textcol, alpha),
                  fontsize      = fontsize,
                  fontfamily    = family,
                  fontface      = fontface)

  gp_box  <- gpar(col       = boxcolour,
                  lty       = boxlinetype,
                  lwd       = boxlinewidth,
                  fill      = boxfill)

  # Allow extra textpath / labelpath parameters to be passed to st_as_grob
  tp_vars <- list(text_params    = text_params,
                  gp_text        = gp_text,
                  gp_box         = gp_box)
  # Build grobs
  out <- gTree()

  for (i in seq_along(x$geometry)) {
    g   <- label_sf(x$geometry[i], labels[i], as_textbox = as_textbox)
    tp_i <- tp_vars
    tp_i$hjust <- x$hjust[i] %||% 0.5
    tp_i$vjust <- x$vjust[i] %||% 0.5
    tp_i$gp_text <- tp_i$gp_text[i]
    tp_i$gp_box  <- tp_i$gp_box[i]
    out <- addGrob(out,
                   sf::st_as_grob(g, pch = pch, gp = gp_line[i], arrow = arrow,
                              textpath_vars = tp_i))
  }
  out
}
