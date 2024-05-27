##---------------------------------------------------------------------------##
##                                                                           ##
##  text_helpers.R                                                           ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 - 2022 by Allan Cameron & Teun van den Brand          ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# replace zero-length characters with alternatives (useful for fallback fonts)

`%nz%` <- function(a, b) {

  a[!nzchar(a)] <- b[1]
  a
}


# Measuring --------------------------------------------------------------------

#  Wrapper for text measurement
#
# This wrap the `textshaping::shape_text()` function to return positions for
# every letter.

measure_label <- function(
    label,
    gp       = gpar(),
    ppi      = 72,
    vjust    = 0.5,
    hjust    = 0,
    halign   = "center",
    straight = FALSE,
    rich     = FALSE
) {

  n_label <- length(label)
  gp      <- rep_gp(gp_fill_defaults(gp), n_label)

  if (isTRUE(rich)) {
    rlang::check_installed(c("xml2", "markdown"), "for parsing rich text")
    label  <- parse_richtext(label, gp)
    gp_new <- attr(label, "gp")
  } else {
    label  <- data_frame(text = label, id = seq_along(label), yoff = 0)
    gp_new <- gp
  }

  straight   <- isTRUE(straight)
  measure    <- if (is.language(label$text)) {
    measure_language
  } else if (straight) {
    measure_straight
  } else {
    measure_curved
  }

  ans <- measure(
    label  = label,
    gp     = gp_new,
    ppi    = ppi,
    vjust  = vjust,
    halign = halign,
    old_gp = gp)

  attr(ans, "gp") <- attr(ans, "gp") %||% gp_new
  ans
}


measure_curved <- function(
    label,
    gp     = gpar(),
    ppi    = 72,
    vjust  = 0.5,
    hjust  = 0,
    halign = "center",
    old_gp = gpar()
) {

  label$text <- as.character(label$text)

  halign     <- match(halign, c("center", "left", "right"), nomatch = 2L)
  halign     <- c("center", "left", "right")[halign]
  nlabel     <- length(unique(label$id))
  unit_vjust <- is.unit(vjust)

  if (unit_vjust) {
    offset_unit <- rep(vjust, length.out = nlabel)
    vjust       <- 0
  }

  txt <- text_shape(label$text, label$id, gp, res = ppi, vjust = vjust,
                    hjust = hjust, align = halign)

  # We use the original gp here because the new gp may have altered font size
  # due to super/subscripts etc.
  info          <- font_info_gp(old_gp, res = ppi)
  metrics       <- txt$metrics
  txt           <- txt$shape
  txt           <- txt[order(txt$metric_id, txt$string_id), , drop = FALSE]
  txt$substring <- group_id(txt, c("metric_id", "string_id"))
  txt$letter    <- translate_glyph(txt$index, txt$substring, gp)
  txt           <- cluster_glyphs(txt)
  txt           <- filter_glyphs(txt, nlabel)
  adjust        <-  - 0.5 * info$max_ascend
  txt$y_offset  <- txt$y_offset - adjust[txt$metric_id]
  txt$y_offset  <- txt$y_offset + label$yoff[txt$substring]

  height <- gapply(txt$y_offset, txt$metric_id, function(x) diff(range(x)), 1.1)

  metrics$height     <- height + info$max_ascend - info$max_descend
  metrics$lineheight <- info$lineheight

  ans <- data_frame(
    glyph     =  txt$letter,
    ymin      =  txt$y_offset,
    xmin      =  txt$x_offset,
    xmid      = (txt$x_offset + txt$x_midpoint),
    xmax      = (txt$x_offset + txt$x_midpoint * 2),
    substring = group_id(txt, c("metric_id", "string_id"))
  )

  ans <- split(ans, txt$metric_id)
  ans <- lapply(seq_along(ans), function(i) {
    df <- ans[[i]]
    offset  <- unique(c(0, df$ymin))
    df$y_id <- match(df$ymin, offset)
    if (unit_vjust) {
      df$y_id <- df$y_id + 1
      offset  <- unit(offset, "inch")
      offset  <- unit.c(unit(0, "inch"), offset + offset_unit[i])
    }
    df$ymin <- as_inch(offset[df$y_id])
    attr(df, "metrics") <- metrics[i, , drop = FALSE]
    attr(df, "offset")  <- offset
    df
  })

  ans
}



measure_language <- function(label, gp = gpar(), ppi = 72, vjust = 0.5, ...) {

  width  <- measure_text_dim(label$text, gp, "width")
  height <- measure_text_dim(label$text, gp, "height")
  ymin   <- - (height * (vjust - 0.5))

  Map(
    function(label, width, height) {
      ans <- data_frame(
        glyph = list(label),
        xmin  = 0,
        xmid  =  width / 2,
        xmax  = width,
        y_id  = 1L
      )
      attr(ans, "offset")  <- ymin
      attr(ans, "metrics") <- data_frame(
        width  = width,
        height = height,
        x_adj  = -0.5 * height
      )
      ans
    },
    label  = as.list(label$text),
    width  = width,
    height = height
  )
}


# This first calls the curved variant, then restructures the data

measure_straight <- function(...) {

  meas   <- measure_curved(...)
  meas[] <- lapply(meas, function(df) {
    metrics <- attr(df, "metrics")
    offsets <- attr(df, "offset")
    inchoff <- as_inch(offsets)
    i       <- unique(df$y_id)
    out     <- data_frame(
          glyph     = list(df$glyph),
          y_id      = which(inchoff == min(inchoff[i])),
          xmin      = 0,
          xmid      = 0.5 * metrics$width,
          xmax      = metrics$width,
          xoffset   = list(df$xmid - metrics$width / 2),
          yoffset   = list(df$ymin - min(inchoff[i])),
          substring = list(df$substring)
        )
    attr(out, "metrics") <- metrics
    attr(out, "offset")  <- offsets
    out
  })
  meas
}


# Glyph utilities --------------------------------------------------------------

# Here a cache that stores index lookup tables of fonts

index_cache <- new.env(parent = emptyenv())


# This function retrieves index lookup tables for fonts. If the font has been
# seen before, it is retrieved from the cache. If not, it looks up the index
# table and stores it in the cache.

glyph_index <- function(family = "") {

  # Empty character is invalid name
  name <- if (family == "") "fallback" else family

  # Retrieve from cache if it exists there
  if (name %in% names(index_cache)) return(index_cache[[name]])

  # Get all unicode characters
  idx     <- intToUtf8(1:65535, multiple = TRUE)
  idx     <- systemfonts::glyph_info(idx, family = name)
  idx$int <- 1:65535

  # Filter invalid glyphs
  idx <- idx[idx$index > 0 & !is.na(idx$glyph), c("index", "int")]

  # Format such that index + 1 gives correct answer
  ans <- rep(0L, max(idx$index) + 1)
  ans[idx$index + 1] <- idx$int

  # Store in cache
  index_cache[[name]] <- ans
  ans
}


# This function does the translation of font indices to glyphs. Note that
# different strings can have different fonts, which is why we need to loop
# over the indices.

translate_glyph <- function(index, id, gp = gpar()) {

  ints <- lapply(gp$fontfamily %nz% "fallback", glyph_index)
  split(index, id) <- Map(
    function(int, idx) int[idx + 1],
    int = ints,
    idx = split(index, id)
  )
  intToUtf8(index, multiple = TRUE)
}


cluster_glyphs <- function(shape, vars = c("glyph", "metric_id", "string_id")) {

  shape$clusters <- group_id(shape, vars)
  shape$letter   <- ave(
    shape$letter, shape$clusters,
    FUN = function(x) paste0(x, collapse = "")
  )
  shape$x_midpoint <- ave(shape$x_midpoint, shape$clusters, FUN = max)
  shape
}


filter_glyphs <- function(shape, n, forbidden = c("\r", "\n", "\t", "")) {

  keep  <- shape$letter %in% forbidden
  keep  <- !(keep | duplicated(shape$clusters))
  shape <- shape[keep, , drop = FALSE]

  if (length(unique(shape$metric_id)) != n) {
    if (nrow(shape)) {
      warn("Not all glyphs for the labels could be retrieved.")
    } else {
      abort("No glyphs could be retrieved for these labels.")
    }
  }
  shape
}


# Rich text --------------------------------------------------------------------

parse_richtext <- function(
  text,
  gp,
  md    = TRUE,
  id    = seq_along(text),
  inner = FALSE
) {

  text <- as.character(text)

  if (!inner) {
    # If text is multiple labels, loop myself
    gps    <- split_gp(gp, seq_along(text))
    ans    <- Map(f     = parse_richtext,
                  text  = text,
                  gp    = gps,
                  md    = md,
                  id    = id,
                  inner = TRUE)
    ans    <- rbind_dfs(ans)
    ans_gp <- setdiff(names(ans), c("text", "id", "yoff"))
    ans_gp <- do.call(gpar, ans[ans_gp])
    attr(ans, "gp") <- ans_gp
    return(ans)

  }

  old_gp <- gp

  if (md) {
    text <- markdown::mark(text = text)
  }
  # Deal with <br> now, they are a pain to handle after parsing
  text <- gsub("<br>", "\n", text, fixed = TRUE)

  doc             <- xml2::read_html(paste0("<!DOCTYPE html>", text))
  doc             <- xml2::as_list(doc)$html$body
  drawing_context <- setup_context(gp = gp)
  processed       <- process_tags(doc, drawing_context)
  strings         <- unlist(processed[, 1])
  gp              <- processed[, 2]
  yoff            <- unlist(processed[, 3])

  gp <- lapply(gp, function(x) do.call(data_frame, unclass(x)))
  gp <- rbind_dfs(gp)

  data_frame(
    text       = strings,
    id         = id,
    fontfamily = gp$fontfamily,
    fontsize   = gp$fontsize,
    font       = gp$font,
    lineheight = gp$lineheight,
    tracking   = old_gp$tracking[1] %||% 0,
    col        = gp$col,
    yoff       = yoff
  )
}


# Helpers -----------------------------------------------------------------

font_info_gp <- function(gp = gpar(), res = 72, unit = "inch") {

  info <- systemfonts::font_info(
    family =  gp$fontfamily %nz% "fallback",
    italic = (gp$font       %||% 1) %in% c(3, 4),
    bold   = (gp$font       %||% 1) %in% c(2, 4),
    size   =  gp$fontsize   %||% 12,
    res    = res
  )
  adj       <- resolution_to_unit(res = res, unit = unit)
  info$bbox <- lapply(info$bbox, `*`, adj)
  vars <- c("max_ascend", "max_descend",
            "max_advance_width", "max_advance_height", "lineheight",
            "underline_pos", "underline_size")
  info[, vars] <- info[, vars] * adj
  info
}


x_height <- function(gp) {

  len <- max(lengths(gp))
  systemfonts::string_metrics_dev(
    rep("x", len),
    family = gp$fontfamily %nz% "fallback",
    face   = gp$fontface     %||% 1,
    size   = gp$fontsize     %||% 12, unit = "inches"
  )$ascent
}


# Takes care of default gpar() settings and translates units from pixels

text_shape <- function(text, id, gp, res = 72, vjust = 0.5, hjust = 0.5,
                       align = "center", unit = "inch") {

  bold <- ifelse((gp$font %||% 1) %in% c(2, 4), "bold", "normal")
  italic <- (gp$font %||% 1) %in% c(3, 4)

  font_path <- systemfonts::match_fonts(
    gp$fontfamily %||% "", italic = italic, weight = bold
  )
  font_info <- systemfonts::font_info(path = font_path$path)

  txt <- list(
    strings    =  text,
    path       =  font_info$path,
    size       =  gp$fontsize   %||% 12,
    italic     =  italic,
    weight     =  bold,
    lineheight =  gp$lineheight %||% 1.2,
    tracking   =  gp$tracking   %||% 0,
    width      =  font_info$width,
    id         =  id,
    res = res, vjust = vjust, hjust = hjust, align = align
  )
  txt         <- do.call(shape_text, txt)
  txt$shape$x_midpoint <- txt$shape$advance / 2
  adj         <- resolution_to_unit(res = res, unit = unit)
  shape_vars  <- c("x_offset", "y_offset", "x_midpoint")

  metric_vars <- c("width", "height", "left_bearing", "right_bearing",
                   "top_bearing", "left_border", "top_border", "pen_x", "pen_y")
  txt$shape[, shape_vars]    <- txt$shape[, shape_vars]    * adj
  txt$metrics[, metric_vars] <- txt$metrics[, metric_vars] * adj
  txt
}


resolution_to_unit <- function(res = 72, unit = "inch") {

  convertUnit(unit(1, "inch"), unitTo = unit, valueOnly = TRUE) / res
}


measure_text_dim <- function(labels, gp = gpar(), dim = "height") {

  dimfun <- list(height = grobHeight, width  = grobWidth)
  gp     <- lapply(seq_along(labels), function(i) recycle_gp(gp, `[`, i))
  grobs  <- Map(textGrob, gp = gp, label = labels)
  ans    <- do.call(unit.c, lapply(grobs, dimfun[[dim]]))
  as_inch(ans, dim)
}
