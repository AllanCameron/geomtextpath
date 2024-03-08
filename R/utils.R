##---------------------------------------------------------------------------##
##                                                                           ##
##  utils.R                                                                  ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 - 2022 by Allan Cameron & Teun van den Brand          ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# simplify calls to vapoly -----------------------------------------------------

# Many vapply calls in the code base have a FUN.VALUE of numeric(1). This
# wrapper simply helps keep the code more legible / maintainable.

numapply <- function(data, fun) {

  vapply(data, FUN = fun, FUN.VALUE = numeric(1))
}


# Wrapper for the frequent use case of getting a vector of the number of rows
# of several data frames in a list.

nrow_multi <- function(data) {

  vapply(data, FUN = nrow, FUN.VALUE = integer(1), USE.NAMES = FALSE)
}


# Run length utilities ---------------------------------------------------------

# Gives the indices of a vector where a run length starts

run_start <- function(x) {

  x <- discretise(x)
  c(0, which(diff(x) != 0)) + 1
}


# Simplified rle(x)$lengths

run_len <- function(x) {

  diff(c(run_start(x), length(x) + 1))
}


# Utilities for data.frames ----------------------------------------------------

# Cheaper data.frame constructor for internal use.
# Only use when `...` has valid names and content are valid data.frame columns

data_frame <- function(...) {

  list_to_df(x = list(...))
}


# Similar in scope to base::list2DF or ggplot2:::new_data_frame or
# vctrs::df_list
# without bothering with 'n'/'size'

list_to_df <- function(x = list()) {

  if (length(x) != 0 && is.null(names(x))) {
    stop("Elements must be named", call. = FALSE)
  }

  lengths <- lengths(x)
  n       <- max(lengths)

  for (i in seq_along(x)) {
    if (lengths[i] == n)
      next
    if (lengths[i] != 1) {
      stop("Elements must equal the number of rows or 1", call. = FALSE)
    }
    x[[i]] <- rep(x[[i]], n)
  }
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(n)
  x
}


# Row-bind a list of data.frames
# `df_list` is a list of data.frames
# `idcol` is a name for an id column, or NULL if it is to be omitted

rbind_dfs <- function(df_list) {

  do.call(rbind.data.frame, c(df_list, make.row.names = FALSE))
}


# Grouping utilities -----------------------------------------------------------

#  Convert unique values to ordered sequence of integers

discretise <- function(x) {

  match(x, unique(x))
}


# A multiple variable equivalent of discretise

group_id <- function(data, vars) {
  id <- lapply(data[vars], discretise)
  id <- do.call(paste, c(list(sep = "&"), id))
  discretise(id)
}


# Shorthand for `vapply(split(x, group), ...)`

gapply <- function(x, group, FUN, FUN.VALUE, ..., USE.NAMES = FALSE) {

  vapply(
    split(x, group),
    FUN = FUN, FUN.VALUE = FUN.VALUE,
    ...,
    USE.NAMES = USE.NAMES
  )
}


# Approx utilities -------------------------------------------------------------

# Function for `approx()`-ing a number of `y` variables. More efficient
# than looping `approx()` due to not having to recalculate indices every
# iteration. For non-numeric values, repeats first entry to match length.
# Note: This also extrapolates based on the four extreme points.

approx_multi <- function(x, y = matrix(), xout) {

  if (length(y) == 0) return(y)

  # Coerce lists and data.frames to matrices
  listmode <- is.list(y)

  if (listmode) {
    lens     <- lengths(y)
    stopifnot(
      "All elements in `y` must have the same length as `x`" =
        all(lens == length(x))
    )
    orig          <- unclass(y)
    is_num        <- vapply(orig, is.numeric, logical(1))
    orig[!is_num] <- lapply(lapply(orig[!is_num], `[`, 1),
                            rep, length.out = length(xout))
    y             <- do.call(cbind, y[is_num])
  }
  # Assign a dimension if there is none
  dimless <- is.null(dim(y))
  if (dimless) dim(y) <- c(length(y), 1L)

  # Checks
  stopifnot(
    "`y` must be numeric." = is.numeric(y),
    "`y` must have a compatible length with `x`" = nrow(y) == length(x))

  # Find indices
  i <- findInterval(xout, x, all.inside = TRUE)
  d <- (xout - x[i]) / (x[i + 1] - x[i])

  # Interpolate
  out <- y[i, , drop = FALSE] * (1 - d) + y[i + 1, , drop = FALSE] * d

  # Restore data.frame/lists
  if (listmode) {
    orig[is_num] <- split(out, col(out))
    out <- list_to_df(orig)
  }
  # Drop dimensions if `y` didn't have dimensions at the beginning
  if (dimless) out <- drop(out)

  out
}


# Missingness utilities --------------------------------------------------------

# Simple linear interpolation for NA values

interp_na <- function(x) {

  if (!anyNA(x)) return(x)
  stopifnot("Cannot interpolate NA in non-numeric vectors" = is.numeric(x))
  x[] <- approx(seq_along(x), x, seq_along(x))$y
  x
}


# Re-implementation of unexported ggplot function

cases <- function(x, fun) {

  ok <- vapply(x, fun, logical(nrow(x)))
  if (is.vector(ok)) {
    all(ok)
  } else {
    rowSums(as.matrix(ok)) == ncol(x)
  }
}


# Consistent missingness check for different input types

is_missing <- function(x) {

  if (typeof(x) == "list") !vapply(x, is.null, logical(1)) else !is.na(x)
}


# Consistent infinity check for different input types

is_finite <- function(x) {

  if (typeof(x) == "list") !vapply(x, is.null, logical(1)) else is.finite(x)
}


# Finds rows with missing data in given data frame

detect_missing <- function(df, vars, finite = FALSE) {

    vars <- intersect(vars, names(df))
    !cases(df[, vars, drop = FALSE], if (finite) is_finite else is_missing)
}


# Finds missing aesthetics in input data frame

find_missing <- function(x, layer) {

  detect_missing(x, c(layer$required_aes, layer$non_missing_aes))
}


# Label utilities --------------------------------------------------------------

# Ensure label vector matches to given object

match_labels <- function(x, labels) {

  if(is.data.frame(x)) {
    if (length(labels) == 1) labels <- rep(labels, nrow(x))
    if (nrow(x) != length(labels)) {
      stop("Could not match labels to object ", deparse(substitute(x)))
    }
    return(labels)
  }
  if (length(labels) == 1) labels <- rep(labels, length(x))
  if (length(x) != length(labels)) {
    stop("Could not match labels to object ", deparse(substitute(x)))
  }
  labels
}

# Similar to ggplot's rename function

rename <- function(x, replace) {

  current_names <- names(x)
  old_names     <- names(replace)
  missing_names <- setdiff(old_names, current_names)

  if (length(missing_names) > 0) {
    replace   <- replace[!old_names %in% missing_names]
    old_names <- names(replace)
  }
  names(x)[match(old_names, current_names)] <- as.vector(replace)

  x
}

# Text utilities ---------------------------------------------------------------

# Parse characters as expressions with validity checks

safe_parse <- function(text) {

  if (!is.character(text)) stop("`text` must be a character vector")
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
      expr     <- parse(text = text[[i]])
      out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}


# Logical test to determine whether a label is to be considered as multiple
# glyphs or as a single unit

is.multichar <- function(x) {

  if (is.list(x)) return(any(vapply(x, is.multichar, logical(1))))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(any(nchar(x) > 1))
  is.language(x)
}


# Based on ggplot2:::draw_axis handling of labels

make_label <- function(x) {

  if (!is.list(x)) return(x)
  if (any(vapply(x, is.language, logical(1)))) {
    do.call(expression, x)
  } else {
    unlist(x, FALSE, FALSE)
  }
}


# Allows information about co-ordinate system to be passed to grob

get_polar_params <- function(coord, params) {

  if (inherits(coord, "CoordPolar")) {
    list(x = 0.5, y = 0.5, theta = coord$theta)
  } else if (inherits(coord, "CoordRadial")) {
    list(
      x = rescale(0.5, from = params$bbox$x),
      y = rescale(0.5, from = params$bbox$y),
      theta = coord$theta
    )
  } else {
    NULL
  }
}


# Grid utilities ---------------------------------------------------------------

# Convert internal ggplot-type aesthetic data frame to gp object for text

data_to_text_gp <- function(data) {

  gpar(
    col        = alpha(data$textcolour %||% data$colour, data$alpha),
    fontsize   = data$size * .pt,
    fontface   = data$fontface %||% data$font,
    fontfamily = data$family,
    lineheight = data$lineheight,
    tracking   = data$spacing
  )
}


# Convert internal ggplot-type aesthetic data frame to gp object for lines

data_to_path_gp <- function(
  data,
  lineend   = "butt",
  linejoin  = "round",
  linemitre = 10) {

  if (all(data$linetype %in% c("0", "blank", NA))) {
    gpar(lty = 0)
  } else {
    gpar(
      col       = alpha(data$linecolour %||% data$colour, data$alpha),
      fill      = alpha(data$linecolour %||% data$colour, data$alpha),
      lwd       = data$linewidth * .pt,
      lty       = data$linetype,
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre
    )
  }
}


# Convert internal ggplot-type aesthetic data frame to gp object for lines

data_to_box_gp <- function(
  data,
  lineend   = "butt",
  linejoin  = "round",
  linemitre = 10) {

  gpar(
    col       = alpha(data$boxcolour %||% data$linecolour %||% data$colour,
                      data$alpha),
    fill      = alpha(data$fill, data$alpha),
    lwd       = data$boxlinewidth %||% data$linewidth * .pt,
    lty       = data$boxlinetype  %||% data$linetype,
    lineend   = lineend,
    linejoin  = linejoin,
    linemitre = linemitre
  )
}


# Helper function to do safe(r) recycling on "gpar" class objects.

recycle_gp <- function(gp, fun, ...) {

  do_recycle     <- lengths(gp) > 1
  gp[do_recycle] <- lapply(unclass(gp)[do_recycle], fun, ...)
  gp[lengths(gp) == 0] <- list(NULL)
  return(gp)
}


# Safely repeat values inside a gpar object

rep_gp <- function(gp, length.out = max(lengths(gp))) {

  gp[] <- lapply(gp, rep, length.out = length.out)
  gp
}


# Split gpar into llist of ength-1 gpars

split_gp <- function(gp, i = seq_len(max(lengths(gp)))) {

  gp <- rep_gp(gp, max(i))
  lapply(i, function(j) recycle_gp(gp, `[`, j))
}


# Helper function to fill in missing parameters by defaults
# Based on ggplot2:::modify_list

gp_fill_defaults <- function(gp, ..., defaults = get.gpar()) {

  extra <- list(...)
  for (i in names(extra)) defaults[[i]] <- extra[[i]]
  for (i in names(gp))    defaults[[i]] <- gp[[i]]
  defaults
}


# Safely subset gpar

gp_subset <- function(gp, ss) {

  subset_these         <- lengths(gp) > 1
  gp[subset_these]     <- lapply(unclass(gp)[subset_these], function(x) x[ss])
  gp[lengths(gp) == 0] <- list(NULL)
  return(gp)
}


# Convert grid unit object to given unit type, then make numeric

as_grid_unit <- function(value, from = "x", unit = "native") {

  if (!is.unit(value)) return(value)

  axis <- list(x = "x", y = "y", width = "x", height = "y")
  type <- list(x = "location", y = "location",
               width = "dimension", height = "dimension")

  convertUnit(x         = value,
              unitTo    = unit,
              axisFrom  = axis[[from]],
              typeFrom  = type[[from]],
              valueOnly = TRUE)
}


# Convert grid units to number of inches

as_inch <- function(x, from = "x") {

  as_grid_unit(x, from, "inch")
}


# Convert grid units to numeric npc values

as_npc <- function(x, from = "x") {

  as_grid_unit(x, from, "npc")
}


# Same as grid::unit, but allows unit objects to safely pass through

as_unit <- function(x, units = NULL, ...) {

  if (!is.unit(x) && !is.null(units)) {
    x <- unit(x, units, ...)
  }
  x
}


# Documentation helpers --------------------------------------------------------

# Simple lowercase converter

as_lower <- function(x) {

  chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
}


# Simple uppercase converter

as_upper <- function(x) {

  chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", x)
}


# Convert snake_case to CamelCase

camelize <- function(x, first = FALSE) {

  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) {
      x <- paste0(as_upper(substring(x, 1, 1)), substring(x, 2))
  }
  x
}


# Find object in given environment with fallback to package namespace

find_global <- function(name, env, mode = "any") {

  if (exists(name, envir = env, mode = mode)) {
      return(get(name, envir = env, mode = mode))
  }
  nsenv <- asNamespace("geomtextpath")
  if (exists(name, envir = nsenv, mode = mode)) {
      return(get(name, envir = nsenv, mode = mode))
  }
  NULL
}

# Finds object of given type and class (as characters if necessary)

check_subclass <- function(
  x,
  subclass,
  argname = as_lower(subclass),
  env     = parent.frame()
  ) {

  if (inherits(x, subclass)) {
    x
  }
  else if (is.character(x) && length(x) == 1) {
    name <- paste0(subclass, camelize(x, first = TRUE))
    obj  <- find_global(name, env = env)
    if (is.null(obj) || !inherits(obj, subclass)) {
        abort(paste0("Can't find `", argname, "` called '", x, "'"))
    } else {
      obj
    }
  }
  else {
    abort(paste0("`", argname, "` must be either a string or a ",
                 subclass, " object"))
  }
}


# Documentation functions adapted from ggplot2 ---------------------------------

# Adds each aesthetic to output in rd_aesthetics

rd_aesthetics_item <- function(x) {

  req          <- x$required_aes
  req          <- sub("|", "} \\emph{or} \\code{", req, fixed = TRUE)
  req_aes      <- unlist(strsplit(x$required_aes, "|", fixed = TRUE))
  optional_aes <- setdiff(x$aesthetics(), req_aes)
  all          <- union(req, sort(optional_aes))

  ifelse(test = all %in% req,
         yes  = paste0("\\strong{\\code{", all, "}}"),
         no   = paste0("\\code{", all, "}"))
}


# Calculate aesthetics and produce documentation item for given function / topic

rd_aesthetics <- function(type, name, check_label_variant = TRUE) {

  obj <- switch(type,
                geom = check_subclass(name, "Geom", env = globalenv()),
                stat = check_subclass(name, "Stat", env = globalenv()))
  aes <- rd_aesthetics_item(obj)
  txt <- "@section Aesthetics:"
  txt <- c(txt,
           paste0("\\code{", type,
                  "_", name, "()} ",
                  "understands the following aesthetics ",
                  "(required aesthetics are in bold):"))
  txt <- c(txt, "\\itemize{", paste0("  \\item ", aes), "}")

  lab_aes <- NULL
  if (check_label_variant) {
    # Check if there is 'text' to be substituted by 'label'
    lab_name <- gsub("^text", "label", name)
    if (!(lab_name == name)) {
      # Check if label variant exists
      lab_obj <- tryCatch(
        {
          switch(type,
                 geom = check_subclass(lab_name, "Geom", env = globalenv()),
                 stat = check_subclass(lab_name, "Stat", env = globalenv()))
        },
        error   = function(cond) {return(NULL)}
      )
      if (!is.null(lab_obj)) {
        lab_aes <- rd_aesthetics_item(lab_obj)
        lab_aes <- setdiff(lab_aes, aes)
      }
      if (length(lab_aes)) {
        txt <- c(txt,
                 paste0("In addition to aforementioned aesthetics,",
                        " \\code{", type, "_", lab_name, "()} ",
                        "also understands:"))
        txt <- c(txt, "\\itemize{", paste0("  \\item ", lab_aes), "}")
      }
    }
  }

  if (any(grepl("spacing", aes))) {
    txt <- c(txt,
             "The \\code{spacing} aesthetic allows fine control of spacing",
             " of text, which is called 'tracking' in typography.",
             "The default is 0 and units are measured in 1/1000 em.",
             "Numbers greater than zero increase the spacing,",
             "whereas negative numbers decrease the spacing.")
  }

  txt <- c(txt,
           "\n\nLearn more about setting these aesthetics ",
           "in \\code{vignette(\"ggplot2-specs\")}.")
  txt
}


# Automatically put the text-path arguments under the `...` description in the
# documentation.
# Checks whether a parameter is formally declared in the function
# and if it is, will not put it in the ellipsis text. You can explicitly exclude
# a parameter from documentation, e.g. if "gap" most definitely does not apply
# to some geom or that kind of situation.

rd_dots <- function(fun, exclude = character()) {

  exclude <- c(exclude, ".type")
  params  <- names(formals(static_text_params))
  forms   <- names(formals(fun))
  exclude <- union(exclude, forms)

  txt <- paste0(
    "@param ... Other arguments passed on to ",
    "\\code{\\link[ggplot2:layer]{layer}}. ",
    "These are often aesthetics, used to set an aesthetic to a fixed value, ",
    'like \\code{colour = "red"} or \\code{size = 3}.'
  )

  if (all(params %in% exclude)) {
    return(txt)
  }

  file <- system.file("R", "text_params.R", package = "geomtextpath")

  # Use roxygen2 to parse text params
  doc  <- roxygen2::parse_file(file)

  # Look for the doc with the "rd_dots" keyword, this should be in the
  # `static_text_params()` function above.
  keep <- vapply(doc, function(x) {
    keywords <- roxygen2::block_get_tag_value(x, "keywords")
    isTRUE(grepl("rd_dots", keywords))
  }, logical(1))
  doc  <- doc[[which(keep)[1]]]

  # Extract `@param` statements from the function's documentation
  tags <- roxygen2::block_get_tags(doc, "param")

  # Separate the param names from description
  tag_names <- vapply(tags, function(x) x$val$name, character(1))
  tag_descr <- vapply(tags, function(x) x$val$description, character(1))

  # Format such that they form new valid params
  tag_descr <- gsub("(\r\n|\r|\n)", " ", tag_descr)
  fmt_tags  <- paste0("\\item{\\code{", tag_names, "}}{",
                      tag_descr, "}")
  fmt_tags  <- fmt_tags[!(tag_names %in% exclude)]
  fmt_tags  <- paste0(fmt_tags, collapse = "")

  paste0(
    txt,
    " These can also be the following text-path parameters:",
    "\\describe{",
    fmt_tags,
    "}"
  )
}


# Required for reference line geoms as parameters can overwrite aesthetics

warn_overwritten_args <- function (
  fun_name,
  overwritten_arg,
  provided_args,
  plural_join = " and/or "
  ) {

  overwritten_arg_text <- paste0("`", overwritten_arg, "`")
  n_provided_args      <- length(provided_args)
  if (n_provided_args == 1) {
      provided_arg_text <- paste0("`", provided_args,
          "`")
      verb <- "was"
  }
  else if (n_provided_args == 2) {
      provided_arg_text <- paste0("`", provided_args,
          "`", collapse = plural_join)
      verb <- "were"
  }
  else {
      provided_arg_text <- paste0(paste0("`", provided_args[-n_provided_args],
          "`", collapse = ", "), ",", plural_join,
          "`", provided_args[n_provided_args], "`")
      verb <- "were"
  }
  warn(paste0(fun_name,
              ": Ignoring ", overwritten_arg_text, " because ",
              provided_arg_text, " ", verb, " provided."))
}
