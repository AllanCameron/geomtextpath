
# simplify calls to vapoly with numeric vapply and integer vapply

numapply <- function(data, fun) {

  vapply(data, FUN = fun, FUN.VALUE = numeric(1))
}

nrow_multi <- function(data) {

  vapply(data, FUN = nrow, FUN.VALUE = integer(1), USE.NAMES = FALSE)
}

# Run length utilities ----------------------------------------------------

# Simplified `rle(x)$lengths`
run_len <- function(x) {
  if ({n <- length(x)} < 2) {
    return(n)
  }
  if (anyNA(x)) {
    x[is.na(x)] <- "NA" # Type conversion is worth safety against NAs
  }
  x <- c(which(x[-1] != x[-n]), n)
  diff(c(0L, x))
}

run_start <- function(x, is_lengths = FALSE) {
  if (!is_lengths) {
    x <- run_len(x)
  }
  cumsum(x) - x + 1L
}

run_end <- function(x, is_lengths = FALSE) {
  if (!is_lengths) {
    x <- run_len(x)
  }
  cumsum(x)
}

# Utilities for data.frames -----------------------------------------------

# Cheaper data.frame constructor for internal use.
# Only use when `...` has valid names and content are valid data.frame columns
data_frame <- function(...) {
  list_to_df(x = list(...))
}

# Similar in scope to base::list2DF or ggplot2:::new_data_frame  or
# vctrs::df_list
# without bothering with 'n'/'size'
list_to_df <- function(x = list()) {
  if (length(x) != 0 && is.null(names(x))) {
    stop("Elements must be named", call. = FALSE)
  }
  lengths <- lengths(x)
  n <- max(lengths)
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
rbind_dfs <- function(df_list, idcol = NULL) {
  # Ideally, we'd use vctrs::vec_c(!!!df_list) which is 10x faster
  ans <- do.call(rbind.data.frame, c(df_list, make.row.names = FALSE))
  if (!is.null(idcol)) {
    n <- nrow_multi(df_list)
    ans[[idcol]] <- rep(names(df_list) %||% seq_along(n), times = n)
  }
  ans
}

# Grouping utilities ------------------------------------------------------

discretise <- function(x) {
  match(x, unique(x))
}

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

# Approx utilities --------------------------------------------------------

# Function for `approx()`-ing a number of `y` variables. More efficient
# than looping `approx()` due to not having to recalculate indices every
# iteration. For non-numeric values, repeats first entry to match length.
# Note: This also extrapolates based on the four extreme points.

approx_multiple <- function(x, xout, y = matrix()) {
  if (length(y) == 0) {
    return(y)
  }
  # Coerce lists and data.frames to matrices
  if ({listmode <- is.list(y)}) {
    is_df    <- is.data.frame(y)
    lens     <- lengths(y)
    stopifnot(
      "All elements in `y` must have the same length as `x`" =
        all(lens == length(x))
    )
    orig     <- unclass(y)
    is_num   <- vapply(orig, is.numeric, logical(1))
    orig[!is_num] <- lapply(lapply(orig[!is_num], `[`, 1),
                            rep, length.out = length(xout))
    y <- do.call(cbind, y[is_num])
  }
  # Assign a dimension if there is none
  if ({dimless <- is.null(dim(y))}) {
    dim(y) <- c(length(y), 1L)
  }
  # Checks
  stopifnot(
    "`y` must be numeric." =
      is.numeric(y),
    "`y` must have a compatible length with `x`" =
      nrow(y) == length(x)
  )

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
  if (dimless) {
    out <- drop(out)
  }
  return(out)
}

# Missingness utilities ---------------------------------------------------

interp_na <- function(x) {

  if (!anyNA(x)) return(x)

  stopifnot("Cannot interpolate NA in non-numeric vectors" = is.numeric(x))

  x[] <- approx(seq_along(x), x, seq_along(x))$y
  x
}

cases <- function (x, fun)
{
    ok <- vapply(x, fun, logical(nrow(x)))
    if (is.vector(ok)) {
        all(ok)
    }
    else {
        rowSums(as.matrix(ok)) == ncol(x)
    }
}

is_missing <- function (x)
{
  if (typeof(x) == "list") !vapply(x, is.null, logical(1)) else !is.na(x)
}

is_finite <- function (x)
{
  if (typeof(x) == "list") !vapply(x, is.null, logical(1)) else is.finite(x)
}

detect_missing <- function (df, vars, finite = FALSE)
{
    vars <- intersect(vars, names(df))
    !cases(df[, vars, drop = FALSE], if (finite)
        is_finite
    else is_missing)
}

find_missing <- function(x, layer) {

  detect_missing(x, c(layer$required_aes, layer$non_missing_aes))
}

# Label utilities ---------------------------------------------------------

match_labels <- function(x, ...) UseMethod("match_labels")

match_labels.data.frame <- function(x, labels) {

  if (length(labels) == 1) labels <- rep(labels, nrow(x))
  if (nrow(x) != length(labels))
  {
    stop("Could not match labels to object ", deparse(substitute(x)))
  }
  labels
}


match_labels.default <- function(x, labels) {

  if (length(labels) == 1) labels <- rep(labels, length(x))
  if (length(x) != length(labels))
  {
    stop("Could not match labels to object ", deparse(substitute(x)))
  }
  labels
}

modify_list <- function (old, new)
{
    for (i in names(new)) old[[i]] <- new[[i]]
    old
}

rename <- function (x, replace)
{
    current_names <- names(x)
    old_names <- names(replace)
    missing_names <- setdiff(old_names, current_names)
    if (length(missing_names) > 0) {
        replace <- replace[!old_names %in% missing_names]
        old_names <- names(replace)
    }
    names(x)[match(old_names, current_names)] <- as.vector(replace)
    x
}

# Text utilities ----------------------------------------------------------

safe_parse <- function (text)
{
  if (!is.character(text)) stop("`text` must be a character vector")

  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
      expr <- parse(text = text[[i]])
      out[[i]] <- if (length(expr) == 0)
          NA
      else expr[[1]]
  }
  out
}


is.multichar <- function(x) {

  if (is.list(x)) return(any(vapply(x, is.multichar, logical(1))))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(any(nchar(x) > 1))
  is.language(x)
}

# Based on ggplot2:::draw_axis handling of labels
make_label <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  if (any(vapply(x, is.language, logical(1)))) {
    do.call(expression, x)
  } else {
    unlist(x, FALSE, FALSE)
  }
}

# Grid utilities ----------------------------------------------------------

data_to_text_gp <- function(data) {
  gpar(
    col        = alpha(data$textcolour %||% data$colour, data$alpha),
    fontsize   = data$size * .pt,
    fontface   = data$fonface,
    fontfamily = data$family,
    lineheight = data$lineheight,
    tracking   = data$spacing
  )
}

data_to_path_gp <- function(data, lineend = "butt", linejoin = "round",
                            linemitre = 10) {
  if (all(data$linetype %in% c("0", "blank", NA))) {
    gpar(lty = 0)
  } else {
    gpar(
      col  = alpha(data$linecolour %||% data$colour, data$alpha),
      fill = alpha(data$linecolour %||% data$colour, data$alpha),
      lwd  = data$linewidth * .pt,
      lty  = data$linetype,
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre
    )
  }
}

data_to_box_gp <- function(data, lineend = "butt", linejoin = "round",
                           linemitre = 10) {
  gpar(
    col  = alpha(data$boxcolour %||% data$linecolour %||% data$colour,
                 data$alpha),
    fill = alpha(data$fill, data$alpha),
    lwd  = data$boxlinewidth %||% data$linewidth * .pt,
    lty  = data$boxlinetype  %||% data$linetype,
    lineend   = lineend,
    linejoin  = linejoin,
    linemitre = linemitre
  )
}

# Helper function to do safe(r) recycling on "gpar" class objects.
recycle_gp <- function(gp, fun, ...) {
  # Recycling rules only apply to non-unique parameters
  do_recycle <- lengths(gp) > 1
  gp[do_recycle] <- lapply(unclass(gp)[do_recycle], fun, ...)
  # Never ever have zero-length objects in the gpar
  gp[lengths(gp) == 0] <- list(NULL)
  return(gp)
}

rep_gp <- function(gp, length.out = max(lengths(gp))) {
  gp[] <- lapply(gp, rep, length.out = length.out)
  gp
}

split_gp <- function(gp, i = seq_len(max(lengths(gp)))) {
  gp <- rep_gp(gp, max(i))
  lapply(i, function(j) {
    recycle_gp(gp, `[`, j)
  })
}

# Helper function to fill in missing parameters by defaults
# Based on ggplot2:::modify_list
gp_fill_defaults <- function(gp, ..., defaults = get.gpar()) {
  extra <- list(...)
  for (i in names(extra)) defaults[[i]] <- extra[[i]]
  for (i in names(gp))    defaults[[i]] <- gp[[i]]
  defaults
}

gp_subset <- function(gp, ss) {
  subset_these <- lengths(gp) > 1
  gp[subset_these] <- lapply(unclass(gp)[subset_these], function(x) x[ss])
  gp[lengths(gp) == 0] <- list(NULL)
  return(gp)
}


as_inch <- function(value, from = "x") {
  if (is.unit(value)) {
    switch(
      from,
      x      = {axis <- "x"; type <- "location"},
      y      = {axis <- "y"; type <- "location"},
      width  = {axis <- "x"; type <- "dimension"},
      height = {axis <- "y"; type <- "dimension"}
    )
    value <- convertUnit(x = value, unitTo = "inch",
                         axisFrom = axis, typeFrom = type,
                         valueOnly = TRUE)
  }
  value
}

as_unit <- function(x, units = NULL, ...) {
  if (!is.unit(x) && !is.null(units)) {
    x <- unit(x, units, ...)
  }
  x
}

# Documentation helpers ---------------------------------------------------

as_lower <- function(x) {
  chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
}

as_upper <- function(x) {
  chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", x)
}

camelize <- function (x, first = FALSE)
{
    x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
    if (first)
    {
        x <- paste0(as_upper(substring(x, 1, 1)), substring(x, 2))
    }
    x
}

find_global <- function (name, env, mode = "any")
{
    if (exists(name, envir = env, mode = mode)) {
        return(get(name, envir = env, mode = mode))
    }
    nsenv <- asNamespace("geomtextpath")
    if (exists(name, envir = nsenv, mode = mode)) {
        return(get(name, envir = nsenv, mode = mode))
    }
    NULL
}

check_subclass <- function (x, subclass, argname = as_lower(subclass),
                            env = parent.frame())
{
    if (inherits(x, subclass)) {
        x
    }
    else if (is.character(x) && length(x) == 1) {

        name <- paste0(subclass, camelize(x, first = TRUE))
        obj <- find_global(name, env = env)
        if (is.null(obj) || !inherits(obj, subclass)) {
            abort(paste0("Can't find `", argname, "` called '", x, "'"))
        }
        else {
            obj
        }
    }
    else {
        abort(paste0("`", argname, "` must be either a string or a ",
                     subclass, " object"))
    }
}

rd_aesthetics_item <- function (x)
{
    req <- x$required_aes
    req <- sub("|", "} \\emph{or} \\code{", req,
        fixed = TRUE)
    req_aes <- unlist(strsplit(x$required_aes, "|", fixed = TRUE))
    optional_aes <- setdiff(x$aesthetics(), req_aes)
    all <- union(req, sort(optional_aes))
    ifelse(all %in% req, paste0("\\strong{\\code{", all,
        "}}"), paste0("\\code{", all, "}"))
}

# Documentation adapted from ggplot2

rd_aesthetics <- function (type, name)
{
    obj <- switch(type, geom = check_subclass(name, "Geom",
        env = globalenv()), stat = check_subclass(name, "Stat",
        env = globalenv()))
    aes <- rd_aesthetics_item(obj)
    c("@section Aesthetics:",
      paste0("\\code{", type,
        "_", name, "()} ",
        "understands the following aesthetics ",
        "(required aesthetics are in bold):"),
        "\\itemize{", paste0("  \\item ", aes), "}",
        "The `spacing` aesthetic allows fine control of spacing of text,",
        "which is called 'tracking' in typography.",
        "The default is 0 and units are measured in 1/1000 em.",
        "Numbers greater than zero increase the spacing,",
        "whereas negative numbers decrease the spacing.",
        "\n\nLearn more about setting these aesthetics ",
        "in \\code{vignette(\"ggplot2-specs\")}."

      )
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

  # Use roxygen2 to parse text params
  filepath <- paste0("https://raw.githubusercontent.com/AllanCameron/",
                     "geomtextpath/main/R/text_params.R")
  tmp <- tempfile(fileext = "R")
  download.file(filepath, tmp, quiet = TRUE)
  doc  <- roxygen2::parse_file(tmp)
  unlink(tmp)

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
  tag_names <- vapply(tags, function(x){x$val$name}, character(1))
  tag_descr <- vapply(tags, function(x){x$val$description}, character(1))

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
