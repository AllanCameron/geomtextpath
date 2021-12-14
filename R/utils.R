# ------------------------------------------------------------------------------
# Row-bind a list of data.frames
# `df_list` is a list of data.frames
# `idcol` is a name for an id column, or NULL if it is to be omitted
rbind_dfs <- function(df_list, idcol = NULL) {
  # Ideally, we'd use vctrs::vec_c(!!!df_list) which is 10x faster
  ans <- do.call(rbind.data.frame, c(df_list, make.row.names = FALSE))
  if (!is.null(idcol)) {
    n <- vapply(df_list, nrow, integer(1), USE.NAMES = FALSE)
    ans[[idcol]] <- rep(names(df_list) %||% seq_along(n), times = n)
  }
  ans
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

# ------------------------------------------------------------------------------
# Cheaper data.frame constructor for internal use.
# Only use when `...` has valid names and content are valid data.frame columns
data_frame <- function(...) {
  list_to_df(x = list(...))
}

# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------

discretise <- function(x) {
  match(x, unique(x))
}

# ------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------
# In general, missing (NA) points on a path can be interpolated between the
# existing points without affecting the shape of the path. To keep all vectors
# the same length, we therefore need a function to linearly interpolate at NAs

.interp_na <- function(x) {

  if(!anyNA(x)) return(x)

  stopifnot("Cannot interpolate NA in non-numeric vectors" = is.numeric(x),
            "Cannot interpolate NA if no non-NA values" = !all(is.na(x)))

  approx(seq_along(x), x, seq_along(x))$y
}



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

  if(is.list(x)) return(any(vapply(x, is.multichar, logical(1))))
  if(is.factor(x)) x <- as.character(x)
  if(is.character(x)) return(any(nchar(x) > 1))
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


# Documentation functions modified from ggplot2

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
