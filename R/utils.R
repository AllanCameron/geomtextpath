# Similar to rlang::`%||%` or utils:::`%||%`
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

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

discretise <- function(x) {
  match(x, unique(x))
}

# Function for `approx()`-ing a number of `y` variables. More efficient
# than looping `approx()` due to not having to recalculate indices every
# iteration. For non-numeric values, repeats first entry to match length.
# Note: This also extrapolates based on the four extreme points.
approx_multiple <- function(x, xout, y = matrix()) {
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
