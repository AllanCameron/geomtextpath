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
