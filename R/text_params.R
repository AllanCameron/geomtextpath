#' Set static parameters
#'
#' This sets parameters for text on a path that aren't expected to change
#' during the construction of a grob.
#'
#' @param .type A `character(1)` with either `"text"` or `"label"`, which decides
#'   what the default is going to be for the `gap` argument. If `"text"` the
#'   default is `NA`, which means to dynamically decide. If `"label"`, the
#'   `"gap"` argument should default to `FALSE`.
#' @param text_only A `logical(1)` indicating whether the path part should be
#'   plotted along with the text (`FALSE`, the default). If `TRUE`, any
#'   parameters or aesthetics relating to the drawing of the path will be
#'   ignored.
#' @param gap A `logical(1)` which if `TRUE`, breaks the path into two sections
#'   with a gap on either side of the label. If `FALSE`, the path is plotted
#'   as a whole. Alternatively, if `NA`, the path will be broken if the string
#'   has a `vjust` between 0 and 1, and not otherwise. The default for the label
#'   variant is `FALSE` and for the text variant is `NA`.
#' @param upright A `logical(1)` which if `TRUE` (default), inverts any text
#'   where the majority of letters would upside down along the path, to improve
#'   legibility. If `FALSE`, the path decides the orientation of text.
#' @param halign A `character(1)` describing how multi-line text should be
#'   justified. Can either be `"center"` (default), `"left"` or `"right"`.
#' @param offset A [`unit`][grid::unit()] object of length 1 to determine the
#'   offset of the text from the path. If this is `NULL` (default), the `vjust`
#'   parameter decides the offset. If not `NULL`, the `offset` argument
#'   overrules the `vjust` setting.
#' @param parse A `logical(1)` which if `TRUE`, will coerce the labels into
#'   expressions, allowing for plotmath syntax to be used.
#' @param straight A `logical(1)` which if `TRUE`, keeps the letters of a label
#'   on a straight baseline and if `FALSE` (default), lets individual letters
#'   follow the curve. This might be helpful for noisy paths.
#' @param padding A [`unit`][grid::unit()] object of length 1 to determine the
#'   padding between the text and the path when the `gap` parameter trims the
#'   path.
#' @param text_smoothing a `numeric(1)` value between 0 and 100 that smooths
#'   the text without affecting the line portion of the geom. The default value
#'   of `0` means no smoothing is applied.
#' @param rich A `logical(1)` whether to interpret the text as html/markdown
#'   formatted rich text. Default: `FALSE`. See also the rich text section of
#'   the details in [`geom_textpath()`][geom_textpath].
#'
#' @return A `list` with the parameters.
#' @md
#' @keywords internal rd_dots
static_text_params <- function(
  .type          = "text",
  text_only      = FALSE,
  gap            = NULL,
  upright        = TRUE,
  halign         = "center",
  offset         = NULL,
  parse          = FALSE,
  straight       = FALSE,
  padding        = unit(0.05, "inch"),
  text_smoothing = 0,
  rich           = FALSE
) {
  if (is.null(gap)) {
    gap <- switch(.type, text = NA, FALSE)
  }
  halign <- rlang::arg_match0(halign, c("center", "left", "right"))
  if (!isFALSE(rich) && !isFALSE(parse)) {
    warn(paste0("Plotmath expressions are incompatible with rich text.\n",
                "Setting `rich = FALSE`. for now."))
    rich <- FALSE
  }

  list(
    text_only      = assert(text_only,      "logical"),
    gap            = assert(gap,            "logical", allow_NAs = TRUE),
    upright        = assert(upright,        "logical"),
    parse          = assert(parse,          "logical"),
    straight       = assert(straight,       "logical", allow_NULL = TRUE),
    padding        = assert(padding,        "unit"),
    offset         = assert(offset,         "unit", allow_NULL = TRUE),
    text_smoothing = assert(text_smoothing, "numeric"),
    rich           = assert(rich,           "logical"),
    halign         = halign
  )
}

# Automatically capture static text parameters
set_params <- function(...) {
  params <- list(...)
  text_names  <- names(formals(static_text_params))
  text_names  <- intersect(text_names, names(params))
  text_params <- do.call(static_text_params, params[text_names])
  params      <- params[setdiff(names(params), text_names)]
  params$text_params <- text_params
  params
}

update_params <- function(params, type = "text") {
  text_params <- params$text_params %||% static_text_params(.type = type)
  text_names  <- names(formals(static_text_params))
  text_names  <- intersect(text_names, names(params))
  for (i in text_names) {
    text_params[[i]] <- params[[i]]
    params[[i]] <- NULL
  }
  params$text_params <- text_params
  params
}

# This function is to check that user input is what we would expect it to be.
# It checks `value` for being of a particular class `type` and have `length`
# length. Optionally, one can allow NAs or NULLs.
assert <- function(value, type, length = 1L,
                   allow_NAs = FALSE, allow_NULL = FALSE,
                   argname = deparse(substitute(value))) {
  if (is.null(value) && allow_NULL) {
    return(NULL)
  }
  force(argname)
  message <- character()
  if (!inherits(value, type)) {
    message <- c(
      message,
      paste0("`", argname, "` must be a `", type, "` vector.")
    )
  }
  if (length(value) != length) {
    message <- c(
      message,
      paste0("`", argname, "` must be of length ", length, ".")
    )
  }
  if (isFALSE(allow_NAs) && anyNA(value)) {
    message <- c(
      message,
      paste0("`", argname, "` contains NAs whereas it cannot.")
    )
  }
  if (length(message)) {
    message <- c(
      "Unexpected input:",
      message
    )
    abort(message)
  }
  value
}
