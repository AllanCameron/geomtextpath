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
#'
#' @return A `list` with the parameters.
#' @md
#' @keywords internal rd_dots
static_text_params <- function(
  .type = "text",
  text_only = FALSE,
  gap       = NULL,
  upright   = TRUE,
  halign    = "center",
  offset    = NULL,
  parse     = FALSE,
  straight  = FALSE,
  padding   = unit(0.15, "inch")
) {
  if (is.null(gap)) {
    gap <- switch(.type, text = NA, FALSE)
  }
  halign <- rlang::arg_match0(halign, c("center", "left", "right"))

  list(
    text_only = assert(text_only, "logical"),
    gap       = assert(gap,       "logical", allow_NAs = TRUE),
    upright   = assert(upright,   "logical"),
    parse     = assert(parse,     "logical"),
    straight  = assert(straight,  "logical", allow_NULL = TRUE),
    padding   = assert(padding,   "unit"),
    offset    = assert(offset,    "unit", allow_NULL = TRUE),
    halign    = halign
  )
}
