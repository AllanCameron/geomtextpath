#' Wrapper for text measurement
#'
#' This wrap the `systemfonts::shape_string()` function to return positions for
#' every letter.
#'
#' @param label A `character` vector of labels.
#' @param gp A `grid::gpar()` object.
#' @param ppi A `numeric(1)` for the resolution in points per inch.
#' @param vjust The justification of the text.
#'
#' @return A `list` with `data.frame`s, parallel to `label`. Every `data.frame`
#' has the columns `glyph`, `ymin`, `xmin`, `xmid`, `xmax` and `y_id`. In
#' addition, every element of the list has `offset` and `metrics` attributes.
#' @noRd
#'
#' @examples
#' measure_text("Hello there,\nGeneral Kenobi")
measure_text <- function(label, gp = gpar(), ppi = 72,
                         vjust = 0.5, hjust = 0, halign = "center") {

  halign <- match(halign, c("center", "left", "right"), nomatch = 2L)
  halign <- c("center", "left", "right")[halign]

  if ({unit_vjust <- is.unit(vjust)}) {
    offset_unit <- rep(vjust, length.out = length(label))
    vjust  <- 0
  }
  # Remedy for https://github.com/r-lib/systemfonts/issues/85
  vjust[vjust == 1] <- 1 + .Machine$double.eps

  # Multiplier for sub-pixel precision
  mult <- 100
  # We need to call shape_string twice with lots of parameters which are mostly
  # the same, so we store the parameters in a list and use do.call to avoid
  # replication in the code.
  string_args <- list(
    strings    = label,
    family     = gp$fontfamily %||% "",
    italic     = (gp$font      %||% 1) %in% c(3, 4),
    bold       = (gp$font      %||% 1) %in% c(2, 4),
    size       = gp$fontsize   %||% 12,
    lineheight = gp$lineheight %||% 1.2,
    tracking   = gp$tracking   %||% 0,
    res = ppi * mult,
    vjust = vjust,
    hjust = hjust,
    align = halign
  )

  txt <- do.call(shape_string, string_args)

  # Now we repeat the call to shape_string with a 0.5 vadjusted "x" to get the
  # y offset. This allows us to correct for the fixed vjust of 0.5 passed to
  # textGrob inside makeContent.textpath

  string_args$strings <- rep("x", length(label))
  string_args$vjust   <- 0.5
  x_adjust <- do.call(shape_string, string_args)$shape$y_offset

  # Adjust metrics
  metrics <- txt$metrics
  metrics$width  <- metrics$width  / (ppi * mult)
  metrics$height <- metrics$height / (ppi * mult)

  # Filter non-letters
  txt <- txt$shape
  txt <- txt[!(txt$glyph %in% c("\r", "\n", "\t")), , drop = FALSE]

  # Adjust shape
  txt$x_offset   <- txt$x_offset   / ppi
  txt$x_midpoint <- txt$x_midpoint / ppi
  txt$y_offset   <- (txt$y_offset - x_adjust[txt$metric_id + 1]) / ppi

  # Format shape
  ans <- data_frame(
    glyph =  txt$glyph,
    ymin  =  txt$y_offset,
    xmin  =  txt$x_offset,
    xmid  = (txt$x_offset + txt$x_midpoint),
    xmax  = (txt$x_offset + txt$x_midpoint * 2)
  )

  # Split and assign group-specific attributes
  ans <- split(ans, txt$metric_id)
  ans <- lapply(seq_along(ans), function(i) {
    df      <- ans[[i]]
    offset  <- unique(c(0, df$ymin))
    df$y_id <- match(df$ymin, offset)
    if (unit_vjust) {

      df$y_id <- df$y_id + 1
      offset  <- unit(offset, "inch")

      offset <- unit.c(unit(0, "inch"), offset + offset_unit[i])
    }
    attr(df, "metrics") <- metrics[i, , drop = FALSE]
    attr(df, "offset")  <- offset
    df
  })

  return(ans)
}

# This is a simpler version of measure_text for expressions only

measure_exp <- function(label, gp = gpar(), ppi = 72, vjust = 0.5)
{
  size <- gp$fontsize %||% 11
  stopifnot(
    "The fontsize vector in gpar does not match the number of labels." =
      length(size) == length(label) || length(size) == 1
  )
  width  <- as_inch(stringWidth(label),  "width")
  height <- as_inch(stringHeight(label), "height")
  width  <- width  * size / 11
  height <- height * size / 11
  ymin   <- -(height * (vjust - 0.5))

  Map(
    function(label, width, height) {
      ans <- data_frame(
        glyph = list(label),
        xmin  = 0,
        xmid  = width / 2,
        xmax  = width,
        y_id  = 1L
      )
      attr(ans, "offset")  <- ymin
      attr(ans, "metrics") <- data_frame(width = width, height = height)
      ans
    },
    label  = as.list(label),
    width  = width,
    height = height
  )
}
