#' Wrapper for text measurement
#'
#' This wrap the `textshaping::shape_text()` function to return positions for
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
measure_text <- function(
  label,
  gp     = gpar(),
  ppi    = 72,
  vjust  = 0.5,
  hjust  = 0,
  halign = "center"
) {

  halign <- match(halign, c("center", "left", "right"), nomatch = 2L)
  halign <- c("center", "left", "right")[halign]
  nlabel <- length(label)

  if ({unit_vjust <- is.unit(vjust)}) {
    offset_unit <- rep(vjust, length.out = nlabel)
    vjust <- 0
  }
  # Remedy for https://github.com/r-lib/systemfonts/issues/85
  vjust[vjust == 1] <- 1 + .Machine$double.eps

  # Parametrise call to shape_text
  string_args <- list(
    strings    =  label,
    family     =  gp$fontfamily %||% "",
    italic     = (gp$font       %||% 1) %in% c(3, 4),
    bold       = (gp$font       %||% 1) %in% c(2, 4),
    size       =  gp$fontsize   %||% 12,
    lineheight =  gp$lineheight %||% 1.2,
    tracking   =  gp$tracking   %||% 0,
    res   = ppi,
    vjust = vjust,
    hjust = hjust,
    align = halign
  )

  txt <- do.call(shape_text, string_args)

  # Acquire x-height
  string_args$strings <- rep("x", length(label))
  string_args$vjust   <- 0.5
  x_adjust <- do.call(shape_text, string_args)$shape$y_offset

  # Extract text and metrics
  metrics <- txt$metrics
  txt     <- txt$shape

  # Translate and cluster glyphs
  txt$letter <- translate_glyph(txt$index, txt$metric_id, gp)
  clusters   <- interaction(txt$glyph, txt$metric_id, drop = TRUE)
  txt$letter <- ave(txt$letter, clusters, FUN = function(x) {
    paste0(x, collapse = "")
  })
  txt$x_midpoint <- ave(txt$x_midpoint, clusters, FUN = max)

  # Filter non-letters
  keep <- txt$letter %in% c("\r", "\n", "\t", "")
  keep <- !(keep | duplicated(txt[, c("glyph", "metric_id")]))
  txt  <- txt[keep, , drop = FALSE]
  if (length(unique(txt$metric_id)) != nlabel) {
    if (nrow(txt)) {
      warn("Not all glyphs for the labels could be retrieved.")
    } else {
      abort("No glyphs could be retrieved for these labels.")
    }
  }

  # Adjust shape for resolution
  metrics$width  <-  metrics$width  / ppi
  metrics$height <-  metrics$height / ppi
  txt$x_offset   <-  txt$x_offset   / ppi
  txt$x_midpoint <-  txt$x_midpoint / ppi
  txt$y_offset   <- (txt$y_offset - x_adjust[txt$metric_id]) / ppi

  # Format shape
  ans <- data_frame(
    glyph =  txt$letter,
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
      offset  <- unit.c(unit(0, "inch"), offset + offset_unit[i])
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

# Here a cache that stores index lookup tables of fonts
index_cache <- new.env(parent = emptyenv())

# This function retrieves index lookup tables for fonts. If the font has been
# seen before, it is retrieved from the cache. If not, it looks up the index
# table and stores it in the cache.
glyph_index <- function(family = "") {
  # Empty character is invalid name
  name <- if (family == "") "_default_" else family

  # Retrieve from cache if it exists there
  if (name %in% names(index_cache)) {
    return(index_cache[[name]])
  }

  # Get all unicode characters
  idx <- intToUtf8(1:65535, multiple = TRUE)
  idx <- systemfonts::glyph_info(idx, family = family)
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
  ints <- lapply(gp$fontfamily %||% "", glyph_index)
  split(index, id) <- Map(
    function(int, idx) int[idx + 1],
    int = ints,
    idx = split(index, id)
  )
  intToUtf8(index, multiple = TRUE)
}
