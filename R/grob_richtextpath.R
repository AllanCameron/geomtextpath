# Grob construction -------------------------------------------------------

#' Draw rich-text on a path
#'
#' This function creates (curved) rich text on a path.
#'
#' @inheritParams textpathGrob
#'
#' @return An object of class `gTree`, containing grobs.
#' @export
#' @md
#'
#' @examples
#' require(grid)
#'
#' t <- seq(0, 2 * pi, length.out = 100)
#' grob <- richtextpathGrob(
#'   label = c(
#'     "Why I am making <span style='color:blue'>trigonometry jokes</span>? Cos I can!",
#'     "I was never any good at <span style='color:red'>sine language</span>."
#'   ),
#'   x = c(t, t) / (2 * pi),
#'   y = c(cos(t), sin(t)) * 0.25 + 0.5,
#'   id = rep(1:2, each = length(t)),
#'   vjust = rep(0.5, 2 * length(t)),
#'   gp_text = gpar(lineheight = c(1.2, 1.2), fontsize = c(10, 10)),
#'   gp_path = gpar(lty = c(1, 2))
#' )
#'
#' grid.newpage(); grid.draw(grob)
richtextpathGrob <- function(
  label,
  x  = 0.5,
  y  = 0.5,
  id = 1L,
  just  = "centre",
  hjust = NULL,
  vjust = NULL,
  halign = "left",
  angle = 0,
  straight = FALSE,
  gp_text = gpar(),
  gp_path = gpar(),
  gap = NA,
  upright = TRUE,
  polar_params  = NULL,
  padding = unit(0.15, "inch"),
  arrow = NULL,
  default.units = "npc",
  name = NULL,
  vp   = NULL
) {
  rlang::check_installed(c("xml2", "markdown"), "for parsing rich text.")
  if (missing(label)) {
    return(gTree(name = name, vp = vp, cl = "textpath"))
  }

  n_label <- length(label)
  id_lens <- run_len(id)

  stopifnot(
    "`x` is not of the same length as `id`" =
      length(x) == length(id),
    "`y` is not the same length as `x`" =
      length(x) == length(y),
    "Cannot match labels to paths" =
      n_label   == length(id_lens),
    "`angle` must be length 1 or the same length as `x`" =
      (lengths(angle) %in% c(1, length(x)))
  )

  hjust  <- rep_len(resolveHJust(just, hjust), n_label)
  vjust  <- rep_len(resolveVJust(just, vjust), n_label)
  halign <- rep_len(halign, n_label)

  gp_old <- gp_fill_defaults(gp_text, fontface = "plain")
  parsed <- parse_richtext(label, gp_old)
  gp_new <- attr(parsed, "gp")

  parsed <- measure_richtext(parsed, gp = gp_new, vjust = vjust,
                             halign = halign, straight = straight,
                             old_gp = gp_old)

  x <- as_unit(x, default.units)
  y <- as_unit(y, default.units)

  if (!is.null(polar_params)) {
    polar_params$x <- unit(polar_params$x, default.units)
    polar_params$y <- unit(polar_params$y, default.units)
  }

  path <- data_frame(x = x, y = y, id = rep(seq_along(id_lens), id_lens))

  gTree(
    textpath = list(
      data    = path,
      label   = parsed,
      gp_text = gp_new,
      gp_path = gp_path,
      params  = list(
        upright      = upright,
        polar_params = polar_params,
        angle        = angle,
        padding      = padding,
        hjust        = hjust,
        vjust        = vjust,
        halign       = halign,
        gap          = gap
      ),
      arrow = arrow
    ),
    name = name,
    vp   = vp,
    cl   = "textpath"
  )
}

# Helpers -----------------------------------------------------------------

# TODO: Add measurement for straight labels
measure_richtext <- function(
  label,
  gp = gpar(),
  ppi = 72,
  vjust = 0.5,
  hjust = 0,
  halign = "center",
  straight = FALSE,
  old_gp = gpar()
) {
  if (isTRUE(straight)) {
    abort("Straight richtext doesn't work yet")
  }

  halign <- match(halign, c("center", "left", "right"), nomatch = 2L)
  halign <- c("center", "left", "right")[halign]
  nlabel <- length(unique(label$id))

  if ({unit_vjust <- is.unit(vjust)}) {
    offset_unit <- rep(vjust, length.out = nlabel)
    vjust <- 0
  }

  txt <- shape_text(
    strings    =  label$text,
    family     =  gp$fontfamily %||% "",
    size       =  gp$fontsize   %||% 12,
    italic     = (gp$font       %||% 1) %in% c(3, 4),
    bold       = (gp$font       %||% 1) %in% c(2, 4),
    lineheight =  gp$lineheight %||% 1.2,
    tracking   =  gp$tracking   %||% 0,
    id         =  label$id,
    res = ppi, vjust = vjust, hjust = hjust, align = halign
  )

  # We use the original gp here because the new gp may have altered font size
  # due to super/subscripts etc.
  info <- systemfonts::font_info(
    family =  old_gp$fontfamily %||% "",
    italic = (old_gp$font       %||% 1) %in% c(3, 4),
    bold   = (old_gp$font       %||% 1) %in% c(2, 4),
    size   =  old_gp$fontsize   %||% 12,
    res = ppi
  )

  metrics <- txt$metrics
  txt     <- txt$shape

  txt <- txt[order(txt$metric_id, txt$string_id), , drop = FALSE]
  txt$letter <- translate_glyph(txt$index, group_id(txt, c("metric_id", "string_id")), gp)
  txt        <- cluster_glyphs(txt)
  txt        <- filter_glyphs(txt, nlabel)

  metrics$width  <-  metrics$width  / ppi
  metrics$height <-  metrics$height / ppi
  metrics$x_adj  <-  - 0.5 * info$max_ascend / ppi
  txt$x_offset   <-  txt$x_offset   / ppi
  txt$x_midpoint <-  txt$x_midpoint / ppi
  substring <- group_id(txt, c("metric_id", "string_id"))
  txt$y_offset   <- (txt$y_offset - (-0.5 - label$yoff[substring]) *
    info$max_ascend[txt$metric_id]) / ppi

  ans <- data_frame(
    glyph =  txt$letter,
    ymin  =  txt$y_offset,
    xmin  =  txt$x_offset,
    xmid  = (txt$x_offset + txt$x_midpoint),
    xmax  = (txt$x_offset + txt$x_midpoint * 2),
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
    attr(df, "metrics") <- metrics[i, , drop = FALSE]
    attr(df, "offset")  <- offset
    df
  })

  ans
}

parse_richtext <- function(text, gp, md = TRUE, id = seq_along(text),
                           inner = FALSE) {
  text <- as.character(text)
  if (!inner) {
    # If text is multiple labels, loop myself
    gps <- lapply(seq_along(text), function(i) {
      recycle_gp(gp, function(x){x[pmin(length(x), i)]})
    })
    ans  <- Map(parse_richtext, text, gp = gps, md = md, id = id, inner = TRUE)
    ans  <- rbind_dfs(ans)

    ans_gp <- setdiff(names(ans), c("text", "id", "yoff"))
    ans_gp <- do.call(gpar, ans[ans_gp])
    attr(ans, "gp") <- ans_gp

    return(ans)
  }

  old_gp <- gp
  if (md) {
    text <- markdown::markdownToHTML(text = text,
                                     options = c("use_xhtml", "fragment_only"))
  }
  # Deal with <br> now, they are a pain to handle after parsing
  text <- gsub("<br>", "\n", text, fixed = TRUE)

  doc <- xml2::read_html(paste0("<!DOCTYPE html>", text))
  doc <- xml2::as_list(doc)$html$body
  drawing_context <- setup_context(gp = gp)

  processed <- process_tags(doc, drawing_context)
  strings   <- unlist(processed[, 1])
  gp        <- processed[, 2]
  yoff      <- unlist(processed[, 3])

  gp <- lapply(gp, function(x) do.call(data_frame, unclass(x)))
  gp <- do.call(rbind, gp)

  data_frame(
    text       = strings,
    id         = id,
    fontfamily = gp$fontfamily,
    fontsize   = gp$fontsize,
    font       = gp$font,
    lineheight = gp$lineheight,
    tracking   = old_gp$tracking[id] %||% 0,
    col        = gp$col,
    yoff       = yoff
  )
}



