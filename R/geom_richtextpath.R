#' Rich text along path
#'
#' Like [`geom_textpath()`][geom_textpath()], this geom draws text along a
#' curve, but the text can be formatted using a limited set of markdown or html
#' tags.
#'
#' @eval rd_dots(geom_richtextpath, exclude = "parse")
#' @inheritParams geom_textpath
#'
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#' @md
#'
#' @details The currently supported HTML tags are `<p>`, `<span>`, `<b>`,
#' `<strong>`, `<i>`, `<em>`, `<sub>`, `<sup>` and `<br>`.
#'
#' @note This function heavily relies on rich-text parsers copied from the
#'   \{\pkg{gridtext}\} package. We thank Claus O. Wilke for developing
#'   \{\pkg{gridtext}\} and allowing us to re-use his code under the MIT licence.
#'
#' @examples
#' # Label can contain a subset of HTML tags
#' label <- paste0(
#'   "Indometacin (",
#'   "C<sub>19</sub>H<sub>16</sub>",
#'   "<span style='color:limegreen'>Cl</span>",
#'   "<span style='color:blue'>N</span>",
#'   "<span style='color:red'>O</span><sub>4</sub>",
#'   ") concentration"
#' )
#'
#' ggplot(Indometh, aes(time, conc)) +
#'   geom_point() +
#'   geom_richtextpath(
#'     label = label,
#'     stat = "smooth", formula = y ~ x, method = "loess",
#'     vjust = -3, size = 8
#'   ) +
#'   scale_x_log10()
geom_richtextpath <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  ...,
  arrow    = NULL,
  na.rm    = FALSE,
  lineend  = "butt",
  linejoin = "round",
  linemitre   = 10,
  show.legend = NA,
  inherit.aes = TRUE
) {
  rlang::check_installed(c("xml2", "markdown"), "for parsing rich text.")
  layer(
    data     = data,
    mapping  = mapping,
    stat     = stat,
    geom     = GeomRichtextpath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = set_params(
      na.rm     = na.rm,
      arrow     = arrow,
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre,
      ...
    )
  )
}

#' @format NULL
#' @usage NULL
#' @export
#' @rdname GeomTextpath
GeomRichtextpath <- ggproto(
  "GeomRichtextpath", GeomTextpath,

  setup_params = function(data, params) {
    if (params$text_params$parse) {
      # Removed parse param in docs, but should warn anyway
      params$text_params$parse <- FALSE
      warn("Parsed (plotmath) expressions are incompatible with rich text.")
    }
    params
  },

  draw_panel = function(
    data, panel_params, coord,
    lineend = "butt", linejoin = "round", linemitre = 10,
    text_params = static_text_params("text"), arrow = NULL
  ) {
    copy_colour <- data$linecolour == "_copy_text_colour_"
    data$linecolour[copy_colour] <- data$colour[copy_colour]

    data$group <- discretise(data$group)

    if (!all(vapply(split(data$label, data$group),
                    function(x) all(x == x[1]), logical(1)))) {
      warn(paste("geom_richtextpath: Multiple strings found in at least",
                 "one group. Only the first will be used."))
    }

    data <- data[order(data$group), , drop = FALSE]
    data <- coord_munch(coord, data, panel_params)

    first <- run_start(data$group)

    text_gp <- gpar(
      col  = alpha(data$colour, data$alpha)[first],
      fontsize   = data$size[first] * .pt,
      fontface   = data$fontface[first],
      fontfamily = data$family[first],
      lineheight = data$lineheight[first],
      tracking   = data$spacing[first]
    )

    if (all(data$linetype %in% c("0", "blank", NA))) {
      path_gp <- gpar(lty = 0)
    } else {
      path_gp <- gpar(
        col  = alpha(data$linecolour, data$alpha)[first],
        fill = alpha(data$linecolour, data$alpha)[first],
        lwd  = data$linewidth[first] * .pt,
        lty  = data$linetype[first],
        lineend   = lineend,
        linejoin  = linejoin,
        linemitre = linemitre
      )
    }

    #---- Dispatch data to grob -----------------------------#

    richtextpathGrob(
      label = safe_labels,
      x = data$x,
      y = data$y,
      id = data$group,
      hjust  = data$hjust[first],
      vjust  = text_params$offset %||% data$vjust[first],
      halign = text_params$halign,
      gap    = text_params$gap,
      gp_text  = text_gp,
      gp_path  = path_gp,
      straight = text_params$straight,
      upright  = text_params$upright,
      default.units = "npc",
      angle = data$angle,
      polar_params = if (inherits(coord, "CoordPolar")){
        list(x = 0.5, y = 0.5, theta = coord$theta)
      } else NULL,
      padding = text_params$padding,
      arrow = arrow
    )

  }
)
