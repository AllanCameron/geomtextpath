##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textpath main                                                       ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Constructor -------------------------------------------------------------

#' @export
#' @rdname geom_textpath
#' @inheritParams textpathGrob
#' @section Aesthetics:
#' In addition to aforementioned aesthetics, \code{geom_labelpath()} also
#' understands the following aesthetics related to the textbox:
#' \itemize{
#'  \item{\code{boxcolour}}
#'  \item{\code{boxlinetype}}
#'  \item{\code{boxlinewidth}}
#' }
#' @examples
#'
#' # Rich text labels can contain a subset of HTML tags
#' label <- paste0(
#'   "Indometacin (",
#'   "C<sub>19</sub>H<sub>16</sub>",
#'   "<span style='color:limegreen'>Cl</span>",
#'   "<span style='color:blue'>N</span>",
#'   "<span style='color:red'>O</span><sub>4</sub>",
#'   ") concentration"
#' )
#'
#' # These are interpreted when `rich = TRUE`
#' ggplot(Indometh, aes(time, conc)) +
#'   geom_point() +
#'   geom_labelpath(
#'     label = label,
#'     stat = "smooth", formula = y ~ x, method = "loess",
#'     vjust = -3, size = 8, rich = TRUE
#'   ) +
#'   scale_x_log10()
geom_labelpath <- function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,
  ...,
  lineend = "butt", linejoin = "round", linemitre = 10,
  text_only = FALSE, gap = FALSE, upright = TRUE,
  halign = "center", offset = NULL, parse = FALSE,
  straight = FALSE,
  padding = unit(0.05, "inch"),
  text_smoothing = 0,
  rich = FALSE,
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  arrow = NULL
) {
  layer(
    geom        = GeomLabelpath,
    mapping     = mapping,
    data        = data,
    stat        = stat,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = set_params(
      na.rm          = na.rm,
      lineend        = lineend,
      linejoin       = linejoin,
      linemitre      = linemitre,
      text_only      = text_only,
      gap            = gap,
      upright        = upright,
      halign         = halign,
      offset         = offset,
      parse          = parse,
      straight       = straight,
      padding        = padding,
      text_smoothing = text_smoothing,
      rich           = rich,
      label.padding  = label.padding,
      label.r        = label.r,
      arrow          = arrow,
      ...
    )
  )
}

# ggproto class -----------------------------------------------------------

#' @export
#' @rdname GeomTextpath
#' @format NULL
#' @usage NULL
#' @include geom_textpath.R
GeomLabelpath <- ggproto(
  "GeomLabelpath", GeomTextpath,

  default_aes = aes(
    colour       = "black",
    alpha        = 1,
    size         = 3.88,
    hjust        = 0.5,
    vjust        = 0.5,
    family       = "",
    fontface     = 1,
    lineheight   = 1.2,
    linewidth    = 0.5,
    linetype     = 1,
    spacing      = 0,
    angle        = 0,
    fill         = "white",
    linecolour   = NULL,
    textcolour   = NULL,
    boxcolour    = NULL,
    boxlinetype  = 1,
    boxlinewidth = NULL
  ),

  extra_params = c("na.rm", names(formals(static_text_params))[-1]),

  setup_params = function(data, params) {
    update_params(params, type = "label")
  },

  draw_panel = function(
    data, panel_params, coord,
    lineend = "butt", linejoin = "round", linemitre = 10,
    label.padding = unit(0.25, "lines"),
    label.r = unit(0.15, "lines"), arrow = NULL,
    text_params = static_text_params("label")
  ) {

    #---- type conversion, checks & warnings ---------------------------#

    # We need to change groups to numeric to order them appropriately
    data$group <- discretise(data$group)

    # If there is more than one text string associated with any of the groups,
    # we warn that only the first is used
    if (!all(gapply(data$label, data$group,
                   function(x) all(x == x[1]), logical(1))))
    {
      warn(paste("geom_labelpath: Multiple strings found in at",
                 "least one group. Only the first will be used."))
    }

    #---- Data manipulation ---------------------------------#

    # Now we can sort the data by group
    data <- data[order(data$group), , drop = FALSE]

    # All our transformations occur after the coord transform:
    data <- coord_munch(coord, data, panel_params)

    #---- Set graphical parameters --------------------------#

    # Get first observation of each group
    first <- run_start(data$group)

    text_gp <- data_to_text_gp(data[first, , drop = FALSE])
    path_gp <- data_to_path_gp(
      data[first, , drop = FALSE],
      lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
    box_gp <- data_to_box_gp(
      data[first, , drop = FALSE],
      lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )

    safe_labels <- if (text_params$parse) {
      safe_parse(as.character(data$label[first]))
    } else {
      data$label[first]
    }

    #---- Dispatch data to grob -----------------------------#

    textpathGrob(
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
      gp_box   = box_gp,
      straight = text_params$straight,
      upright  = text_params$upright,
      default.units = "npc",
      angle = data$angle,
      polar_params = if (inherits(coord, "CoordPolar")){
        list(x = 0.5, y = 0.5, theta = coord$theta)
      } else NULL,
      padding = text_params$padding,
      text_smoothing = text_params$text_smoothing,
      rich    = text_params$rich,
      label.padding = label.padding,
      label.r = label.r,
      arrow = arrow,
      as_label = TRUE
    )
  }
)

#' @rdname geom_textpath
#' @inheritParams ggplot2::geom_line
#' @inheritParams textpathGrob
#' @export
geom_labelline <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...,
  lineend = "butt", linejoin = "round", linemitre = 10,
  text_only = FALSE, gap = FALSE, upright = TRUE,
  halign = "center", offset = NULL, parse = FALSE,
  straight = FALSE,
  padding = unit(0.05, "inch"),
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  arrow = NULL
) {
  layer(
    geom        = GeomLabelpath,
    mapping     = mapping,
    data        = data,
    stat        = stat,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = set_params(
      na.rm         = na.rm,
      lineend       = lineend,
      linejoin      = linejoin,
      linemitre     = linemitre,
      text_only     = text_only,
      gap           = gap,
      upright       = upright,
      halign        = halign,
      offset        = offset,
      parse         = parse,
      straight      = straight,
      padding       = padding,
      label.padding = label.padding,
      label.r       = label.r,
      arrow         = arrow,
      ...
    )
  )
}

#' @rdname geom_textpath
#' @format NULL
#' @usage NULL
#' @export
GeomLabelLine <- ggproto("GeomLabelLine", GeomLabelpath,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    update_params(params, type = "label")
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    flip_data(data, params$flipped_aes)
  }
)
