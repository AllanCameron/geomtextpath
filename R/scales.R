#' justification scales
#'
#' Sometimes text labels on adjacent lines can clash if the lines are not well
#' separated vertically. One option for controlling this is to use an hjust or
#' vjust scale that will place each label on a different position on each path,
#' either vertically (vjust) or horizontally (hjust).
#'
#' The simplest way to separate labels is by  adding `scale_hjust_discrete()`
#' or `scale_vjust_discrete()` to your plot, but you can get more control with
#' `scale_hjust_manual` and `scale_vjust_manual`.
#'
#'
#' @param ... Other arguments passed on to [continuous_scale()], [binned_scale],
#'   or [discrete_scale()] as appropriate, to control name, limits,
#'   breaks, labels and so forth.
#' @param range Output range of hjust and vjust. Must lie between 0 and 1 for
#'   hjust.
#' @param guide A function used to create a guide or its name. See
#'   [guides()] for more information.
#' @param na.value Missing values will be replaced with this value.
#' @param values a set of aesthetic values to map data values to. The values
#'   will be matched in order (usually alphabetical) with the limits of the
#'   scale, or with breaks if provided. If this is a named vector, then the
#'   values will be matched based on the names instead. Data values that
#'   don't match will be given na.value.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     [transformation object][scales::trans_new()]
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks
#'     as output (e.g., a function returned by [scales::extended_breaks()]).
#'     Also accepts rlang [lambda][rlang::as_function()] function notation.
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Length, color = Species)) +
#'   geom_textdensity(aes(label = Species, hjust = Species), size = 6) +
#'   scale_hjust_discrete()

scale_hjust_discrete <- function(..., range = c(0, 1), guide = "none") {

  discrete_scale(
    "hjust",
    "hjust_d",
    function(n) seq(range[1], range[2], length.out = n),
    guide = guide,
    ...
  )
}

#' @rdname scale_hjust_discrete
#' @export
#'
scale_hjust_manual <- function (..., values, breaks = waiver(),
                                guide = "none", na.value = NA)
{
    manual_scale("hjust",
                 values,
                 breaks,
                 guide = guide,
                 ...,
                 na.value = na.value)
}

#' @rdname scale_hjust_discrete
#' @export

scale_hjust_identity <- function (..., guide = "none")
{
    continuous_scale("hjust",
                     "identity",
                     identity_pal(),
                     ...,
                     guide = guide,
                     super = ScaleContinuousIdentity)
}

#' @rdname scale_hjust_discrete
#' @export

scale_vjust_discrete <- function(..., guide = "none", range = c(-0.5, 1.5)) {
  discrete_scale(
    "vjust",
    "vjust_d",
    function(n) seq(range[1], range[2], length.out = n),
    guide = guide,
    ...
  )
}

#' @rdname scale_hjust_discrete
#' @export
#'
scale_vjust_manual <- function (..., values, breaks = waiver(),
                                guide = "none", na.value = NA)
{
    manual_scale("vjust",
                 values,
                 breaks,
                 guide = guide,
                 ...,
                 na.value = na.value)
}


#' @rdname scale_hjust_discrete
#' @export

scale_vjust_identity <- function (..., guide = "none")
{
    continuous_scale("vjust",
                     "identity",
                     identity_pal(),
                     ...,
                     guide = guide,
                     super = ScaleContinuousIdentity)
}


# Non-exported ggplot2 function

manual_scale <- function (aesthetic, values = NULL,
                          breaks = waiver(), ..., limits = NULL)
{
    force(values)

    if (is.null(limits)) {
        limits <- names(values)
    }
    if (is.vector(values) && is.null(names(values)) &&
        !inherits(breaks, "waiver") &&
        !is.null(breaks) && !is.function(breaks)) {
        if (length(breaks) <= length(values)) {
            names(values) <- breaks
        }
        else {
            names(values) <- breaks[1:length(values)]
        }
    }
    pal <- function(n) {
        if (n > length(values)) {
            rlang::abort(
              paste0("Insufficient values in manual scale. ", n,
                     " needed but only ", length(values), " provided."))
        }
        values
    }
    discrete_scale(aesthetic, "manual", pal, breaks = breaks,
        limits = limits, ...)
}

identity_pal <- function () function(x) x


