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
#' Because textpaths are constructed from several rows of input data, the
#' continuous variants `scale_hjust_continuous` and `scale_hjust_identity` are
#' less useful, but are included for completeness; they may offer more control
#' if the data is prepared beforehand with these uses in mind.
#'
#' @param ... Other arguments passed on to [continuous_scale()], [binned_scale],
#'   or [discrete_scale()] as appropriate, to control name, limits,
#'   breaks, labels and so forth.
#' @param range Output range of hjust values. Must lie between 0 and 1.
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Length, color = Species)) +
#'   geom_textdensity(aes(label = Species, hjust = Species), size = 6) +
#'   scale_hjust_discrete()

scale_hjust_discrete <- function(..., range = c(0, 1)) {
  discrete_scale(
    "hjust",
    "hjust_d",
    function(n) seq(range[1], range[2], length.out = n),
    ...
  )
}

#' @rdname scale_hjust_discrete
#' @export
#'
scale_hjust_manual <- function (..., values, breaks = waiver(), na.value = NA)
{
    manual_scale("hjust", values, breaks, ..., na.value = na.value)
}


#' @rdname scale_hjust_discrete
#' @export

scale_hjust_continuous <- function (..., range = c(0, 1))
{
    continuous_scale("hjust", "hjust_c", rescale_pal(range),
        ...)
}

#' @rdname scale_hjust_discrete
#' @export

scale_hjust_identity <- function (..., guide = "none")
{
    sc <- continuous_scale("hjust", "identity", identity_pal(),
        ..., guide = guide, super = ScaleContinuousIdentity)
    sc
}

#' @rdname scale_hjust_discrete
#' @export

scale_vjust_discrete <- function(..., range = c(-0.5, 1.5)) {
  discrete_scale(
    "vjust",
    "vjust_d",
    function(n) seq(range[1], range[2], length.out = n),
    ...
  )
}

#' @rdname scale_hjust_discrete
#' @export
#'
scale_vjust_manual <- function (..., values, breaks = waiver(), na.value = NA)
{
    manual_scale("vjust", values, breaks, ..., na.value = na.value)
}


#' @rdname scale_hjust_discrete
#' @export

scale_vjust_continuous <- function (..., range = c(0, 1))
{
    continuous_scale("hjust", "hjust_c", rescale_pal(range),
        ...)
}

#' @rdname scale_hjust_discrete
#' @export

scale_vjust_identity <- function (..., guide = "none")
{
    sc <- continuous_scale("vjust", "identity", identity_pal(),
        ..., guide = guide, super = ScaleContinuousIdentity)
    sc
}


# Non-exported ggplot2 function

manual_scale <- function (aesthetic, values = NULL,
                          breaks = waiver(), ..., limits = NULL)
{
    if (rlang::is_missing(values)) {
        values <- NULL
    }
    else {
        force(values)
    }
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
