##---------------------------------------------------------------------------##
##                                                                           ##
##  Functions borrowed from the {gridtext} package                           ##
##                                                                           ##
##  Many of these functions are copied verbatim or with modifications        ##
##  from the gridtext package. The copyright to the code this file           ##
##  therefore belongs to Claus O. Wilke (2019), and was distributed          ##
##  under the MIT licence.                                                   ##
##                                                                           ##
##  see https://mit-license.org                                              ##
##  or the LICENSE file on                                                   ##
##  https://github.com/wilkelab/gridtext/blob/master/LICENSE.md              ##
##                                                                           ##
##---------------------------------------------------------------------------##

# The most notable change relative to {gridtext}'s code is that we've omitted
# dealing with textboxes, which simplifies a few things.

# Tag processing ----------------------------------------------------------

process_tags <- function(node, drawing_context) {
  new  <- lapply(seq_along(node), function(i) {
    dispatch_tag(node[[i]], names(node)[i], drawing_context)
  })
  rbind_dfs(new)
}


# Will process any tag as long as it has a function to handle it with the
# prefix 'process_tag_' . This allows for easier handling of additional tags
# simply by writing an appropriately named method

dispatch_tag <- function(node, tag, drawing_context) {
  if (is.null(tag) || tag == "") return(process_text(node, drawing_context))

  call_name <- paste("process_tag", tag, sep = "_")
  if(!exists(call_name)) {
      abort(paste0(
      "The rich-text has a tag that isn't supported (yet): <", tag, ">\n",
      "Only a very limited number of tags are currently supported."
    ))
  }
  dc <- set_style(drawing_context, attr(node, "style"))
  call(call_name, node = node, drawing_context = dc)
}


# Departs from gridtext:::process_text in that it returns a list-matrix

process_text <- function(node, drawing_context) {
  cbind(list(unlist(node)),
        list(drawing_context$gp),
        list(drawing_context$yoff))
}


process_tag_p <- process_tag_span <-  function(node, drawing_context) {
  process_tags(node, drawing_context)
}


process_tag_b <- process_tag_strong <- function(node, drawing_context) {
  drawing_context <- set_context_font(drawing_context, 2)
  process_tags(node, drawing_context)
}


process_tag_i <- process_tag_em <- function(node, drawing_context) {
  drawing_context <- set_context_font(drawing_context, 3)
  process_tags(node, drawing_context)
}


# TODO: Are we handling nested <sub>/<sup> correctly in terms of y-offset?
# Note that we departed here from {gridtext} in terms of how the y-offset is
# handled

process_tag_sub <- function(node, drawing_context) {
  drawing_context <- set_context_size(drawing_context, 0.8)
  drawing_context <- set_context_offset(drawing_context, -0.5)
  process_tags(node, drawing_context)
}


process_tag_sup <- function(node, drawing_context) {
  drawing_context <- set_context_size(drawing_context, 0.8)
  drawing_context <- set_context_offset(drawing_context,  0.5)
  process_tags(node, drawing_context)
}


# Context -----------------------------------------------------------------

setup_context <- function(fontsize = 12, fontfamily = "", fontface = "plain",
                          colour = "black", lineheight = 1.2, gp = NULL) {
  if (is.null(gp)) {
    gp <- gpar(
      fontsize   = fontsize,
      fontfamily = fontfamily,
      fontface   = fontface,
      col        = colour,
      cex        = 1,
      lineheight = lineheight
    )
  }
  gp <- update_gpar(get.gpar(), gp)
  set_context_gp(list(yoff = 0), gp)
}


update_gpar <- function(gp, gp_new) {
  names_new <- names(gp_new)
  names_old <- names(gp)
  gp[c(intersect(names_old, names_new), "fontface")] <- NULL
  gp_new["fontface"] <- NULL
  do.call(gpar, c(gp, gp_new))
}


set_context_gp <- function(drawing_context, gp = NULL) {
  gp <- update_gpar(drawing_context$gp, gp)
  update_context(drawing_context, ascent = x_height(gp), gp = gp)
}


set_context_font <- function(drawing_context, font = 1, overwrite = FALSE) {
  font_old <- drawing_context$gp$font
  old_bold <- font_old %in% c(2, 4)
  new_bold <- font     %in% c(2, 4)
  old_ital <- font_old %in% c(3, 4)
  new_ital <- font     %in% c(3, 4)

  if (!isTRUE(overwrite)) {
    if (isTRUE(new_ital) && isTRUE(old_bold)) {
      font <- 4
    } else if (isTRUE(new_bold) && isTRUE(old_ital)) {
      font <- 4
    }
  }

  set_context_gp(drawing_context, gpar(font = font))
}


set_context_size <- function(drawing_context, size = 1) {
  fontsize        <- size * drawing_context$gp$fontsize
  set_context_gp(drawing_context, gpar(fontsize = fontsize))
}


set_context_offset <- function(drawing_context, offset = 0) {
  drawing_context$yoff <- drawing_context$yoff + drawing_context$ascent * offset
  drawing_context
}


update_context <- function(drawing_context, ...) {
  dc_new    <- list(...)
  names_new <- names(dc_new)
  names_old <- names(drawing_context)
  drawing_context[intersect(names_old, names_new)] <- NULL
  c(drawing_context, dc_new)
}


set_style <- function(drawing_context, style = NULL) {

  if (is.null(style)) return(drawing_context)

  css <- parse_css(style)

  if (!is.null(css$`font-size`)) {
    font_size <- convert_css_unit_pt(css$`font-size`)
  } else {
    font_size <- NULL
  }

  drawing_context <- set_context_gp(
    drawing_context,
    gpar(col = css$color, fontfamily = css$`font-family`, fontsize = font_size)
  )
}


# CSS parsing -------------------------------------------------------------

parse_css <- function(text) {
  lines <- strsplit(text, ";", fixed = TRUE)[[1]]
  unlist(lapply(lines, parse_css_line), FALSE)
}


parse_css_line <- function(line) {
  pattern <- "\\s*(\\S+)\\s*:\\s*(\"(.*)\"|'(.*)'|(\\S*))\\s*"
  m       <- attributes(regexpr(pattern, line, perl = TRUE))
  start   <- m$capture.start
  end     <- start + m$capture.length - 1
  if (start[1] > 0) {
    key <- substr(line, start[1], end[1])
    # Ensure capitalized tags are handled
    key <- as_lower(key)
  } else {
    key <- NULL
  }

  if (start[3] > 0) {
    value <- substr(line, start[3], end[3])
  } else if (start[4] > 0) {
    value <- substr(line, start[4], end[4])
  } else if (start[5] > 0) {
    value <- substr(line, start[5], end[5])
  } else value <- NULL

  if (is.null(key)) list()
  else rlang::list2(!!key := value)
}


parse_css_unit <- function(x) {
  pattern <- "^((-?\\d+\\.?\\d*)(%|[a-zA-Z]+)|(0))$"
  m       <- attributes(regexpr(pattern, x, perl = TRUE))
  start   <- m$capture.start
  end     <- start + m$capture.length - 1
  if (start[4] > 0) {
    # matched null value
    return(list(value = 0, unit = "pt"))
  } else {
    if (start[2] > 0) {
      value <- as.numeric(substr(x, start[2], end[2]))
      if (start[3] > 0) {
        unit <- substr(x, start[3], end[3])
        return(list(value = value, unit = unit))
      }
    }
  }
  abort(paste0("The string '", x, "' does not represent a valid CSS unit."))
}


convert_css_unit_pt <- function(x) {
  u <- parse_css_unit(x)
  switch(
    u$unit,
    pt   = u$value,
    px   = (72 / 96)   * u$value,
    `in` =  72         * u$value,
    cm   = (72 / 2.54) * u$value,
    mm   = (72 / 25.4) * u$value,
    abort(paste0("Cannot convert ", u$value, u$unit, " to pt."))
  )
}
