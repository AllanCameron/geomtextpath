##---------------------------------------------------------------------------##
##                                                                           ##
##  geomtextpath utilities                                                   ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron                                      ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

#------------------------------------------------------------------------------
#' darken
#'
#' @param color a character vector of valid R colors as names or hex
#' @param factor the factor by which the color should be darkened
#'
#' @return a vector of hex strings representing darkened colors
#' @export
#'
#' @examples
#' darken(c("white", "#565656"), 1.4)
#'
darken <- function(color, factor = 1.4)
{
  rgb(t(col2rgb(color)/factor), maxColorValue = 255)
}
