test_that("We can label an sfc_LINESTRING", {
  skip_if_not_installed("sf")

  multipolygon <- waterways$geometry[1]
  linestring <- waterways$geometry[2]

  # Ensure the geometry objects are of the correct class:
  expect_s3_class(multipolygon, c("sfc_MULTIPOLYGON", "sfc"))
  expect_s3_class(linestring, c("sfc_LINESTRING", "sfc"))

  multipolygon <- label_sf(multipolygon, "test")
  linestring1 <- label_sf(linestring, "test")
  linestring2 <- label_sf(linestring, "test", as_textbox = TRUE)

  # Multipolygon should be unaltered, linestring should change
  expect_s3_class(multipolygon, c("sfc_MULTIPOLYGON", "sfc"))
  expect_s3_class(linestring1, c("sfc_labelled", "sfc"))

  expect_s3_class(linestring2, c("sfc_textbox", "sfc"))
  expect_equal(attr(linestring1, "label"), "test")
})

test_that("sf objects are converted to correct grob types", {
  skip_if_not_installed("sf")

  multipolygon <- waterways$geometry[1]
  linestring <- waterways$geometry[2]
  doubler <- linestring
  doubler[[2]] <- sf::st_linestring()
  nullstring <- linestring
  nullstring[[1]] <- sf::st_linestring()
  linestring_blank <- label_sf(linestring, "")
  linestring2_blank <- label_sf(linestring, "", as_textbox = TRUE)
  multipolygon <- label_sf(multipolygon, "test")
  linestring1 <- label_sf(linestring, "test")
  linestring2 <- label_sf(linestring, "test", as_textbox = TRUE)

  linestring_null_label <- linestring1
  attr(linestring_null_label, "label") <- NULL
  doubler1 <- label_sf(doubler, "test")
  nullstring1 <- label_sf(nullstring, "test")

  linestring2_null_label <- linestring2
  attr(linestring2_null_label, "label") <- NULL
  doubler2 <- label_sf(doubler, "test", as_textbox = TRUE)
  nullstring2 <- linestring2
  nullstring2[[1]] <- sf::st_linestring()
  nullstring2 <- label_sf(nullstring2, "test", as_textbox = TRUE)

  linegrob <- sf::st_as_grob(linestring1)
  linegrob2 <- sf::st_as_grob(linestring2)
  multipolygongrob <- sf::st_as_grob(multipolygon)
  blank_linegrob <- sf::st_as_grob(linestring_blank)
  blank_linegrob2 <- sf::st_as_grob(linestring2_blank)
  doublegrob1 <- sf::st_as_grob(doubler1)
  doublegrob2 <- sf::st_as_grob(doubler2)
  nullgrob1 <- sf::st_as_grob(nullstring1)
  nullgrob2 <- sf::st_as_grob(nullstring2)
  null_label_grob <- sf::st_as_grob(linestring_null_label)
  null_label_grob2 <- sf::st_as_grob(linestring2_null_label)

  expect_s3_class(linegrob, "textpath")
  expect_s3_class(linegrob2, "labelpath")
  expect_s3_class(doublegrob1, "textpath")
  expect_s3_class(doublegrob2, "labelpath")
  expect_s3_class(nullgrob1, "null")
  expect_s3_class(nullgrob2, "null")
  expect_s3_class(blank_linegrob, "polyline")
  expect_s3_class(blank_linegrob2, "polyline")
  expect_s3_class(null_label_grob, "polyline")
  expect_s3_class(null_label_grob2, "polyline")
  expect_s3_class(multipolygongrob, "pathgrob")

})

test_that("We can make grobs from sf features", {
  skip_if_not_installed("sf")

  p <- ggplot(waterways) + geom_textsf(label = "rivers")
  p_built <- ggplot_build(p)
  df <- p_built$data[[1]]
  df_missing <- df
  df_missing$geometry[[2]] <- sf::st_point()
  df_missing$size[2] <- NA


  river <- sf::st_cast(waterways[2, ], "LINESTRING")
  river$boxcolour <- "green"
  river$alpha <- 1
  river$label <- "A"
  labelgrob <- sf_textgrob(river, as_textbox = TRUE)

  expect_equal(labelgrob$children[[1]]$textpath$gp_box$col, "green")
  expect_silent(sf_textgrob(df))
  expect_warning(sf_textgrob(df_missing, na.rm = FALSE))

  expect_silent(sf_textgrob(df, as_textbox = TRUE))
})

test_that("geom_labelsf constructor wotks", {
  skip_if_not_installed("sf")

  x <- geom_labelsf()

  expect_s3_class(x[[1]], "LayerInstance")
  expect_s3_class(x[[1]]$geom, "GeomLabelSf")
  expect_s3_class(x[[1]]$stat, "StatSf")
})
