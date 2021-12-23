test_that("We can label an sfc_LINESTRING", {

  multipolygon <- waterways$geometry[1]
  linestring <- waterways$geometry[2]

  # Ensure the geometry objects are of the correct class:
  expect_equal(class(multipolygon), c("sfc_MULTIPOLYGON", "sfc"))
  expect_equal(class(linestring), c("sfc_LINESTRING", "sfc"))

  multipolygon <- label_sf(multipolygon, "test")
  linestring <- label_sf(linestring, "test")

  # Multipolygon should be unaltered, linestring should change
  expect_equal(class(multipolygon), c("sfc_MULTIPOLYGON", "sfc"))
  expect_equal(class(linestring), c("sfc_LINESTRING_labelled", "sfc"))

  expect_equal(attr(linestring, "label"), "test")
})

test_that("sf objects are converted to correct grob types", {

  multipolygon <- waterways$geometry[1]
  linestring <- waterways$geometry[2]
  doubler <- linestring
  doubler[[2]] <- sf::st_linestring()
  nullstring <- linestring
  nullstring[[1]] <- sf::st_linestring()
  linestring_blank <- label_sf(linestring, "")
  multipolygon <- label_sf(multipolygon, "test")
  linestring <- label_sf(linestring, "test")
  linestring_null_label <- linestring
  attr(linestring_null_label, "label") <- NULL
  doubler <- label_sf(doubler, "test")
  nullstring <- label_sf(nullstring, "test")

  linegrob <- st_as_grob(linestring)
  multipolygongrob <- st_as_grob(multipolygon)
  blank_linegrob <- st_as_grob(linestring_blank)
  doublegrob <- st_as_grob(doubler)
  nullgrob <- st_as_grob(nullstring)
  null_label_grob <- st_as_grob(linestring_null_label)

  expect_equal(class(linegrob)[1], "textpath")
  expect_equal(class(doublegrob)[1], "textpath")
  expect_equal(class(nullgrob)[1], "null")
  expect_equal(class(blank_linegrob)[1], "polyline")
  expect_equal(class(null_label_grob)[1], "polyline")
  expect_equal(class(multipolygongrob)[1], "pathgrob")

})

test_that("We can make grobs from sf features", {

  p <- ggplot(waterways) + geom_textsf(label = "rivers")
  p_built <- ggplot_build(p)
  df <- p_built$data[[1]]
  df_missing <- df
  df_missing$geometry[[2]] <- sf::st_point()
  df_missing$size[2] <- NA
  expect_silent(sf_textgrob(df))

  expect_warning(sf_textgrob(df_missing, na.rm = FALSE))

})
