test_that("creates LINESTRING", {
  result <- make_line(-75.1, 35, -75.5, 36.2) |>
    expect_s3_class(c("XY", "LINESTRING", "sfg"))

  expect_equal(
    sf::st_coordinates(result),
    matrix(
      c(-75.1, 35, 1, -75.5, 36.2, 1),
      c(2, 3),
      byrow = T,
      dimnames = list(c(NULL), c("X", "Y", "L1"))
    )
  )
})
