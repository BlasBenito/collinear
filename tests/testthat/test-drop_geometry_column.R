testthat::test_that("`drop_geometry_column()` works", {
  testthat::skip_on_cran()
  data(vi_smol)

  vi_smol

  #creating fake geometry column without sf loaded
  vi_smol$geometry <- NA

  attr(
    x = vi_smol,
    which = "sf_column"
  ) <- "geometry"

  #check new attribute
  testthat::expect_true(
    attributes(vi_smol)$sf_column == "geometry"
  )

  testthat::expect_true(
    "geometry" %in% colnames(vi_smol)
  )

  #drop geometry column
  testthat::expect_message(
    vi_smol <- drop_geometry_column(
      df = vi_smol,
      quiet = FALSE
    ),
    regexp = "dropping geometry column from 'df'"
  )

  #checking that the geometry was droppped
  testthat::expect_false(
    "geometry" %in% colnames(vi_smol)
  )

  testthat::expect_true(
    is.null(attributes(vi_smol)$sf_column)
  )
})
