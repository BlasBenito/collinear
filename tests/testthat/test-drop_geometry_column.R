testthat::test_that("`case_weights()` works", {

  data(vi_smol)

  df <- vi_smol

  #creating fake geometry column without sf loaded
  df$geometry <- NA

  attr(
    x = df,
    which = "sf_column"
    ) <- "geometry"

  #check new attribute
  testthat::expect_true(
    attributes(df)$sf_column == "geometry"
  )

  testthat::expect_true(
    "geometry" %in% colnames(df)
  )


  #drop geometry column
  df <- drop_geometry_column(
    df = df,
    quiet = TRUE
    )

  #checking that the geometry was droppped
  testthat::expect_false(
    "geometry" %in% colnames(df)
  )

  testthat::expect_true(
    is.null(attributes(df)$sf_column)
  )



})
