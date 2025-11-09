testthat::test_that("`summary.collinear_selection()` works", {

  library(collinear)

  data(
    vi_smol,
    vi_predictors_numeric
    )

  x <- collinear(
    df = vi_smol,
    response = "vi_numeric",
    predictors = vi_predictors_numeric,
    f = f_auto,
    quiet = TRUE
  )

  testthat::expect_output(
    y <- summary(object = x),
    regexp = "response: "
  )

  testthat::expect_true(
    names(y) == "vi_numeric"
  )

  testthat::expect_true(
    all(x$vi_numeric$selection %in% y$vi_numeric)
  )


})
