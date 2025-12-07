testthat::test_that("`cor_cramer()` works", {
  testthat::skip_on_cran()
  data(vi_smol)

  testthat::expect_error(
    x <- cor_cramer(
      x = NULL,
      y = vi_smol$koppen_zone
    ),
    regexp = "argument 'x' cannot be NULL"
  )

  testthat::expect_error(
    x <- cor_cramer(
      x = vi_smol$koppen_zone,
      y = NULL,
    ),
    regexp = "argument 'y' cannot be NULL"
  )

  testthat::expect_error(
    x <- cor_cramer(
      x = vi_smol$koppen_zone,
      y = vi_smol$soil_type[1:10],
    ),
    regexp = "arguments 'x' and 'y' must be of the same length"
  )

  x <- cor_cramer(
    x = vi_smol$soil_type,
    y = vi_smol$koppen_zone
  )

  testthat::expect_true(
    is.numeric(x)
  )

  testthat::expect_true(
    x >= 0 && x <= 1
  )

  # perfect one-to-one association
  x <- cor_cramer(
    x = c("a", "a", "b", "c"),
    y = c("a", "a", "b", "c")
  )

  testthat::expect_true(
    x == 1
  )

  # still perfect: labels differ but mapping is unique
  x <- cor_cramer(
    x = c("a", "a", "b", "c"),
    y = c("a", "a", "b", "d")
  )

  testthat::expect_true(
    x == 1
  )

  # high but < 1: mostly aligned, one category of y repeats
  x <- cor_cramer(
    x = c("a", "a", "b", "c"),
    y = c("a", "a", "b", "b")
  )

  testthat::expect_true(
    x < 1 & x > 0
  )

  # appears similar by position, but no association by distribution
  # (x = "a" mixes with y = "a" and "b")
  x <- cor_cramer(
    x = c("a", "a", "a", "c"),
    y = c("a", "a", "b", "b")
  )

  testthat::expect_equal(x, 0, tolerance = 1e-10)

  # numeric inputs are coerced to character internally
  x <- cor_cramer(
    x = c(1, 1, 2, 3),
    y = c(1, 1, 2, 2)
  )

  testthat::expect_true(
    x < 1 & x > 0
  )

  # logical inputs are also coerced to character
  x <- cor_cramer(
    x = c(TRUE, TRUE, FALSE, FALSE),
    y = c(TRUE, TRUE, FALSE, FALSE)
  )

  testthat::expect_true(
    x == 1
  )
})
