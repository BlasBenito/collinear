testthat::test_that("`cor_cramer_v()` works", {

  data(vi_smol)

    v <- cor_cramer_v(
      x = vi_smol$soil_type,
      y = vi_smol$koppen_zone
      )

    testthat::expect_true(
      is.numeric(v)
    )

    testthat::expect_true(
      v >= 0 && v <= 1
    )

    # perfect one-to-one association
    v <- cor_cramer_v(
      x = c("a", "a", "b", "c"),
      y = c("a", "a", "b", "c")
    )

    testthat::expect_true(
      v == 1
    )

    # still perfect: labels differ but mapping is unique
    v <- cor_cramer_v(
      x = c("a", "a", "b", "c"),
      y = c("a", "a", "b", "d")
    )

    testthat::expect_true(
      v == 1
    )

    # high but < 1: mostly aligned, one category of y repeats
    v <- cor_cramer_v(
      x = c("a", "a", "b", "c"),
      y = c("a", "a", "b", "b")
    )

    testthat::expect_true(
      v < 1 & v > 0
    )

    # appears similar by position, but no association by distribution
    # (x = "a" mixes with y = "a" and "b")
    v <- cor_cramer_v(
      x = c("a", "a", "a", "c"),
      y = c("a", "a", "b", "b")
    )

    testthat::expect_true(
      v == 0
    )

    # numeric inputs are coerced to character internally
    v <- cor_cramer_v(
      x = c(1, 1, 2, 3),
      y = c(1, 1, 2, 2)
    )

    testthat::expect_true(
      v < 1 & v > 0
    )

    # logical inputs are also coerced to character
    v <- cor_cramer_v(
      x = c(TRUE, TRUE, FALSE, FALSE),
      y = c(TRUE, TRUE, FALSE, FALSE)
    )

    testthat::expect_true(
      v == 1
    )

})
