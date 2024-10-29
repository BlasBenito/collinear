testthat::test_that("`cor_cramer_v()` works", {

  data(vi)

    v <- cor_cramer_v(
      x = vi$soil_type,
      y = vi$koppen_zone
      )

    testthat::expect_true(
      is.numeric(v),
      info = "Result should be a numeric value."
    )

    testthat::expect_true(
      v >= 0 && v <= 1,
      info = "Cramer's V should be within the range [0, 1]."
    )

    testthat::expect_error(
      v <- cor_cramer_v(
        x = vi$soil_type,
        y = vi$vi_mean
      )
    )

})
