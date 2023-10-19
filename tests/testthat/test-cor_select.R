testthat::test_that("`cor_select()` works", {

  data(vi, vi_predictors)
  vi <- vi[1:1000, ]

  # Test with only numeric predictors, permissive max_cor
  selected_predictors <- cor_select(
    df = vi,
    predictors = vi_predictors,
    max_cor = 0.8
    )

  # Check that the result is a character vector
  testthat::expect_true(is.character(selected_predictors))

  # Test with only numeric predictors, restrictive max_cor
  selected_predictors <- cor_select(
    df = vi,
    predictors = vi_predictors,
    max_cor = 0.5
    )

  # Check that the result is a character vector
  testthat::expect_true(is.character(selected_predictors))

  # Test with response variable included, restrictive max_cor
  selected_predictors <- cor_select(
    df = vi,
    response = "vi_mean",
    predictors = vi_predictors,
    max_cor = 0.5
    )

  # Check that the result is a character vector
  testthat::expect_true(is.character(selected_predictors))

  # Test with response variable included, user-defined preference order, and restrictive max_cor
  user_preference_order <- c(
    "soil_type",
    "soil_temperature_mean",
    "swi_mean",
    "rainfall_mean",
    "evapotranspiration_mean"
    )

  selected_predictors <- cor_select(
    df = vi,
    response = "vi_mean",
    predictors = vi_predictors,
    preference_order = user_preference_order,
    max_cor = 0.5
    )

  # Check that the result is a character vector
  testthat::expect_true(is.character(selected_predictors))


})
