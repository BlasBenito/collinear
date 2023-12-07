testthat::test_that("`vif_select()` works", {
  data(vi, vi_predictors)
  vi <- vi[1:1000, ]

  #create a few perfect correlations in vi
  #reduce correlation in predictors with cor_select()
  vi_predictors <- cor_select(
    df = vi,
    response = "vi_mean",
    predictors = vi_predictors,
    max_cor = 0.75
  )

  # Test with only numeric predictors
  selected_predictors <- vif_select(
    df = vi,
    predictors = vi_predictors,
    max_vif = 10
    )

  # Check that the result is a character vector
  testthat::expect_true(is.character(selected_predictors))
  testthat::expect_true(all(selected_predictors %in% vi_predictors))

  # Test with response variable included
  selected_predictors <- vif_select(
    df = vi,
    response = "vi_mean",
    predictors = vi_predictors,
    max_vif = 2.5
    )

  # Check that the result is a character vector
  testthat::expect_true(is.character(selected_predictors))
  testthat::expect_true(all(selected_predictors %in% vi_predictors))

  # Test with a user-defined preference order
  user_preference_order <- c(
    "soil_type",
    "soil_temperature_mean",
    "swi_mean", "rainfall_mean",
    "evapotranspiration_mean"
    )

  selected_predictors3<- vif_select(
    df = vi, response = "vi_mean",
    predictors = vi_predictors,
    preference_order = user_preference_order,
    max_vif = 2.5
    )

  # Check that the result is a character vector
  testthat::expect_true(is.character(selected_predictors))
  testthat::expect_true(all(selected_predictors %in% vi_predictors))

})
