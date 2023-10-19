testthat::test_that("`collinear()` works", {

    data(vi, vi_predictors)

    vi <- vi[1:1000, ]

    #without response, only numeric predictors processed
    ####################################################

    #permissive selection
    selected.predictors.1 <- collinear(
      df = vi,
      predictors = vi_predictors,
      max_cor = 0.8,
      max_vif = 10
    )

    #restrictive selection
    selected.predictors.2 <- collinear(
      df = vi,
      predictors = vi_predictors,
      max_cor = 0.5,
      max_vif = 2.5
    )

    #basic tests
    testthat::expect_true(
      is.vector(selected.predictors.1)
    )

    testthat::expect_true(
      is.vector(selected.predictors.2)
    )

    testthat::expect_true(
      length(selected.predictors.1) > 0
    )

    testthat::expect_true(
      length(selected.predictors.2) > 0
    )

    testthat::expect_true(
      length(selected.predictors.1) > length(selected.predictors.2)
    )

    #advanced test
    non.numeric.predictors <- identify_non_numeric_predictors(
      df = vi,
      predictors = vi_predictors
    )

    testthat::expect_true(
      !all(non.numeric.predictors %in% selected.predictors.1)
    )

    testthat::expect_true(
      !all(non.numeric.predictors %in% selected.predictors.2)
    )

    #with response, numerics and non numerics processed
    ######################################################

    selected.predictors <- collinear(
      df = vi,
      response = "vi_mean",
      predictors = vi_predictors,
      max_cor = 0.8,
      max_vif = 10
    )

    testthat::expect_true(
      is.vector(selected.predictors)
    )

    testthat::expect_true(
      length(selected.predictors) > 0
    )

    #check that there are non-numerics selected
    testthat::expect_true(
      sum(non.numeric.predictors %in% selected.predictors) > 0
    )

    #with response and preference order
    ######################################################

    selected.predictors <- collinear(
      df = vi,
      response = "vi_mean",
      predictors = vi_predictors,
      preference_order = c(
        "soil_temperature_mean",
        "swi_mean",
        "rainfall_mean",
        "soil_nitrogen"
      ),
      max_cor = 0.8,
      max_vif = 10
    )

    testthat::expect_true(
      is.vector(selected.predictors)
    )

    testthat::expect_true(
      length(selected.predictors) > 0
    )

    testthat::expect_true(
      "soil_temperature_mean" %in% selected.predictors
    )

    testthat::expect_true(
      "swi_mean" %in% selected.predictors
    )

    testthat::expect_true(
      "rainfall_mean" %in% selected.predictors
    )

    testthat::expect_true(
      "soil_nitrogen" %in% selected.predictors
    )


})
