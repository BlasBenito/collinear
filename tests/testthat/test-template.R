testthat::test_that("mc_ functions work", {

  library(collinear)

  #loading data
  data(
    vi,
    vi_predictors
  )

  #adding zero variance zolumn
  #passing zero variance column
  vi$zeros <- runif(n = nrow(vi)) /10000
  vi_tibble$zeros <- runif(n = nrow(vi)) /10000
  vi_sf$zeros <- runif(n = nrow(vi)) /10000

  vi_predictors <- c(vi_predictors, "zeros")

  #making a set of not collinear variables
  variables_not_collinear <- cor_select(
    data = vi,
    predictor.variable.names = vi_predictors
  )

  #lists to iterate over
  training <- list(
    df = vi,
    sf = vi_sf,
    tibble = vi_tibble,
    null = NULL
  )

  response <- list(
    continuous = vi_continuous_response,
    binary = vi_binary_response,
    null = NULL,
    other = "other"
  )

  predictors <- list(
    all = vi_all_predictors,
    not_collinear = variables_not_collinear,
    two = vi_predictors[1:2],
    one = vi_predictors[1],
    null = NULL
  )

  preference <- list(
    all = vi_all_predictors,
    partial = vi_all_predictors[1:10],
    null = NULL
  )

  max.cor <- list(
    uno = -0.5,
    dos = 0.5,
    tres = 1.5
  )

  max.vif <- list(
    uno = -5,
    dos = 0,
    tres = 5,
    cuatro = 15
  )

  for(training.i in names(training)){
    for(response.i in names(response)){
      for(predictors.i in names(predictors)){
        for(preference.i in names(preference)){

          #MC AUTO
          for(max.cor.i in names(max.cor)){
            for(max.vif.i in names(max.vif)){

              #exceptions for mc_auto
              if(
                max.cor[[max.cor.i]] < 0 |
                max.cor[[max.cor.i]] > 1 |
                max.vif[[max.vif.i]] < 0 |
                is.null(max.cor[[max.cor.i]]) |
                is.null(max.vif[[max.vif.i]]) |
                is.null(training[[training.i]]) |
                predictors.i == "one"
              ){

                testthat::expect_error(
                  x <- mc_auto(
                    data = training[[training.i]],
                    predictor.variable.names = predictors[[predictors.i]],
                    dependent.variable.name = response[[response.i]],
                    preference.order = preference[[preference.i]],
                    max.cor = max.cor[[max.cor.i]],
                    max.vif = max.vif[[max.vif.i]],
                    verbose = TRUE
                  )
                )

              } else {

                #non-exceptions
                x <- mc_auto(
                  data = training[[training.i]],
                  predictor.variable.names = predictors[[predictors.i]],
                  dependent.variable.name = response[[response.i]],
                  preference.order = preference[[preference.i]],
                  max.cor = max.cor[[max.cor.i]],
                  max.vif = max.vif[[max.vif.i]],
                  verbose = TRUE
                )

              }

              #exceptions for vif_select
              if(
                max.vif[[max.vif.i]] < 0 |
                is.null(max.vif[[max.vif.i]]) |
                is.null(training[[training.i]]) |
                predictors.i %in% c("all", "null") |
                predictors.i == "one"
              ){

                testthat::expect_error(
                  x <- vif_select(
                    data = training[[training.i]],
                    predictor.variable.names = predictors[[predictors.i]],
                    dependent.variable.name = response[[response.i]],
                    preference.order = preference[[preference.i]],
                    max.vif = max.vif[[max.vif.i]],
                    verbose = TRUE
                  )
                )

              } else {

                x <- vif_select(
                  data = training[[training.i]],
                  predictor.variable.names = predictors[[predictors.i]],
                  dependent.variable.name = response[[response.i]],
                  preference.order = preference[[preference.i]],
                  max.vif = max.vif[[max.vif.i]],
                  verbose = TRUE
                )

              }
            }
          }

          #mc_cor

          if(
            training.i == "null" |
            predictors.i == "one"
          ){

            testthat::expect_error(
              x <- mc_cor(
                data = training[[training.i]],
                predictor.variable.names = predictors[[predictors.i]],
                dependent.variable.name = response[[response.i]]
              )
            )

          } else {

            x <- mc_cor(
              data = training[[training.i]],
              predictor.variable.names = predictors[[predictors.i]],
              dependent.variable.name = response[[response.i]]
            )

          }


          #mc_vif
          #exception when there is too much correlation between variables
          if(
            predictors.i %in% c("all", "null") |
            training.i == "null" |
            predictors.i == "one"
          ){

            testthat::expect_error(
              x <- mc_vif(
                data = training[[training.i]],
                predictor.variable.names = predictors[[predictors.i]],
                dependent.variable.name = response[[response.i]]
              )
            )

          } else {

            x <- mc_vif(
              data = training[[training.i]],
              predictor.variable.names = predictors[[predictors.i]],
              dependent.variable.name = response[[response.i]]
            )

          }
        }
      }
    }
  }


})
