#' Target encoding of non-numeric variables
#'
#' @description
#'
#' Target encoding involves replacing the values of categorical variables with numeric ones from a "target variable", usually a model's response. Target encoding can be useful for improving the performance of machine learning models.
#'
#' This function identifies categorical variables in the input data frame, transforms them using a set of target-encoding methods defined by the user, and returns the input data frame with the newly encoded variables.
#'
#' The target encoding methods implemented in this function are:
#'
#' \itemize{
#'   \item `rank`: returns the order of the group as a integer, starting with the value 1 as the rank of the group with the lower mean of the response variable. This method accepts the  `noise` argument, which adds white noise to the result to increase data variability and reduce overfitting. The variables returned by this method are named with the suffix "__encoded_rank". This method is implemented in the function [target_encoding_rank()].
#'   \item `mean`: uses the mean value of the response over each group in the categorical variable. This option accepts `noise`. The variables returned by this method are named with the suffix "__encoded_mean".  This method is implemented in the function [target_encoding_mean()].
#'   \item `rnorm`: This method computes the mean and standard deviation of the response for each group of the categorical variable, and uses [rnorm()] to generate values taken from a normal distribution. The argument `sd_width` is used as a multiplier of the standard deviation to reduce the range of values produced by [rnorm()] for each group of the categorical predictor. The variables returned by this method are named with the suffix "__encoded_rnorm".  This method is implemented in the function [target_encoding_rnorm()].
#'   \item `loo`: This is the leave-one-out method. Each categorical value is replaced with the mean of the response variable across the other cases within the same group. The variables returned by this method are named with the suffix "__encoded_loo". This method is implemented in the function [target_encoding_loo()].
#' }
#'
#' The methods "mean" and "rank" support the `noise` argument. Values larger than zero in this argument add white noise to the target-encoded variables using `stats::rnorm()` via the function [target_encoding_noise()]). The `noise` argument represents a fraction of the average differences between groups of the target-encoded variable. For example, if noise = 0.25 and the target-encoded variable has the unique values c(1, 2, 3), as it could be the case when using the "rank" method, then the average between-groups difference would be 1, and the range of the noise added to each row would go between 0 and 0.25
#'
#' The method "rnorm" has the argument `sd_width`, which multiplies the standard deviation argument of the `rnorm()` function to limit the spread of the encoded values between groups.
#'
#' @param df (required; data frame, tibble, or sf) A training data frame. Default: `NULL`
#' @param response (required; character string) Name of the response. Must be a column name of `df`. Default: `NULL`
#' @param predictors (required; character vector) Names of all the predictors in `df`. Only character and factor predictors are processed, but all are returned in the "df" slot of the function's output.  Default: `NULL`
#' @param methods (optional; character string). Name of the target encoding methods. Default: `c("mean", "rank", "loo", "rnorm")`
#' @param seed (optional; integer) Random seed to facilitate reproducibility when `noise` is not 0. Default: 1
#' @param noise (optional; numeric vector) Numeric vector with noise values in the range 0-1. Used only in methods "mean" and "rank". Generates white noise to reduce overfitting. Default: 0.
#' @param sd_width (optional; numeric vector) Only for the method "rnorm". Numeric vector with multiplicators of the standard deviation of each group in the categorical variable, in the range 0.01-1. Default: 0.1
#' @param replace (optional; logical) Advanced option that changes the behavior of the function. Use only if you really know exactly what you neare doing. If `TRUE`, only the first option in the `methods`, `noise`, and `sd_width` arguments is used, it replaces each categorical variable with its encoded version, and returns the input data frame with the encoded variables and without the original ones. Default: FALSE
#' @param verbose (optional; logical) If TRUE, messages generated during the execution of the function are printed to the console Default: TRUE
#'
#' @return
#' If no target encoding is needed because all predictors are numeric, the function returns `df`.
#'
#' Otherwise it returns a list with these slots:
#' \itemize{
#'   \item `data`: Input data frame, but with target-encoded character or factor columns.
#'   \item `correlation_test`: Data frame with the results of a linear model between the target-encoded variable and the response. It contains the following columns:
#'   \itemize{
#'     \item `encoded_predictor`: name of the target-encoded variable.
#'     \item `correlation_with_response`: R-squared resulting from `cor.test()` on the target-encoded variable and the response.
#'   }
#' }
#'
#' If the option `replace` is `TRUE`, then the input data frame is returned with the categorical variables replaced with their encoded versions.
#'
#'
#' @examples
#' if(interactive()){
#'
#' #loading example data
#' data(
#'   vi,
#'   vi_predictors
#'   )
#'
#' #applying all methods for a continuous response
#' output <- target_encoding_lab(
#'   data = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors,
#'   methods = c(
#'     "mean",
#'     "rank",
#'     "rnorm",
#'     "loo"
#'   ),
#'   sd_width = c(0.01, 0.1, 1),
#'   noise = c(0, 1)
#' )
#'
#' #the output has several objects
#' names(output)
#'
#' #names of the encoded predictors
#' output$encoded_predictors
#'
#' #the data with the original and the encoded predictors
#' colnames(output$data)
#'
#' #a correlation test assessing the correlation between the response and the encoded predictors
#' output$leakage_test
#'
#' #plotting the transformations of "primary_productivity"
#' tidyr::pivot_longer(
#'   data = output$data,
#'   cols = dplyr::all_of(
#'     grep(
#'       pattern = "primary_productivity",
#'       x = output$encoded_predictors,
#'       value = TRUE)
#'     )
#' ) |>
#'   dplyr::select(
#'     plant_richness,
#'     primary_productivity,
#'     name,
#'     value
#'   ) |>
#'   ggplot2::ggplot() +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = value,
#'     color = primary_productivity
#'   ) +
#'   ggplot2::facet_wrap(~name, scales = "free_y") +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(
#'     x = "Response values",
#'     y = "Encoded values",
#'     color = "Original\ngroups"
#'   )
#'
#' }
#' @autoglobal
#' @export
target_encoding_lab <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    methods = c(
      "mean",
      "rank",
      "loo",
      "rnorm"
      ),
    sd_width = 0.1,
    seed = 1,
    noise = 0,
    replace = FALSE,
    verbose = TRUE
){

  #testing method argument
  methods <- match.arg(
    arg = methods,
    choices = c(
      "mean",
      "rank",
      "loo",
      "rnorm"
    ),
    several.ok = TRUE
  )


  if(replace == TRUE){
    verbose <- FALSE
    methods <- methods[1]
    noise <- noise[1]
    sd_width <- sd_width[1]
  }

  #factors to characters
  df <- rapply(
    object = df,
    f = as.character,
    classes = c(
      "factor",
      "ordered",
      "logical"
    ),
    how = "replace"
  )

  #return data if all predictors are numeric
  predictors.non.numeric <- identify_non_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(length(predictors.non.numeric) == 0){

    if(verbose == TRUE){
      message("All predictors are numeric, returning the input data frame.")
    }

    return(data)

  }

  if(verbose == TRUE){
    message(
      "Encoding the variables:\n",
      paste0(
        predictors.non.numeric,
        collapse = "\n"
      ),
      "\n"
    )
  }

  #original column names
  original.column.names <- colnames(df)

  #iterating over categorical variables
  for(predictors.non.numeric.i in predictors.non.numeric){

      #method "mean"
      if("mean" %in% methods){

        for(noise.i in noise){

        df <- target_encoding_mean(
          df = df,
          response = response,
          predictor = predictors.non.numeric.i,
          noise = noise.i,
          seed = seed,
          replace = replace,
          verbose = verbose
        )

        }

      }

      if("rank" %in% methods){

        for(noise.i in noise){

        df <- target_encoding_rank(
          df = df,
          response = response,
          predictor = predictors.non.numeric.i,
          noise = noise.i,
          seed = seed,
          replace = replace,
          verbose = verbose
        )

        }

      }


    if("rnorm" %in% methods){

      for(sd_width.i in sd_width){

        df <- target_encoding_rnorm(
          df = df,
          response = response,
          predictor = predictors.non.numeric.i,
          sd_width = sd_width.i,
          seed = seed,
          replace = replace,
          verbose = verbose
        )

      }

    }

    if("loo" %in% methods){

      df <- target_encoding_loo(
        df = df,
        response = response,
        predictor = predictors.non.numeric.i,
        replace = replace,
        verbose = verbose
      )

    }

  } #end of iteration over predictors

  #new variables
  if(replace == FALSE){

    encoded.predictors <- setdiff(
      x = colnames(df),
      y = original.column.names
    )

    correlation.df <- lapply(
      X = df[, encoded.predictors],
      FUN = function(x){
        stats::cor.test(
          x,
          df[[response]]
        )$estimate
      }
    ) |>
      unlist() |>
      as.data.frame() |>
      dplyr::rename(
        r_squared = "."
      ) |>
      dplyr::mutate(
        r_squared = round(r_squared, 2),
        encoded_predictor = encoded.predictors
      ) |>
      dplyr::transmute(
        encoded_predictor,
        correlation_with_response = r_squared
      ) |>
      dplyr::arrange(
        correlation_with_response
      )

    rownames(correlation.df) <- seq_len(nrow(correlation.df))

    #message with output
    if(verbose == TRUE){
      message(
        "Correlation test for method:\n\n",
        paste0(
          utils::capture.output(as.data.frame(correlation.df)),
          collapse = "\n"
        ),
        "\n\nr_squared: correlation between the target-encoded variable and the response.\n"
      )
    }

  }

  #return df frame right away if replace is TRUE
  if(replace == TRUE){
    return(df)
  }

  #preparing output object
  out <- list(
    encoded_predictors = encoded.predictors,
    df = df,
    correlation_test = correlation.df
  )

  #return output
  out

}


