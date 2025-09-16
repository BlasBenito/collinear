#' Autoconfigure Argument \code{max_vif}
#'
#' If \code{max_vif = NULL}, returns the prediction of the model [gam_cor_to_vif] on the input \code{max_cor} (see [experiment_df] for further details).
#'
#' @inheritParams collinear
#' @param max_vif optional, numeric or NULL) Maximum Variance Inflation Factor allowed for \code{predictors} during multicollinearity filtering. Recommended values are between 2 (strict) and 10 (permissive). If NULL, the value is autoconfigured depending on the average pairwise correlation of the dataset.
#' @inheritParams validate_arg_quiet
#'
#' @returns numeric
#' @export
#' @autoglobal
#' @examples
#' max_vif <- autoconfig_arg_max_vif(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric,
#'   max_vif = NULL
#' )
#'
#' max_vif
autoconfig_arg_max_vif <- function(
    df = NULL,
    predictors = NULL,
    max_cor = NULL,
    max_vif = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::autoconfig_arg_max_vif()",
    ... = ...
  )

  if(
    is.numeric(max_vif) &&
    (max_vif > 10 || max_vif < 2.5)
  ){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": invalid 'max_vif' value (valid range is [2.5, 10]), setting it to NULL to proceed with the autoconfiguration."
      )

    }

    max_vif <- NULL

  }

  if(is.null(max_vif)){

    predictors.numeric <- identify_predictors_numeric(
      df = df,
      predictors = predictors,
      quiet = quiet
    )

    if(is.null(predictors.numeric)){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": no numeric variables available, skipping VIF autoconfiguration (max_vif = NULL)."
        )

      }

      return(NULL)

    } else if(length(predictors.numeric) == 1){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": only one numeric variable available, skipping VIF autoconfiguration (max_vif = NULL)."
        )

      }

      return(NULL)

    }

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": autoconfiguring 'max_vif' ..."
      )

    }

    if(is.null(max_cor)){

      max_cor <- autoconfig_arg_max_cor(
        df = df,
        predictors = predictors.numeric,
        max_cor = max_cor,
        function_name = NULL,
        quiet = TRUE
      )

    }


      max_vif <- mgcv::predict.gam(
        object = gam_cor_to_vif,
        newdata = data.frame(
          max_cor = max_cor
        )
      ) |>
        round(digits = 2)

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": 'max_vif' = ",
          max_vif
        )

      }

  }

  max_vif <- validate_arg_max_vif(
    max_vif = max_vif,
    function_name = function_name,
    quiet = quiet
  )

  max_vif


}
