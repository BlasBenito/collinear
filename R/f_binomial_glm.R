#' Area Under the Curve of Binomial GLM Predictions vs. Observations
#'
#' @description
#' Fits a Quasibinomial GLM model \code{y ~ x} with the binomial  response \code{y} (values 0 and 1) and the numeric, character, or factor predictor \code{x} using [stats::glm()] and returns the area under the ROC curve of the observations against the predictions (see [score_auc()]).
#'
#' Cases are weighted with [case_weights()] to prevent issues arising from class imbalance.
#'
#' Supports cross-validation via the arguments arguments \code{cv_training_fraction} (numeric between 0 and 1) and \code{cv_iterations} (integer between 1 and \code{n}) introduced via ellipsis (\code{...}). See [preference_order()] for further details.
#'
#' @param df (required, dataframe) with columns:
#' \itemize{
#'   \item "x": (numeric, character, factor) predictor.
#'   \item "y" (integer) binomial response with unique values 0 and 1.
#' }
#' @inheritParams f_numeric_glm
#' @return numeric or numeric vector: AUC
#' @examples
#' data(vi_smol)
#'
#' df <- data.frame(
#'   y = vi_smol[["vi_binomial"]],
#'   x = vi_smol[["swi_max"]]
#' )
#'
#' #no cross-validation
#' f_binomial_glm(df = df)
#'
#' #cross-validation
#' f_binomial_glm(
#'   df = df,
#'   cv_training_fraction = 0.5,
#'   cv_iterations = 10
#'   )
#'
#' #categorical predictor
#' df <- data.frame(
#'   y = vi_smol[["vi_binomial"]],
#'   x = vi_smol[["koppen_zone"]]
#' )
#'
#' f_binomial_glm(df = df)
#' @family preference_order_functions
#' @autoglobal
#' @export
f_binomial_glm <- function(
  df,
  ...
) {
  dots <- list(...)

  cv_training_fraction <- if (is.null(dots$cv_training_fraction)) {
    1
  } else {
    dots$cv_training_fraction
  }

  cv_iterations <- if (is.null(dots$cv_iterations)) 1 else dots$cv_iterations

  function_name <- validate_arg_function_name(
    default_name = "collinear::f_binomial_glm()",
    function_name = dots$function_name
  )

  #check column names in df
  if (!all(c("x", "y") %in% names(df))) {
    stop(
      "\n",
      function_name,
      ": dataframe 'df' must have the column names 'x' and 'y'.",
      call. = FALSE
    )
  }

  u <- unique(df[["y"]])
  if (
    !is.numeric(df[["y"]]) ||
      !is.integer(df[["y"]]) ||
      length(u) != 2L ||
      !all(u %in% 0:1)
  ) {
    stop(
      "\n",
      function_name,
      ": column 'y' of dataframe 'df' must be integer and have 0 and 1 as unique values.",
      call. = FALSE
    )
  }

  df <- stats::na.omit(object = df)

  scores <- rep(x = NA_real_, times = cv_iterations)

  #iterations
  for (i in seq_len(cv_iterations)) {
    #data split
    if (cv_training_fraction < 1) {
      train_indices <- sample(
        x = nrow(df),
        size = floor(nrow(df) * cv_training_fraction),
        replace = FALSE
      )

      train_df <- df[train_indices, , drop = FALSE]
      test_df <- df[-train_indices, , drop = FALSE]

      if (!is.numeric(df[["x"]])) {
        test_df <- test_df[test_df[["x"]] %in% train_df[["x"]], ]
      }
    } else {
      train_df <- df
      test_df <- df
    }

    if (nrow(test_df) < 2) {
      next
    }

    scores[i] <- tryCatch(
      {
        #train
        m <- stats::glm(
          formula = y ~ x,
          data = train_df,
          family = stats::quasibinomial(
            link = "logit"
          ),
          weights = case_weights(
            x = train_df[["y"]],
            function_name = function_name
          )
        )

        #predict
        p <- stats::predict(
          object = m,
          newdata = test_df,
          type = "response"
        )

        #evaluate
        score <- score_auc(
          o = test_df[["y"]],
          p = p,
          function_name = function_name
        )
      },
      error = function(e) {
        return(NA)
      }
    )
  }

  scores
}
