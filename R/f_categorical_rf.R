#' Cramer's V of Categorical Random Forest predictions vs. observations
#'
#' @description
#' Fits a univariate random forest model \code{y ~ x} with the character or factor response \code{y} and the numeric, character or factor predictor \code{x} using \code{ranger::ranger()} and returns the Cramer's V (see [cor_cramer()]) between the observed responses and the model predictions. Cases are weighted with [case_weights()] to prevent issues arising from class imbalance.
#'
#' Cases are weighted with [case_weights()] to prevent issues arising from class imbalance.
#'
#' Supports cross-validation via the arguments arguments \code{cv_training_fraction} (numeric between 0 and 1) and \code{cv_iterations} (integer between 1 and \code{n}) introduced via ellipsis (\code{...}). See [preference_order()] for further details.
#'
#' @param df (required, dataframe) with columns:
#' \itemize{
#'   \item \code{x}: (numeric) numeric, character, or factor predictor.
#'   \item \code{y} (numeric) character or factor response.
#' }
#' @inheritParams f_numeric_glm
#' @return numeric or numeric vector: Cramer's V
#' @examples
#' data(vi_smol)
#'
#' df <- data.frame(
#'   y = vi_smol[["vi_factor"]],
#'   x = vi_smol[["soil_type"]]
#' )
#'
#' #no cross-validation
#' f_categorical_rf(df = df)
#'
#' #cross-validation
#' f_categorical_rf(
#'   df = df,
#'   cv_training_fraction = 0.5,
#'   cv_iterations = 10
#'   )
#'
#' #numeric predictor
#' df <- data.frame(
#'   y = vi_smol[["vi_categorical"]],
#'   x = vi_smol[["swi_max"]]
#' )
#'
#' f_categorical_rf(df = df)
#'
#' @family preference_order_functions
#' @autoglobal
#' @export
f_categorical_rf <- function(
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
    default_name = "collinear::f_categorical_rf()",
    function_name = dots$function_name
  )

  #check column names in df
  if (!all(c("x", "y") %in% names(df))) {
    stop(
      "\n",
      function_name,
      ": dataframe 'df' must have the column names 'x' and 'y'."
    )
  }

  if (!class(df[["y"]]) %in% c("character", "factor")) {
    stop(
      "\n",
      function_name,
      ": column 'y' of dataframe 'df' must be character or factor.",
      call. = FALSE
    )
  }

  if (!is.factor(df[["y"]])) {
    df[["y"]] <- as.factor(df[["y"]])
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
        m <- ranger::ranger(
          formula = y ~ x,
          data = train_df,
          case.weights = case_weights(
            x = train_df[["y"]],
            function_name = function_name
          ),
          num.threads = 1,
          num.trees = 100,
          min.node.size = max(c(floor(nrow(train_df) * 0.05), 5)),
          seed = i
        )

        #predict
        p <- stats::predict(
          object = m,
          data = test_df
        )$predictions

        #evaluate
        score <- score_cramer(
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
