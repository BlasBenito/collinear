#' R-squared of Gaussian GAM Predictions vs. Observations
#'
#' @description
#' Fits a Gaussian GAM model \code{y ~ s(x)} (\code{y ~ x} if \code{x} is non-numeric) with the numeric response \code{y} and the numeric, character or factor predictor \code{x} using [mgcv::gam()] and returns the R-squared of the observations against the predictions (see [score_r2()]).
#'
#' Supports cross-validation via the arguments arguments \code{cv_training_fraction} (numeric between 0 and 1) and \code{cv_iterations} (integer between 1 and \code{n}) introduced via ellipsis (\code{...}). See [preference_order()] for further details.
#'
#'
#' @inheritParams f_numeric_glm
#' @return numeric or numeric vector: R-squared
#' @examples
#'
#' data(vi_smol)
#'
#' df <- data.frame(
#'   y = vi_smol[["vi_numeric"]],
#'   x = vi_smol[["swi_max"]]
#' )
#'
#' #no cross-validation
#' f_numeric_gam(df = df)
#'
#' #cross-validation
#' f_numeric_gam(
#'   df = df,
#'   cv_training_fraction = 0.5,
#'   cv_iterations = 10
#'   )
#'
#' #categorical predictor
#' df <- data.frame(
#'   y = vi_smol[["vi_numeric"]],
#'   x = vi_smol[["koppen_zone"]]
#' )
#'
#' f_numeric_gam(df = df)
#'
#'
#' @family preference_order_functions
#' @autoglobal
#' @export
f_numeric_gam <- function(
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
    default_name = "collinear::f_numeric_gam()",
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

  if (!is.numeric(df[["y"]])) {
    stop(
      "\n",
      function_name,
      ": column 'y' of dataframe 'df' must be numeric.",
      call. = FALSE
    )
  }

  formula <- stats::as.formula(
    object = "y ~ s(x)"
  )

  if (
    !is.numeric(df[["x"]]) ||
      length(unique(df[["x"]])) < 4
  ) {
    formula <- stats::as.formula(
      object = "y ~ x"
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

    scores[i] <- tryCatch(
      {
        #train
        m <- mgcv::gam(
          formula = formula,
          data = train_df,
          family = stats::gaussian(link = "identity"),
          select = TRUE
        )

        #predict
        p <- stats::predict(
          object = m,
          newdata = test_df,
          type = "response"
        )

        #evaluate
        score <- score_r2(
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
