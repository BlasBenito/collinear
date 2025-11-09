#' Pairwise Correlation Dataframe
#'
#' @description
#' Computes absolute pairwise correlations between predictors using appropriate methods for different variable types:
#' \itemize{
#'   \item **Numeric vs. Numeric**: Absolute Pearson correlation via [stats::cor()].
#'   \item **Numeric vs. Categorical**: Target-encodes the categorical variable  using the numeric variable as reference via [target_encoding_lab()] with leave-one-out method, then computes absolute Pearson correlation.
#'   \item **Categorical vs. Categorical**: Cramer's V via [cor_cramer()] as a measure of association. See [cor_cramer()] for important notes on mixing Pearson correlation and Cramer's V in multicollinearity analysis.
#' }
#'
#' Parallelization via [future::plan()] and progress bars via [progressr::handlers()] are supported but only beneficial for large datasets with categorical predictors. Numeric-only correlations do not use parallelization or progress bars. Example: With 16 workers, 30k rows (dataframe [vi]), 49 numeric and 12 categorical predictors (see [vi_predictors]), parallelization achieves a 5.4x speedup (147s → 27s).
#'
#' @inheritParams collinear
#'
#' @return dataframe with columns:
#' \itemize{
#'   \item \code{x}: character, first predictor name.
#'   \item \code{y}: character, second predictor name.
#'   \item \code{correlation}: numeric, absolute Pearson correlation (numeric vs. numeric and numeric vs. categorical) or Cramer's V (categorical vs. categorical).
#' }
#'
#'
#' @examples
#' data(vi_smol)
#'
#' ## OPTIONAL: parallelization setup
#' ## irrelevant when all predictors are numeric
#' ## only worth it for large data with many categoricals
#' # future::plan(
#' #   future::multisession,
#' #   workers = future::availableCores() - 1
#' # )
#'
#' ## OPTIONAL: progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #predictors
#' predictors = c(
#'   "koppen_zone", #character
#'   "soil_type", #factor
#'   "topo_elevation", #numeric
#'   "soil_temperature_mean" #numeric
#' )
#'
#' x <- cor_df(
#'   df = vi_smol,
#'   predictors = predictors
#' )
#'
#' x
#'
#' ## OPTIONAL: disable parallelization
#' #future::plan(future::sequential)
#' @autoglobal
#' @family pairwise_correlation
#' @export
cor_df <- function(
    df = NULL,
    predictors = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::cor_df()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  quiet <- validate_arg_quiet(
    quiet = quiet,
    function_name = function_name
  )

  predictors <- validate_arg_predictors(
    df = df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  df.ncol <- ncol(df)

  df <- validate_arg_df(
    df = df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  #revalidate predictors if any columns were removed
  if(ncol(df) < df.ncol){

    attributes(predictors)$validated <- NULL

    predictors <- validate_arg_predictors(
      df = df,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  }

  #identify predictors types
  predictors <- identify_valid_variables(
    df = df,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  #univariate case
  if(length(c(predictors$numeric, predictors$categorical)) == 1){

    if(quiet == FALSE){

      message(
        "\n",
        function_name,
        ": only one valid predictor, returning one-row dataframe."
      )

    }

    temp <- c(predictors$numeric, predictors$categorical)

    out_df <- data.frame(
      x = temp,
      y = temp,
      correlation = 1,
      metric = ifelse(
        test = length(predictors$categorical) == 1,
        yes = "cramer_v",
        no = "pearson"
      )
    )

    class(out_df) <- c("collinear_cor_df", class(out_df))

    return(out_df)

  }

  #initialize output dataframes
  numerics_df <- NULL
  categoricals_df <- NULL

  #categoricals
  df_num_cat <- NULL
  df_cat_cat <- NULL

  #num vs cat
  if(
    all(
      c(
        length(predictors$categorical),
        length(predictors$numeric)
      ) > 0
    )
  ){

    #numerics_vs_categoricals
    df_num_cat <- expand.grid(
      x = predictors$numeric,
      y = predictors$categorical,
      correlation = NA_real_,
      metric = "Pearson",
      type = 1,
      stringsAsFactors = FALSE
    )

  }

  #cat vs cat
  if(length(predictors$categorical) > 1){

    pairs_cat_cat <- t(
      utils::combn(
        x = predictors$categorical,
        m = 2
      )
    )

    df_cat_cat <- data.frame(
      x = pairs_cat_cat[, 1],
      y = pairs_cat_cat[, 2],
      correlation = NA_real_,
      metric = "Cramer's V",
      type = 2,
      stringsAsFactors = FALSE
    )

  }

  #df to iterate over
  categoricals_df <- rbind(
    df_num_cat,
    df_cat_cat
  )

  #compute iterations for categorical vars
  iterations_categorical <- ifelse(
    test = !is.null(categoricals_df) && nrow(categoricals_df) > 0,
    yes = nrow(categoricals_df),
    no = 0
  )

  if(iterations_categorical > 0){

    p <- progressr::progressor(
      steps = iterations_categorical
    )


    categoricals_df$correlation <- future.apply::future_apply(
      X = categoricals_df,
      MARGIN = 1,
      FUN = function(x){

        p()

        df.x <- data.frame(
          x = df[[x[1]]],
          y = df[[x[2]]]
        ) |>
          stats::na.omit()

        #num_vs_cat
        if(x[5] == "1"){

          attr(
            x = df.x,
            which = "validated"
          ) <- TRUE

          #target encode
          df.x <- target_encoding_lab(
            df = df.x,
            response = "x",
            predictors = "y",
            encoding_method = "loo",
            overwrite = TRUE,
            quiet = TRUE,
            function_name = function_name
          )

          #compute correlation
          score <- stats::cor(
            x = df.x$x,
            y = df.x$y,
            use = "complete.obs",
            method = "pearson"
          ) |>
            abs()

        } else {

          #cat vs cat
          score <- cor_cramer(
            x = df.x$x,
            y = df.x$y,
            check_input = FALSE,
            function_name = function_name
          ) |>
            abs()

        }

        score

      }, #end of lambda function
      future.seed = TRUE
    )

    #remove type
    categoricals_df$type <- NULL

  }


  #numerics
  if(length(predictors$numeric) > 1){

    numerics_matrix <- stats::cor(
      x = df[, predictors$numeric, drop = FALSE],
      use = "complete.obs",
      method = "pearson"
    ) |>
      abs()

    upper_indices <- which(
      x = upper.tri(numerics_matrix),
      arr.ind = TRUE
    )

    numerics_df <- data.frame(
      x = rownames(numerics_matrix)[upper_indices[, 1]],
      y = colnames(numerics_matrix)[upper_indices[, 2]],
      correlation = numerics_matrix[upper_indices],
      metric = "Pearson",
      stringsAsFactors = FALSE
    )

  }

  out_df <- rbind(
    numerics_df,
    categoricals_df
  )

  #arrange by correlation values
  out_df <- out_df[
    order(
      out_df$correlation,
      decreasing = TRUE
    ),
    , drop = FALSE
  ]

  rownames(out_df) <- NULL

  class(out_df) <- c("collinear_cor_df", class(out_df))

  out_df

}

