#' Correlation data frame of numeric and character variables
#'
#' @description Returns a correlation data frame between all pairs of predictors in a training dataset. Non-numeric predictors are transformed into numeric via target encoding, using the 'response' variable as reference.
#'
#' @details
#' This function attempts to handle correlations between pairs of variables that can be of different types:
#' \itemize{
#'   \item numeric vs. numeric: computed with stats::cor() with the methods "pearson" or "spearman".
#'   \item numeric vs. character, two alternatives leading to different results:
#'   \itemize{
#'     \item 'response' is provided: the character variable is target-encoded as numeric using the values of the response as reference, and then its correlation with the numeric variable is computed with stats::cor(). This option generates a response-specific result suitable for training statistical and machine-learning models
#'     \item 'response' is NULL (or the name of a non-numeric column): the character variable is target-encoded as numeric using the values of the numeric predictor (instead of the response) as reference, and then their correlation is computed with stats::cor(). This option leads to a response-agnostic result suitable for clustering problems.
#'   }
#'   \item character vs. character, two alternatives leading to different results:
#'   \itemize{
#'     \item 'response' is provided: the character variables are target-encoded as numeric using the values of the response as reference, and then their correlation is computed with stats::cor().
#'     \item response' is NULL (or the name of a non-numeric column): the association between the character variables is computed using Cramer's V. This option might be problematic, because R-squared values and Cramer's V, even when having the same range between 0 and 1, are not fully comparable.
#'   }
#' }
#'
#' @param df (required; data frame) A data frame with numeric and/or character predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param cor_method (optional; character string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#' @param encoding_method (optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: "mean"
#'
#' @return data frame with pairs of predictors and their correlation.
#'
#' @examples
#'
#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' #reduce size of vi to speed-up example execution
#' vi <- vi[1:1000, ]
#' vi_predictors <- vi_predictors[1:10]
#'
#' #without response
#' #categorical vs categorical compared with cramer_v()
#' #categorical vs numerical compared wit stats::cor() via target-encoding
#' #numerical vs numerical compared with stats::cor()
#' df <- cor_df(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' head(df)
#'
#' #with response
#' #different solution than previous one
#' #because target encoding is done against the response
#' #rather than against the other numeric in the pair
#' df <- cor_df(
#'   df = vi,
#'   response = "vi_mean",
#'   predictors = vi_predictors
#' )
#'
#' head(df)
#'
#' @autoglobal
#' @author Blas M. Benito
#' @export
cor_df <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    cor_method = "pearson",
    encoding_method = "mean"
){

  #method argument for stats::cor
  cor_method <- match.arg(
    arg = cor_method,
    choices = c(
      "pearson",
      "spearman"
    ),
    several.ok = FALSE
  )

  #validate input data frame
  df <- validate_df(
    df = df,
    min_rows = ifelse(
      test = cor_method == "pearson",
      yes = 30,
      no = 10
    )
  )

  #validate predictors
  predictors <- validate_predictors(
    df = df,
    response = response,
    predictors = predictors,
    min_numerics = 0
  )

  #target encode character predictors
  df <- target_encoding_lab(
    df = df,
    response = response,
    predictors = predictors,
    encoding_methods = encoding_method,
    replace = TRUE,
    verbose = FALSE
  )

  #list to store correlation data frames
  cor.list <- list()

  #correlation between numeric variables
  cor.list[["numerics"]] <- cor_numerics(
    df = df,
    predictors = predictors,
    cor_method = cor_method
  )

  #correlation between numeric and character variables
  cor.list[["num-char"]] <- cor_numerics_and_characters(
    df = df,
    predictors = predictors,
    cor_method = cor_method
  )

  #correlation between characters
  cor.list[["char-char"]] <- cor_characters(
    df = df,
    predictors = predictors
  )

  #join results
  cor.df <- cor.list |>
    dplyr::bind_rows() |>
    dplyr::filter(
      x != y
    ) |>
    dplyr::arrange(
      dplyr::desc(abs(correlation))
    ) |>
    dplyr::mutate(
      correlation = round(
        x = correlation,
        digits = 3
        )
    )

  cor.df

}




#' Correlation data frame between numeric variables
#'
#' @param df (required; data frame) A data frame Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. Default:'NULL'
#' @param cor_method (optional; charater string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#' @return data frame
#' @noRd
#' @keywords internal
#' @autoglobal
cor_numerics <- function(
    df = NULL,
    predictors = NULL,
    cor_method = "pearson"
){

  #method argument for stats::cor
  cor_method <- match.arg(
    arg = cor_method,
    choices = c(
      "pearson",
      "spearman"
    ),
    several.ok = FALSE
  )

  predictors.numeric <- identify_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(length(predictors.numeric) <= 1){
    return(NULL)
  }

  cor.numerics <- stats::cor(
    x = df[, predictors.numeric],
    use = "pairwise.complete.obs",
    method = cor_method
  ) |>
    #correlation matrix to data frame
    as.table() |>
    as.data.frame() |>
    dplyr::transmute(
      x = as.character(Var1),
      y = as.character(Var2),
      correlation = Freq
    ) |>
    #remove matrix diagonal
    dplyr::filter(
      x != y
    ) |>
    #remove mirrored pairs
    dplyr::rowwise() |>
    dplyr::mutate(
      pair_name = paste0(sort(c(x, y)), collapse = " ")
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct(
      pair_name,
      .keep_all = TRUE
    ) |>
    dplyr::select(-pair_name)

  cor.numerics

}


#' Correlation data frame between numeric and character variables
#'
#' @param df (required; data frame) A data frame Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. Default:'NULL'
#' @param cor_method (optional; charater string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#' @param encoding_method (optional; character string). Name of the target encoding method to convert character and factor predictors to numeric. One of "mean", "rank", "loo", "rnorm" (see [target_encoding_lab()] for further details). Default: "mean"
#'
#' @return data frame
#' @noRd
#' @keywords internal
#' @autoglobal
cor_numerics_and_characters <- function(
    df = NULL,
    predictors = NULL,
    cor_method = "pearson",
    encoding_method = "mean"
){

  #check input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )

  #check predictors
  predictors <- validate_predictors(
    df = df,
    response = response,
    predictors = predictors,
    min_numerics = 0
  )

  #method argument for stats::cor
  cor_method <- match.arg(
    arg = cor_method,
    choices = c(
      "pearson",
      "spearman"
    ),
    several.ok = FALSE
  )

  predictors.numeric <- identify_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(length(predictors.numeric) == 0){
    return(NULL)
  }

  predictors.character <- identify_non_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(length(predictors.character) == 0){
    return(NULL)
  }

  #data frame to store results
  r.num.char <- expand.grid(
    x = predictors.numeric,
    y = predictors.character,
    correlation = NA,
    stringsAsFactors = FALSE
  )

  #iterate to compute correlation
  for(i in seq_len(nrow(r.num.char))){

    #get response and predictor to a temp data frame
    df.i <- data.frame(
      x = df[[r.num.char$x[i]]],
      y = df[[r.num.char$y[i]]]
    ) |>
      na.omit()

    #target encode
    df.i <- target_encoding_lab(
      df = df.i,
      response = "x",
      predictors = "y",
      encoding_methods = encoding_method,
      replace = TRUE,
      verbose = FALSE
    )

    #compute correlation
    r.num.char$correlation[i] <- stats::cor(
      x = df.i$x,
      y = df.i$y,
      method = cor_method,
      use = "pairwise.complete.obs"
    )

  }

  r.num.char

}

#' Correlation data frame between character variables
#'
#' @param df (required; data frame) A data frame Default: NULL.
#' @param predictors (optional; character vector) character vector with predictor names in 'df'. Default:'NULL'
#'
#' @return data frame
#' @noRd
#' @keywords internal
#' @autoglobal
cor_characters <- function(
    df,
    predictors
){

  predictors.character <- identify_non_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(
    length(predictors.character) <= 1
  ){
    return(NULL)
  }

  #data frame to store results
  r.char.char <- expand.grid(
    x = predictors.character,
    y = predictors.character,
    correlation = NA,
    stringsAsFactors = FALSE
  )

  #iterate to compute correlation
  for(i in seq_len(nrow(r.char.char))){

    r.char.char$correlation[i] <- cramer_v(
      x = df[[r.char.char$x[i]]],
      y = df[[r.char.char$y[i]]],
      check_input = FALSE
    )

  }

  r.char.char

}
