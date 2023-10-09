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
#' @param df (required; data frame or tibble) A data frame with numeric and/or character predictors predictors, and optionally, a response variable. Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. If omitted, all columns of 'df' are used as predictors. Default:'NULL'
#' @param method (optional; character string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#'
#' @return Data frame with pairs of predictors and their correlation.
#'
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions,
#'   ecoregions_predictors
#' )
#'
#' df <- cor_df(
#'       df = ecoregions,
#'       predictors = ecoregions_predictors
#'   )
#'
#' }
#' @autoglobal
#' @export
cor_df <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    method = "pearson"
){

  #for development only
  # df <- ecoregions
  # response <- "plant_richness"
  # predictors <- ecoregions_predictors
  # method = "pearson"


  #method argument for stats::cor
  method <- match.arg(
    arg = method,
    choices = c(
      "pearson",
      "spearman"
    ),
    several.ok = FALSE
  )

  #check input data frame
  df <- df_inspect(
    df = df,
    min_rows = ifelse(
      test = method == "pearson",
      yes = 30,
      no = 10
    )
  )

  #check predictors
  predictors <- predictors_inspect(
    df = df,
    predictors = predictors,
    min_numerics = 0
  )

  #check response
  response <- response_inspect(
    df = df,
    response = response
  )

  #factors, logical, and ordered to characters
  df <- rapply(
    object = df[, c(response, predictors)],
    f = as.character,
    classes = c(
      "factor",
      "ordered",
      "logical"
    ),
    how = "replace"
  )

  #target encode character predictors
  df <- target_encode(
    df = df,
    response = response,
    predictors = predictors
  )

  #list to store correlation data frames
  cor.list <- list()

  #correlation between numeric variables
  cor.list[["numerics"]] <- cor_numerics(
    df = df,
    predictors = predictors,
    method = method
  )

  #correlation between numeric and character variables
  cor.list[["num-char"]] <- cor_numerics_and_characters(
    df = df,
    predictors = predictors,
    method = method
  )

  #correlation between characters
  cor.list[["char-char"]] <- cor_characters(
    df = df,
    predictors = predictors
  )

  #join results
  cor.df <- cor.list |>
    dplyr::bind_rows() |>
    dplyr::arrange(
      dplyr::desc(r_squared)
    ) |>
    dplyr::mutate(
      r_squared = round(
        x = r_squared,
        digits = 3
        )
    )

  cor.df

}


#' Target encoding of 'df' argument
#'
#' @param df (required; data frame or tibble) A data frame Default: NULL.
#' @param response (recommended, character string) Name of a numeric response variable. Character response variables are ignored. Please, see 'Details' to better understand how providing this argument or not leads to different results when there are character variables in 'predictors'. Default: NULL.
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. If ommited, all columns of 'df' are used as predictors. Default:'NULL'
#'
#' @return data frame
#' @noRd
#' @keywords internal
#' @autoglobal
target_encode <- function(
    df = NULL,
    response = NULL,
    predictors = NULL
){

  predictors.character <- predictors_character(
    df = df,
    predictors = predictors
  )

  if(
    is.null(response) |
    length(predictors.character) == 0
  ){
    return(df)
  }

  df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(predictors.character),
        ~target_encoder(
          x = df[[response]],
          y = .,
          check_input = FALSE
        )
      )
    )

  df

}


#' Correlation data frame between numeric variables
#'
#' @param df (required; data frame or tibble) A data frame Default: NULL.
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. Default:'NULL'
#' @param method (optional; charater string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#' @return data frame
#' @noRd
#' @keywords internal
#' @autoglobal
cor_numerics <- function(
    df = NULL,
    predictors = NULL,
    method = "pearson"
){

  #method argument for stats::cor
  method <- match.arg(
    arg = method,
    choices = c(
      "pearson",
      "spearman"
    ),
    several.ok = FALSE
  )

  predictors.numeric <- predictors_numeric(
    df = df[, predictors]
  )

  if(length(predictors.numeric) == 0){
    return(NULL)
  }

  cor.numerics <- stats::cor(
    x = df[, predictors.numeric],
    use = "pairwise.complete.obs",
    method = method
  ) |>
    #correlation matrix to data frame
    as.table() |>
    as.data.frame() |>
    dplyr::transmute(
      x = as.character(Var1),
      y = as.character(Var2),
      r_squared = Freq^2
    ) |>
    #remove matrix diagonal
    dplyr::filter(
      r_squared < 1
    ) |>
    #unique pair names to remove mirrored pairs
    dplyr::rowwise() |>
    dplyr::mutate(
      pair_name = paste0(sort(c(x, y)), collapse = " ")
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct(
      pair_name,
      .keep_all = TRUE
    ) |>
    dplyr::select(-pair_name) |>
    dplyr::arrange(
      dplyr::desc(r_squared)
    )

  cor.numerics

}


#' Correlation data frame between numeric and character variables
#'
#' @param df (required; data frame or tibble) A data frame Default: NULL.
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. Default:'NULL'
#' @param method (optional; charater string) Method used to compute pairwise correlations. Accepted methods are "pearson" (with a recommended minimum of 30 rows in 'df') or "spearman" (with a recommended minimum of 10 rows in 'df'). Default: "pearson".
#'
#' @return data frame
#' @noRd
#' @keywords internal
#' @autoglobal
cor_numerics_and_characters <- function(
    df = NULL,
    predictors = NULL,
    method = "pearson"
){

  #method argument for stats::cor
  method <- match.arg(
    arg = method,
    choices = c(
      "pearson",
      "spearman"
    ),
    several.ok = FALSE
  )

  predictors.numeric <- predictors_numeric(
    df = df,
    predictors = predictors
  )

  predictors.character <- predictors_character(
    df = df,
    predictors = predictors
  )

  if(
    length(predictors.numeric) == 0 |
    length(predictors.character) == 0
  ){
    return(NULL)
  }

  #data frame to store results
  r.num.char <- expand.grid(
    x = predictors.numeric,
    y = predictors.character,
    r_squared = NA,
    stringsAsFactors = FALSE
  )

  #iterate to compute r_squared
  for(i in seq_len(nrow(r.num.char))){

    #get variable for target encoding
    x <- df[[r.num.char$x[i]]]

    #apply target-encoding against the given predictor
    y <- target_encoder(
      x = x,
      y = df[[r.num.char$y[i]]],
      check_input = FALSE
    )

    #compute correlation
    r.num.char$r_squared[i] <- stats::cor(
      x = x,
      y = y,
      method = method
    )^2

  }

  r.num.char <- r.num.char |>
    dplyr::arrange(
      dplyr::desc(r_squared)
    )

  r.num.char

}

#' Correlation data frame between character variables
#'
#' @param df (required; data frame or tibble) A data frame Default: NULL.
#' @param predictors (optional; character vector) Character vector with column names of predictors in 'df'. Default:'NULL'
#'
#' @return data frame
#' @noRd
#' @keywords internal
#' @autoglobal
cor_characters <- function(
    df,
    predictors
){

  predictors.character <- predictors_character(
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
    x = predictors.character[1],
    y = predictors.character[2:length(predictors.character)],
    r_squared = NA,
    stringsAsFactors = FALSE
  )

  #iterate to compute r_squared
  for(i in seq_len(nrow(r.char.char))){

    r.char.char$r_squared[i] <- cramer_v(
      x = df[[r.char.char$x[i]]],
      y = df[[r.char.char$y[i]]],
      check_input = FALSE
    )

  }

  r.char.char <- r.char.char |>
    dplyr::arrange(
      dplyr::desc(r_squared)
    )

  r.char.char

}