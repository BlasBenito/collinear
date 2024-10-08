#' Pairwise Correlation Data Frame
#'
#' @description
#' Returns a correlation data frame between all pairs of predictors in a training data frame. If the argument  Non-numeric predictors are transformed into numeric via target encoding, using the 'response' variable as reference.
#'
#' Attempts to handle correlations between pairs of variables of different types, as follows:
#' \itemize{
#'   \item numeric vs. numeric: computed with stats::cor() with the methods "pearson" or "spearman" using [cor_numerics()].
#'   \item numeric vs. character via [cor_numerics_and_characters()], two alternatives leading to different results:
#'   \itemize{
#'     \item 'response' is provided: the character variable is target-encoded as numeric using the values of the response as reference, and then its correlation with the numeric variable is computed with stats::cor(). This option generates a response-specific result suitable for training statistical and machine-learning models
#'     \item 'response' is NULL (or the name of a non-numeric column): the character variable is target-encoded as numeric using the values of the numeric predictor (instead of the response) as reference, and then their correlation is computed with stats::cor(). This option leads to a response-agnostic result suitable for clustering problems.
#'   }
#'   \item character vs. character, via [cor_characters()], with two alternatives leading to different results:
#'   \itemize{
#'     \item 'response' is provided: the character variables are target-encoded as numeric using the values of the response as reference, and then their correlation is computed with stats::cor().
#'     \item response' is NULL (or the name of a non-numeric column): the association between the character variables is computed using Cramer's V. This option might be problematic, because R-squared values and Cramer's V, even when having the same range between 0 and 1, are not fully comparable.
#'   }
#' }
#'
#' @inheritParams collinear
#' @return data frame; variable pairs and their correlation
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
#' @family correlation
#' @author Blas M. Benito, PhD
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

  response <- validate_response(
    df = df,
    response = response
  )

  predictors <- validate_predictors(
    df = df,
    response = response,
    predictors = predictors
  )

  #early output if only one predictor
  if(length(predictors) == 1){
    return(
      data.frame(
        x = predictors,
        y = predictors,
        correlation = 1.0
      )
    )
  }

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
#' @inheritParams collinear
#' @return data frame
#' @rdname cor_df
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
#' @inheritParams collinear
#'
#' @return data frame
#' @rdname cor_df
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
    predictors = predictors
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
#' @inheritParams collinear
#'
#' @return data frame
#' @rdname cor_df
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
