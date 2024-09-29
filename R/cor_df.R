#' Pairwise Correlation Data Frame
#'
#' @description
#' Returns a correlation data frame between all pairs of predictors in a training data frame. If the argument  Non-numeric predictors are transformed into numeric via target encoding, using the 'response' variable as reference.
#'
#' Attempts to handle correlations between pairs of variables of different types, as follows:
#' \itemize{
#'   \item numeric vs. numeric: computed with stats::cor() with the methods "pearson" or "spearman" using [cor_numeric_vs_numeric()].
#'   \item numeric vs. character via [cor_numeric_vs_categorical()], two alternatives leading to different results:
#'   \itemize{
#'     \item 'response' is provided: the character variable is target-encoded as numeric using the values of the response as reference, and then its correlation with the numeric variable is computed with stats::cor(). This option generates a response-specific result suitable for training statistical and machine-learning models
#'     \item 'response' is NULL (or the name of a non-numeric column): the character variable is target-encoded as numeric using the values of the numeric predictor (instead of the response) as reference, and then their correlation is computed with stats::cor(). This option leads to a response-agnostic result suitable for clustering problems.
#'   }
#'   \item character vs. character, via [cor_categorical_vs_categorical()], with two alternatives leading to different results:
#'   \itemize{
#'     \item 'response' is provided: the character variables are target-encoded as numeric using the values of the response as reference, and then their correlation is computed with stats::cor().
#'     \item response' is NULL (or the name of a non-numeric column): the association between the character variables is computed using Cramer's V. This option might be problematic, because R-squared values and Cramer's V, even when having the same range between 0 and 1, are not fully comparable.
#'   }
#' }
#'
#' @inheritParams collinear
#' @return data frame; pairwise correlations
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

  #early output if only one predictor
  if(
    length(predictors) == 1 &&
    predictors %in% colnames(df)
  ){
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
  cor.list[["num-vs-num"]] <- cor_numeric_vs_numeric(
    df = df,
    predictors = predictors,
    cor_method = cor_method
  )

  #correlation between numeric and character variables
  cor.list[["num-vs-cat"]] <- cor_numeric_vs_categorical(
    df = df,
    predictors = predictors,
    cor_method = cor_method
  )

  #correlation between characters
  cor.list[["cat-vs-cat"]] <- cor_categorical_vs_categorical(
    df = df,
    predictors = predictors
  )

  #join results
  cor.df <- do.call(
    what = "rbind",
    args = cor.list
  )

  rownames(cor.df) <- NULL

  #arrange by absolute correlation values
  cor.df[
    order(
      abs(cor.df$correlation),
      decreasing = TRUE
    ),
  ]

}




#' Pairwise Correlation Between Numeric Variables
#'
#' @inheritParams collinear
#' @inherit cor_df return
#' @rdname cor_df
#' @keywords internal
#' @autoglobal
cor_numeric_vs_numeric <- function(
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
  #get numeric predictors only
  predictors <- validate_predictors(
    df = df,
    predictors = predictors
  )

  #identify numerics
  predictors <- identify_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(length(predictors) <= 1){
    return(NULL)
  }

  #correlation matrix to data frame
  cor.df <- stats::cor(
    x = df[, predictors],
    use = "pairwise.complete.obs",
    method = cor_method
  ) |>
    as.table() |>
    as.data.frame()

  #remove factors
  cor.df$Var1 <- as.character(cor.df$Var1)
  cor.df$Var2 <- as.character(cor.df$Var2)

  #rename columns
  colnames(cor.df) <- c(
    "x",
    "y",
    "correlation"
  )

  #filter out x == y
  cor.df <- cor.df[
    cor.df$x != cor.df$y,
  ]

  #identify pairs
  cor.df$pair_name <- apply(
    X = cor.df[, c("x", "y")],
    MARGIN = 1,
    FUN = function(x){
      paste0(
        sort(x),
        collapse = " "
      )
    }
  )

  #remove duplicated pairs
  cor.df <- cor.df[
    !duplicated(cor.df$pair_name),
  ]

  #remove pair name
  cor.df$pair_name <- NULL

  cor.df

}


#' Pairwise Correlation Between Numeric and Categorical Variables
#'
#' @inheritParams collinear
#'
#' @inherit cor_df return
#' @rdname cor_df
#' @keywords internal
#' @autoglobal
cor_numeric_vs_categorical <- function(
    df = NULL,
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

  #validate predictors without losing non-numerics
  #random response name to disable non-numeric filtering
  predictors <- validate_predictors(
    df = df,
    response = paste0("x", Sys.time()),
    predictors = predictors
  )

  #get numeric and character predictors
  predictors.numeric <- identify_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(length(predictors.numeric) == 0){
    return(NULL)
  }

  predictors.non.numeric <- identify_non_numeric_predictors(
    df = df,
    predictors = predictors
  )

  if(length(predictors.non.numeric) == 0){
    return(NULL)
  }

  #data frame to store results
  cor.df <- expand.grid(
    x = predictors.numeric,
    y = predictors.non.numeric,
    stringsAsFactors = FALSE
  )

  #progress bar
  p <- progressr::progressor(
    steps = nrow(cor.df)
  )

  #parallelized version
  cor.df$correlation <- future.apply::future_apply(
    X = cor.df,
    MARGIN = 1,
    FUN = function(x){

      p()

      df.x <- data.frame(
        x = df[[x[1]]],
        y = df[[x[2]]]
      ) |>
        na.omit()

      #target encode
      df.x <- target_encoding_lab(
        df = df.x,
        response = "x",
        predictors = "y",
        encoding_methods = encoding_method,
        replace = TRUE,
        verbose = FALSE
      )

      #compute correlation
      stats::cor(
        x = df.x$x,
        y = df.x$y,
        method = cor_method,
        use = "pairwise.complete.obs"
      )

    }
  )

  cor.df

}

#' Pairwise Cramer's V Between Categorical Variables
#'
#' @inheritParams collinear
#'
#' @inherit cor_df return
#' @rdname cor_df
#' @keywords internal
#' @autoglobal
cor_categorical_vs_categorical <- function(
    df,
    predictors
){

  #validate input data frame
  df <- validate_df(
    df = df,
    min_rows = 30
  )


  #validate predictors without losing non-numerics
  #random response name to disable non-numeric filtering
  predictors <- validate_predictors(
    df = df,
    response = paste0("x", Sys.time()),
    predictors = predictors
  )

  #get only non numeric predictors
  predictors <- identify_non_numeric_predictors(
    df = df,
    predictors = predictors
  )

  #empty output if no predictors
  if(length(predictors) <= 1){
    return(NULL)
  }

  #data frame to store results
  cor.df <- expand.grid(
    x = predictors,
    y = predictors,
    correlation = NA,
    stringsAsFactors = FALSE
  )

  #filter out x == y
  cor.df <- cor.df[
    cor.df$x != cor.df$y,
  ]

  #future apply
  #progress bar
  p <- progressr::progressor(
    steps = nrow(cor.df)
  )

  #parallelized version
  cor.df$correlation <- future.apply::future_apply(
    X = cor.df,
    MARGIN = 1,
    FUN = function(x){

      p()

      df.x <- data.frame(
        x = df[[x[1]]],
        y = df[[x[2]]]
      ) |>
        na.omit()

      cramer_v(
        x = df.x$x,
        y = df.x$y,
        check_input = FALSE
      )

    },
    future.seed = TRUE
  )

  cor.df

}
