#' Pairwise Correlation Data Frame
#'
#' @description
#' Returns a pairwise correlation data frame for different types of variables:
#' \itemize{
#'   \item **numeric vs. numeric**: as computed with [stats::cor()] using the methods "pearson" or "spearman", via [cor_numeric_vs_numeric()].
#'   \item **numeric vs. categorical**: the function [cor_numeric_vs_categorical()] target-encodes the categorical variable using the numeric variable as reference with [target_encoding_lab()] and the method "mean", and then their correlation is computed with [stats::cor()].
#'   \item **categorical vs. categorical**: the function [cor_categorical_vs_categorical()] computes Cramer's V (see [cramer_v()]) as indicator of the association between character or factor variables. However, take in mind that Cramer's V is not directly comparable with R-squared, even when having the same range from zero to one. It is always recommended to target-encode categorical variables with [target_encoding_lab()] before the pairwise correlation analysis.
#'   }
#'
#' @inheritParams collinear
#' @return data frame; pairwise correlation
#'
#' @examples
#' data(
#'   vi,
#'   vi_predictors
#' )
#'
#' #reduce size of vi to speed-up example execution
#' vi <- vi[1:1000, ]
#'
#' #mixed predictors
#' vi_predictors <- vi_predictors[1:10]
#'
#' #parallelization setup
#' future::plan(
#'   future::multisession,
#'   workers = 2 #set to parallelly::availableCores() - 1
#' )
#'
#' #progress bar
#' # progressr::handlers(global = TRUE)
#'
#' #correlation data frame
#' df <- cor_df(
#'   df = vi,
#'   predictors = vi_predictors
#' )
#'
#' df
#'
#' #disable parallelization
#' future::plan(future::sequential)
#'
#' @autoglobal
#' @family pairwise_correlation
#' @export
cor_df <- function(
    df = NULL,
    predictors = NULL,
    cor_method = "pearson"
){

  #validate input data frame
  df <- validate_df(
    df = df
  )

  predictors <- validate_predictors(
    df = df,
    predictors = predictors
  )

  predictors <- validate_data_cor(
    df = df,
    predictors = predictors,
    function_name = "collinear::cor_df()"
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

  #arrange by absolute correlation values
  cor.df <- cor.df[
    order(
      abs(cor.df$correlation),
      decreasing = TRUE
    ),
    , drop = FALSE
  ]


  rownames(cor.df) <- NULL

  cor.df

}




#' Pairwise Correlation Between Numeric Variables
#'
#' @inheritParams collinear
#' @inherit cor_df return
#' @rdname cor_df
#' @family pairwise_correlation
#' @autoglobal
cor_numeric_vs_numeric <- function(
    df = NULL,
    predictors = NULL,
    cor_method = "pearson"
){

  #validate input data frame
  df <- validate_df(
    df = df
  )

  #validate predictors
  #get numeric predictors only
  predictors <- validate_predictors(
    df = df,
    predictors = predictors
  )

  #identify numerics
  predictors <- identify_predictors_numeric(
    df = df,
    predictors = predictors
  )

  if(length(predictors) <= 1){
    return(NULL)
  }

  #correlation matrix to data frame
  cor.df <- stats::cor(
    x = df[, predictors, drop = FALSE],
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
    , drop = FALSE
  ]

  #identify pairs
  cor.df$pair_name <- apply(
    X = cor.df[, c("x", "y"), drop = FALSE],
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
    , drop = FALSE
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
#' @family pairwise_correlation
#' @autoglobal
cor_numeric_vs_categorical <- function(
    df = NULL,
    predictors = NULL,
    cor_method = "pearson"
){

  #validate input data frame
  df <- validate_df(
    df = df
  )

  #validate predictors without losing non-numerics
  #random response name to disable non-numeric filtering
  predictors <- validate_predictors(
    df = df,
    response = paste0("x", Sys.time()),
    predictors = predictors
  )

  #identify numeric and categorical predictors
  predictors <- identify_predictors(
    df = df,
    predictors = predictors
  )

  if(
    any(
      length(predictors$numeric) == 0,
      length(predictors$categorical) == 0
    )
  ){
    return(NULL)
  }

  #data frame to store results
  cor.df <- expand.grid(
    x = predictors$numeric,
    y = predictors$categorical,
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
        encoding_methods = "mean",
        replace = TRUE,
        quiet = TRUE
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
#' @family pairwise_correlation
#' @autoglobal
cor_categorical_vs_categorical <- function(
    df,
    predictors
){

  #validate input data frame
  df <- validate_df(
    df = df
  )

  #validate predictors without losing non-numerics
  #random response name to disable non-numeric filtering
  predictors <- validate_predictors(
    df = df,
    response = paste0("x", Sys.time()),
    predictors = predictors
  )

  #get only non numeric predictors
  predictors <- identify_predictors_categorical(
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
    , drop = FALSE
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
