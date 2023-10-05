#' @title Automated multicollinearity reduction via Variance Inflation Factor
#'
#' @description
#'
#' Warning: input predictors with a pairwise correlation very close to 1 might create issues during VIF computation. If that's the case, please apply [mc_auto_cor()] before.
#'
#' Warning: near-zero variance columns are ignored by this function.
#'
#' The Variance Inflation Factor for a given variable `y` is computed as `1/(1-R2)`, where `R2` is the multiple R-squared of a multiple regression model fitted using `y` as response and all the remaining variables of the input data set as predictors. The equation can be interpreted as "the rate of perfect model's R-squared to the unexplained variance of this model".
#'
#' The possible range of VIF values is (1, Inf]. A VIF lower than 10 suggest that removing `y` from the data set would reduce overall multicollinearity. The recommended thresholds for maximum VIF may vary depending on the source consulted, being the most common values, 2.5, 5, and 10.
#'
#' The function `mc_auto_vif()` applies a recursive algorithm to remove variables with a VIF higher than a given threshold (defined by the argument `max.vif`).
#'
#' The function allows the user to define a preference selection order via the argument `preference.order`. This argument helps "protect" variables that might be interesting or even required for the given analysis. Please, see the examples to understand better how this feature works.
#'
#'
#' @param data (required; data.frame or tibble) A data frame, tibble, or sf. Default: `NULL`.
#' @param predictors (optional; character vector) Character vector with the names of the predictive variables. Every element of this vector must be in the column names of `data`. Default: `NULL`
#' @param preference.order (optional, character vector) Character vector indicating the preference order to protect variables from elimination.  Predictors not included in this argument are ranked by their VIFs. Default: `NULL`.
#' @param max.vif (optional, numeric) Numeric with recommended values between 2.5 and 10 defining the maximum VIF allowed in the output dataset. Higher VIF thresholds should result in a higher number of selected variables. Default: `5`.
#' @param verbose (optional, logical) Set to `FALSE` to silence messages. Default: `TRUE`
#' @return Character vector with the names of uncorrelated predictors.
#' @seealso [mc_auto_cor()]
#' @examples
#' if(interactive()){
#'
#' data(
#'   ecoregions,
#'   ecoregions_continuous_response,
#'   ecoregions_predictors,
#'   ecoregions_all_predictors
#' )
#'
#' #NUMERIC PREDICTORS ONLY
#' ################################
#'
#'
#' #automatic VIF reduction with low max.vif
#' #-------------------------------
#' selected.predictors <- mc_auto_vif(
#'   data = ecoregions,
#'   predictors = ecoregions_predictors,
#'   max.vif = 2
#' )
#'
#' #check vif of the resulting dataset
#' #variance inflation factor
#' vif(data = ecoregions[, selected.predictors])
#'
#' #a small number of predictors is selected
#' length(selected.predictors)
#'
#'
#' #automatic VIF reduction with high max.vif
#' #-------------------------------
#' selected.predictors <- mc_auto_vif(
#'   data = ecoregions,
#'   predictors = ecoregions_predictors,
#'   max.vif = 10
#' )
#'
#' #a larger number of predictors is selected
#' length(selected.predictors)
#'
#'
#' #supervised VIF reduction
#' #------------------------------------------------
#' #1. Notice that "fragmentation_ca" is selected
#' #even though it has the maximum VIF in the dataset
#' #2. "climate_bio12_average" is excluded from the result
#' #because it's highly correlated to "climate_bio1_average"
#' selected.predictors <- mc_auto_vif(
#'   data = ecoregions,
#'   predictors = ecoregions_predictors,
#'   preference.order = c(
#'     "fragmentation_ca",
#'     "climate_bio1_average",
#'     "climate_bio12_average"
#'   )
#' )
#'
#'
#' #using quantitative criteria as preference.order
#' #-----------------------------------------------
#' #computing univariate correlation between each predictor and the response
#' preference.order <- apply(
#'   X = ecoregions[, ecoregions_predictors],
#'   MARGIN = 2,
#'   FUN = function(x){
#'     cor(
#'       x = x,
#'       y = ecoregions[, ecoregions_continuous_response]
#'       )
#'     }
#'   )
#'
#' #sorting from maximum to minimum and getting names only
#' preference.order <- names(
#'   sort(
#'     preference.order,
#'     decreasing = TRUE
#'     )
#'   )
#'
#' #using it in mc_auto_vif as preference.order
#' selected.predictors <- mc_auto_vif(
#'   data = ecoregions,
#'   predictors = ecoregions_predictors,
#'   preference.order = preference.order
#' )
#'
#'
#' #using mc_auto_vif and mc_auto_cor together
#' #-----------------------------------------------
#' mc_auto_cor.result <- mc_auto_cor(
#'   data = ecoregions,
#'   predictors = ecoregions_predictors,
#'   preference.order = preference.order,
#'   max.cor = 0.75
#' )
#'
#' selected.predictors <- mc_auto_vif(
#'   data = ecoregions,
#'   predictors = mc_auto_cor.result,
#'   preference.order = preference.order
#' )
#'
#' #NUMERIC AND CATEGORICAL PREDICTORS
#' #-----------------------------------
#'
#' #there are two categorial predictors in the dataset below
#' class(ecoregions$dominant_landcover)
#' class(ecoregions$primary_productivity)
#'
#' #these variables are included in the vector ecoregions_all_predictors
#' ecoregions_all_predictors[45:46]
#'
#' #automatic VIF reduction using target-encoding for categorical predictors
#' #---------------------------------------------------
#' #requires the name of the response to compute the target-encoding
#' selected.predictors <- mc_auto_vif(
#'   data = ecoregions,
#'   predictors = ecoregions_all_predictors
#' )
#'
#' #both categorical predictors have been selected
#' ecoregions_all_predictors[45:46] %in% selected.predictors
#'
#'
#' #applying target-encoding before vif
#' #---------------------------------------------------
#' data.encoded <- fe_target_encoding(
#'   data = ecoregions,
#'   predictors = ecoregions_all_predictors,
#'   methods = "mean",
#'   replace = TRUE
#' )
#'
#' #now the function does not require the response's name
#' selected.predictors <- mc_auto_vif(
#'   data = data.encoded,
#'   predictors = ecoregions_all_predictors
#' )
#'
#' #both categorical predictors have been selected again
#' ecoregions_all_predictors[45:46] %in% selected.predictors
#'
#'
#' }
#' @rdname mc_auto_vif
#' @autoglobal
#' @export
mc_auto_vif <- function(
    data = NULL,
    predictors = NULL,
    preference.order = NULL,
    max.vif = 5,
    verbose = TRUE
){

  if(is.null(data)){
    stop("Argument 'data' is required.")
  }

  if((is.null(max.vif)) | (max.vif < 0)){
    stop("Argument 'max.vif' must be in the range (0, Inf).")
  }

  #internal vif function
  .vif <- function(data){

    vif <- NULL

    out <- data.frame(
      diag(
        solve(
          cor(
            x = df,
            use = "complete.obs"
          ),
          tol = 0
        )
      ),
      stringsAsFactors = FALSE
    ) |>
      dplyr::rename(vif = 1) |>
      tibble::rownames_to_column(var = "variable") |>
      dplyr::mutate(vif = round(vif, 3)) |>
      dplyr::arrange(vif)

    out

  }

  df <- df[, predictors]

  #check cor
  df.cor <- cor_df(
    df = df,
    predictors = predictors
  )

  if(max(data.cor$cor) >= 0.99){
    stop("The maximum correlation between a pair of predictors is > 0.99. The VIF computation will fail. Please use the output of 'mc_cor_auto()' as input for the argument 'predictors'.")
  }

  #auto preference order by vif
  preference.order.auto <- .vif(df)[["variable"]]

  #if preference.order is not provided, use auto
  if(is.null(preference.order)){

    preference.order <- preference.order.auto

  } else {

    #subset preference.order to colnames(x)
    preference.order <- preference.order[preference.order %in% colnames(df)]

    #if there are variables not in preference.order, add them in the order of preference.order.auto
    if(length(preference.order) < ncol(df)){

      not.in.preference.order <- colnames(df)[!(colnames(df) %in% preference.order)]
      preference.order <- c(preference.order, preference.order.auto[preference.order.auto %in% not.in.preference.order])

    }

  }

  #order x according to preference order
  df <- df[, preference.order]

  #rank of interest
  df.rank <- data.frame(
    variable = colnames(df),
    rank = 1:ncol(df)
  )

  #iterating through reversed preference order
  for(i in seq(from = nrow(df.rank), to = 2)){

    vif.i <- .vif(df = df[, df.rank$variable]) |>
      dplyr::filter(
        variable == df.rank[i, "variable"]
      ) |>
      dplyr::pull(vif)

    #removing var if vif is above threshold
    if(vif.i > max.vif){

      #removing it from x.rank
      df.rank <- dplyr::filter(
        df.rank,
        variable != df.rank[i, "variable"]
      )

    }

  }

  removed.vars <- setdiff(
    x = colnames(df),
    y = df.rank$variable
  )

  #message
  if(verbose == TRUE){
    if(length(removed.vars) == 0){
      message("Variables are not collinear, nothing to do here.")
    }
  }

  #selected variables
  selected.variables <- preference.order[preference.order %in% setdiff(colnames(df), removed.vars)]

  if(verbose == TRUE){
    message(
      paste0(
        "\nThe selected variables are:\n",
        paste(selected.variables, collapse = "\n")
      )
    )
  }


  selected.variables

}




