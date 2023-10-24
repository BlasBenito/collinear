#' Target-encoding methods
#'
#' @description Methods to apply target-encoding to individual categorical variables. The functions implemented are:
#' \itemize{
#'   \item [target_encoding_mean()]: Each group is identified by the mean of the response over the group cases. White noise can be added via the `white_noise` argument. Columns encoded with this function are identified by the suffix "__encoded_mean". If `white_noise` is used, then the amount of white noise is also added to the suffix.
#'   \item [target_encoding_rank()]: Each group is identified by the rank of the mean of the response variable over the group cases. The group with the lower mean receives the rank 1. White noise can be added via the `white_noise` argument. Columns encoded with this function are identified by the suffix "__encoded_rank". If `white_noise` is used, then the amount of noise is also added to the suffix.
#'   \item [target_encoding_rnorm()]: Each case in a group receives a value coming from a normal distribution with the mean and the standard deviation of the response over the cases of the group. The argument `rnorm_sd_multiplier` multiplies the standard deviation to reduce the spread of the obtained values. Columns encoded with this function are identified by the suffix "__encoded_rnorm_rnorm_sd_multiplier_X", where X is the amount of `rnorm_sd_multiplier` used.
#'   \item [target_encoding_loo()]: The suffix "loo" stands for "leave-one-out". Each case in a group is encoded as the average of the response over the other cases of the group. Columns encoded with this function are identified by the suffix "__encoded_loo".
#' }
#'
#' @param df (required; data frame, tibble, or sf) A training data frame. Default: NULL
#' @param response (required; character string) Name of the response. Must be a column name of `df`. Default: NULL
#' @param predictor (required; character) Name of the categorical variable to encode.
#' @param white_noise (optional; numeric) Numeric with white noise values in the range 0-1, representing a fraction of the range of the response to be added as noise to the encoded variable. Controls the variability in the encoded variables to mitigate potential overfitting. Default: `0`.
#' @param rnorm_sd_multiplier (optional; numeric) Numeric with multiplier of the standard deviation of each group in the categorical variable, in the range 0-1. Controls the variability in the encoded variables to mitigate potential overfitting. Default: `1`
#' @param seed (optional; integer) Random seed to facilitate reproducibility. Default: `1`
#' @param replace (optional; logical) Advanced option that changes the behavior of the function. Use only if you really know exactly what you need. If `TRUE`, it replaces each categorical variable with its encoded version, and returns the input data frame with the replaced variables.
#' @param verbose (optional; logical) If TRUE, messages and plots generated during the execution of the function are displayed. Default: `TRUE`
#'
#'
#' @return The input data frame with a target-encoded variable.
#'
#' @examples
#'
#loading example data
#' data(vi)
#'
#' #subset to limit example run time
#' vi <- vi[1:1000, ]
#'
#' #mean encoding
#' #-------------
#'
#' #without noise
#' df <- target_encoding_mean(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type",
#'   replace = TRUE
#' )
#'
#' plot(
#'   x = df$soil_type,
#'   y = df$vi_mean,
#'   xlab = "encoded variable",
#'   ylab = "response"
#' )
#'
#' #with noise
#' df <- target_encoding_mean(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type",
#'   white_noise = 0.1,
#'   replace = TRUE
#' )
#'
#' plot(
#'   x = df$soil_type,
#'   y = df$vi_mean,
#'   xlab = "encoded variable",
#'   ylab = "response"
#' )
#'
#'
#' #group rank
#' #----------
#'
#' df <- target_encoding_rank(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type",
#'   replace = TRUE
#' )
#'
#' plot(
#'   x = df$soil_type,
#'   y = df$vi_mean,
#'   xlab = "encoded variable",
#'   ylab = "response"
#' )
#'
#'
#' #leave-one-out
#' #-------------
#'
#' #without noise
#' df <- target_encoding_loo(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type",
#'   replace = TRUE
#' )
#'
#' plot(
#'   x = df$soil_type,
#'   y = df$vi_mean,
#'   xlab = "encoded variable",
#'   ylab = "response"
#' )
#'
#' #with noise
#' df <- target_encoding_loo(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type",
#'   white_noise = 0.1,
#'   replace = TRUE
#' )
#'
#' plot(
#'   x = df$soil_type,
#'   y = df$vi_mean,
#'   xlab = "encoded variable",
#'   ylab = "response"
#' )
#'
#'
#' #rnorm
#' #-----
#'
#' #without sd multiplier
#' df <- target_encoding_rnorm(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type",
#'   replace = TRUE
#' )
#'
#' plot(
#'   x = df$soil_type,
#'   y = df$vi_mean,
#'   xlab = "encoded variable",
#'   ylab = "response"
#' )
#'
#' #with sd multiplier
#' df <- target_encoding_rnorm(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type",
#'   rnorm_sd_multiplier = 0.1,
#'   replace = TRUE
#' )
#'
#' plot(
#'   x = df$soil_type,
#'   y = df$vi_mean,
#'   xlab = "encoded variable",
#'   ylab = "response"
#' )
#'
#'
#' @export
#' @autoglobal
#' @author Blas M. Benito
#' @references
#' \itemize{
#'  \item Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality Categorical Attributes in Classification and Prediction Problems. SIGKDD Explor. Newsl. 3, 1, 27-32 \doi{10.1145/507533.507538}
#' }
#' @rdname target_encoding_methods
target_encoding_mean <- function(
    df,
    response,
    predictor,
    white_noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  #new variable name
  if(white_noise == 0){
    encoded.variable.name <- paste0(
      predictor,
      "__encoded_mean"
    )
  } else {
    encoded.variable.name <- paste0(
      predictor,
      "__encoded_mean__white_noise_",
      white_noise
    )
  }

  #mean encoding
  df[[encoded.variable.name]] <- stats::ave(
    x = df[[response]],
    df[[predictor]],
    FUN = function(x) mean(x, na.rm = TRUE)
  )

  #add white_noise if any
  df <- add_white_noise(
    df = df,
    response = response,
    predictor = encoded.variable.name,
    white_noise = white_noise,
    seed = seed
  )

  if(verbose == TRUE && replace == FALSE){
    message(
      "New encoded predictor: '",
      encoded.variable.name,
      "'"
    )
  }

  #replacing original variable with encoded version
  if(replace == TRUE){
    df[[predictor]] <- NULL
    colnames(df)[colnames(df) == encoded.variable.name] <- predictor
  }

  df

}


#' @rdname target_encoding_methods
#' @autoglobal
#' @export
target_encoding_rnorm <- function(
    df,
    response,
    predictor,
    rnorm_sd_multiplier = 1,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  if(length(rnorm_sd_multiplier) > 1){
    rnorm_sd_multiplier <- rnorm_sd_multiplier[1]
  }

  if(rnorm_sd_multiplier == 0){
    rnorm_sd_multiplier <- 0.0001
    encoded.variable.name <- paste0(
      predictor,
      "__encoded_rnorm"
    )
  } else {
    encoded.variable.name <- paste0(
      predictor,
      "__encoded_rnorm__sd_multiplier_",
      rnorm_sd_multiplier
    )
  }

  set.seed(seed)

  #target encoding
  df <- df |>
    dplyr::group_by(df[[predictor]]) |>
    dplyr::mutate(
      encoded = stats::rnorm(
        n = dplyr::n(),
        mean = mean(
          get(response),
          na.rm = TRUE
        ),
        sd = stats::sd(
          get(response),
          na.rm = TRUE
        ) * rnorm_sd_multiplier
      )
    ) |>
    dplyr::ungroup() |>
    suppressWarnings()

  #fill NAs produced in groups with one row
  df <- df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      encoded = ifelse(
        test = is.na(encoded),
        yes = stats::rnorm(
          n = 1,
          mean = get(response),
          sd = get(response)/10
        ),
        no = encoded
      )
    ) |>
    dplyr::ungroup()

  #rename encoded column
  names(df)[names(df) == "encoded"] <- encoded.variable.name

  if(verbose == TRUE && replace == FALSE){
    message(
      "New encoded predictor: '",
      encoded.variable.name,
      "'"
    )
  }

  #replacing original variable with encoded version
  if(replace == TRUE){
    df[[predictor]] <- NULL
    colnames(df)[colnames(df) == encoded.variable.name] <- predictor
  }

  df

}


#' @rdname target_encoding_methods
#' @autoglobal
#' @export
target_encoding_rank <- function(
    df,
    response,
    predictor,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  #new variable name
  encoded.variable.name <- paste0(
    predictor,
    "__encoded_rank"
  )

  #aggregate by groups
  df.map <- tapply(
    X = df[[response]],
    INDEX = df[[predictor]],
    FUN = mean,
    na.rm = TRUE
  ) |>
    sort()

  #add rank column
  df.map <- data.frame(
    names(df.map),
    seq_along(df.map) #rank column
  )
  names(df.map) <- c(predictor, encoded.variable.name)

  #merge
  df <- dplyr::inner_join(
    x = df,
    y = df.map,
    by = predictor
  )

  if(verbose == TRUE && replace == FALSE){
    message(
      "New encoded predictor: '",
      encoded.variable.name,
      "'"
    )
  }

  #replacing original variable with encoded version
  if(replace == TRUE){
    df[[predictor]] <- NULL
    colnames(df)[colnames(df) == encoded.variable.name] <- predictor
  }

  df

}

#' @rdname target_encoding_methods
#' @autoglobal
#' @export
target_encoding_loo <- function(
    df,
    response,
    predictor,
    white_noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  #new variable name
  if(white_noise == 0){
    encoded.variable.name <- paste0(
      predictor,
      "__encoded_loo"
    )
  } else {
    encoded.variable.name <- paste0(
      predictor,
      "__encoded_loo__white_noise_",
      white_noise
    )
  }

  #leave one out
  #by group, sum all cases of the response, subtract the value of the current row, and divide by n-1
  df <- df |>
    dplyr::group_by(df[[predictor]]) |>
    dplyr::mutate(
      encoded = (
        sum(
          get(response),
          na.rm = TRUE
        ) -
          get(response)
      ) /
        (dplyr::n() - 1)
    ) |>
    dplyr::ungroup()

  #rename encoded column
  names(df)[names(df) == "encoded"] <- encoded.variable.name

  #add white_noise if any
  df <- add_white_noise(
    df = df,
    response = response,
    predictor = encoded.variable.name,
    white_noise = white_noise,
    seed = seed
  )

  if(verbose == TRUE && replace == FALSE){
    message(
      "New encoded predictor: '",
      encoded.variable.name,
      "'"
    )
  }

  #replacing original variable with encoded version
  if(replace == TRUE){
    df[[predictor]] <- NULL
    colnames(df)[colnames(df) == encoded.variable.name] <- predictor
  }

  df

}

#' @rdname target_encoding_methods
#' @autoglobal
#' @export
add_white_noise <- function(
    df,
    response,
    predictor,
    white_noise = 0.1,
    seed = 1
){

  if(length(white_noise) > 1){
    white_noise <- white_noise[1]
  }

  if(white_noise < 0){
    white_noise <- 0
  }

  if(white_noise > 1){
    white_noise <- 1
  }

  if(white_noise == 0){
    return(df)
  }

  #internal function to rescale white_noise
  rescale_white_noise <- function(
    x = NULL,
    new_min = 0,
    new_max = 1
  ){

    #data extremes
    x.min <- min(x, na.rm = TRUE)
    x.max <- max(x, na.rm = TRUE)

    #scaling
    x <- ((x - x.min) / (x.max - x.min)) *
      (new_max - new_min) +
      new_min

    x

  }

  #mean difference between groups
  response.range <- max(
    df[[response]],
    na.rm = TRUE
    ) -
    min(
      df[[response]],
      na.rm = TRUE
    )

  #minimum and maximum white_noise
  max.white_noise <- response.range * white_noise
  min.white_noise <- -max.white_noise

  #reset random seed
  set.seed(seed)

  #generate white noise
  noise <- rescale_white_noise(
    x = stats::runif(
      n = nrow(df)
    ),
    new_min = min.white_noise,
    new_max = max.white_noise
  )



  #add white_noise to the given variable
  df[[predictor]] <- df[[predictor]] + noise

  #return df
  df

}
