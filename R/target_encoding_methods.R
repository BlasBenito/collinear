#' Target Encoding Methods
#'
#' @description Methods to apply target-encoding to transform categorical variables into numeric.
#' The functions implemented are:
#' \itemize{
#'   \item [target_encoding_mean()]: Each category is identified by the mean of the response over the category cases. The argument `smoothing` controls pushes the mean of small groups towards the global mean to avoid overfitting. White noise can be added via the `white_noise` argument. Columns encoded with this function are identified by the suffix "__encoded_mean". If `white_noise` is used, then the amount of white noise is also added to the suffix.
#'   \item [target_encoding_rank()]: Each category is identified by the rank of the mean of the response variable over the category cases. The category with the lower mean receives the rank 1. White noise can be added via the `white_noise` argument. Columns encoded with this function are identified by the suffix "__encoded_rank". If `white_noise` is used, then the amount of noise is also added to the suffix.
#'   \item [target_encoding_rnorm()]: Each case in a category receives a value coming from a normal distribution with the mean and the standard deviation of the response over the cases of the category. The argument `rnorm_sd_multiplier` multiplies the standard deviation to reduce the spread of the obtained values. Columns encoded with this function are identified by the suffix "__encoded_rnorm_rnorm_sd_multiplier_X", where X is the amount of `rnorm_sd_multiplier` used.
#'   \item [target_encoding_loo()]: The suffix "loo" stands for "leave-one-out". Each case in a category is encoded as the average of the response over the other cases of the category Columns encoded with this function are identified by the suffix "__encoded_loo".
#' }
#'
#' @inheritParams target_encoding_lab
#' @param predictor (required; character) Name of the categorical variable to encode. Default: NULL
#'
#'
#' @inherit target_encoding_lab return

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
#' @author Blas M. Benito, PhD
#' @references
#' \itemize{
#'  \item Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality Categorical Attributes in Classification and Prediction Problems. SIGKDD Explor. Newsl. 3, 1, 27-32 \doi{10.1145/507533.507538}
#' }
#' @rdname target_encoding_methods
#' @family target_encoding
#' @autoglobal
#' @export
target_encoding_mean <- function(
    df,
    response,
    predictor,
    smoothing = 0,
    white_noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  if(length(white_noise) > 1){
    white_noise <- white_noise[1]
  }

  if(length(smoothing) > 1){
    smoothing <- smoothing[1]
  }

  if(smoothing < 0){
    smoothing <- 0
  }

  if(white_noise == 0){
    name.noise <- ""
  } else {
    name.noise <- paste0("__noise_", white_noise)
  }

  if(smoothing == 0){
    name.smoothing <- ""
  } else {
    name.smoothing <- paste0("__smoothing_", smoothing)
  }

  encoded.variable.name <- paste0(
    predictor,
    "__encoded_mean",
    name.smoothing,
    name.noise
  )

  #checking smoothing parameter
  if(smoothing < 0 ){
    smoothing <- 0
  }
  if(smoothing > nrow(df)){
    smoothing <- nrow(df)
  }

  #global response mean
  global_response_mean <- mean(df[[response]], na.rm = TRUE)

  #mean encoding when smoothing > 0
  if(smoothing == 0){

    df[[encoded.variable.name]] <- stats::ave(
      x = df[[response]],
      df[[predictor]],
      FUN = function(x) mean(x, na.rm = TRUE)
    )

  } else {

    df[[encoded.variable.name]] <- stats::ave(
      x = df[[response]],
      df[[predictor]],
      FUN = function(x) {
        n <- length(x)
        mean_x <- mean(x, na.rm = TRUE)
        (n * mean_x + smoothing * global_response_mean) / (n + smoothing)
      }
    )

  }


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
    rnorm_sd_multiplier <- 0.01
    name.multiplier <- ""
  } else {
    name.multiplier <- paste0("__sd_multiplier_", rnorm_sd_multiplier)
  }

  encoded.variable.name <- paste0(
    predictor,
    "__encoded_rnorm",
    name.multiplier
  )

  set.seed(seed)

  #target encoding
  df <- df |>
    dplyr::group_by_at(predictor) |>
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
    white_noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  if(length(white_noise) > 1){
    white_noise <- white_noise[1]
  }

  if(white_noise == 0){
    name.noise <- ""
  } else {
    name.noise <- paste0("__noise_", white_noise)
  }

  encoded.variable.name <- paste0(
    predictor,
    "__encoded_rank",
    name.noise
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
target_encoding_loo <- function(
    df,
    response,
    predictor,
    white_noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  if(length(white_noise) > 1){
    white_noise <- white_noise[1]
  }

  if(white_noise == 0){
    name.noise <- ""
  } else {
    name.noise <- paste0("__noise_", white_noise)
  }

  encoded.variable.name <- paste0(
    predictor,
    "__encoded_loo",
    name.noise
  )

  #leave one out
  #by group
  #sum all cases of the response
  #subtract the value of the current row
  #divide by n-1
  df <- df |>
    dplyr::group_by_at(predictor) |>
    dplyr::mutate(
      encoded =
        (sum(get(response), na.rm = TRUE) - get(response)) /
        (dplyr::n() - 1),
    ) |>
    dplyr::ungroup()

  #fill groups with NaN or NA with the global mean
  df[is.na(df$encoded), "encoded"] <- mean(df[[response]], na.rm = TRUE)
  df[is.nan(df$encoded), "encoded"] <- mean(df[[response]], na.rm = TRUE)

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
