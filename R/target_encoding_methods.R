#' Target-encoding methods
#'
#' @description Methods to apply target-encoding to individual categorical variables. The functions implemented are:
#' \itemize{
#'   \item `target_encoding_mean()`: Each group is identified by the mean of the response over the group cases. White noise can be added via the `noise` argument. Columns encoded with this function are identified by the suffix "__encoded_mean". If `noise` is used, then the amount of noise is also added to the suffix.
#'   \item `target_encoding_rank()`: Each group is identified by the rank of the mean of the response variable over the group cases. The group with the lower mean receives the rank 1. White noise can be added via the `noise` argument. Columns encoded with this function are identified by the suffix "__encoded_rank". If `noise` is used, then the amount of noise is also added to the suffix.
#'   \item `target_encoding_rnorm()`: Each case in a group receives a value coming from a normal distribution with the mean and the standard deviation of the response over the cases of the group. The argument `sd_width` multiplies the standard deviation to reduce the spread of the obtained values. Columns encoded with this function are identified by the suffix "__encoded_rnorm_sd_width_X", where X is the amount of `sd_width` used.
#'   \item `target_encoding_loo()`: The suffix "loo" stands for "leave-one-out". Each case in a group is encoded as the average of the response over the other cases of the group. olumns encoded with this function are identified by the suffix "__encoded_loo".
#' }
#'
#' @param df (required; data frame, tibble, or sf) A training data frame. Default: `NULL`
#' @param response (required; character string) Name of the response. Must be a column name of `df`. Default: `NULL`
#' @param predictor (required; character) Name of the categorical variable to encode.
#' @param noise (optional; numeric) Numeric with noise values in the range 0-1. Default: `0`.
#' @param sd_width (optional; numeric) Numeric with multiplicator of the standard deviation of each group in the categorical variable, in the range 0.01-1. Default: `0.1`
#' @param seed (optional; integer) Random seed to facilitate reproducibility. Default: `1`
#' @param replace (optional; logical) Advanced option that changes the behavior of the function. Use only if you really know exactly what you need. If `TRUE`, it replaces each categorical variable with its encoded version, and returns the input data frame with the replaced variables.
#' @param verbose (optional; logical) If TRUE, messages and plots generated during the execution of the function are displayed. Default: `TRUE`
#'
#'
#' @return The input data frame with a target-encoded variable.
#'
#' @examples
#' if(interactive()){
#'
#' #target_encoding mean
#' data(
#'   vi,
#'   "vi_mean",
#'   vi_predictors
#' )
#'
#'
#' #Group mean
#' #########################################################
#'
#' #transforming soil_type
#' vi <- target_encoding_mean(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type"
#' )
#'
#' #the encoded variable is named soil_type__encoded_mean
#'
#' #plotting it against the response
#' ggplot2::ggplot(data = vi) +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = soil_type__encoded_mean,
#'     color = soil_type
#'   ) +
#'   ggplot2::geom_point()
#'
#'
#' #Group rank
#' #########################################################
#'
#' #transforming soil_type
#' vi <- target_encoding_rank(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type"
#' )
#'
#' #the encoded variable is named soil_type__encoded_rank
#'
#' #plotting it against the response
#' ggplot2::ggplot(data = vi) +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = soil_type__encoded_rank,
#'     color = soil_type
#'   ) +
#'   ggplot2::geom_point()
#'
#'
#' #Leave-one-out
#' #########################################################
#' #transforming soil_type
#' vi <- target_encoding_loo(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type"
#' )
#'
#' #the encoded variable is named soil_type__encoded_loo
#'
#' #plotting it against the response
#' ggplot2::ggplot(data = vi) +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = soil_type__encoded_loo,
#'     color = soil_type
#'   ) +
#'   ggplot2::geom_point()
#'
#'
#' #rnorm
#' #########################################################
#'
#' #transforming soil_type
#' vi <- target_encoding_rnorm(
#'   df = vi,
#'   response = "vi_mean",
#'   predictor = "soil_type"
#' )
#'
#' #the encoded variable is named soil_type__encoded_rnorm_sd_width_0.1
#'
#' #plotting it against the response
#' ggplot2::ggplot(data = vi) +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = soil_type__encoded_rnorm_sd_width_0.1,
#'     color = soil_type
#'   ) +
#'   ggplot2::geom_point()
#'
#'
#' }
#'
#' @export
#' @autoglobal
#' @rdname target_encoding_methods
target_encoding_mean <- function(
    df,
    response,
    predictor,
    noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  #new variable name
  if(noise == 0){
    categorical.variable.new.name <- paste0(
      predictor,
      "__encoded_mean"
    )
  } else {
    categorical.variable.new.name <- paste0(
      predictor,
      "__encoded_mean_",
      "noise_",
      noise
    )
  }

  #mean encoding
  df[[categorical.variable.new.name]] <- stats::ave(
    x = df[[response]],
    df[[predictor]],
    FUN = function(x) mean(x, na.rm = TRUE)
  )

  #add noise if any
  df <- target_encoding_noise(
    df = df,
    predictor = categorical.variable.new.name,
    noise = noise,
    seed = seed
  )

  if(verbose == TRUE && replace == FALSE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  #replacing original variable with encoded version
  if(replace == TRUE){
    df[[predictor]] <- NULL
    colnames(df)[colnames(df) == categorical.variable.new.name] <- predictor
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
    sd_width = 0.1,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  if(sd_width <= 0){
    sd_width <- 0.0001
  }
  if(sd_width > 1){
    sd_width <- 1
  }


  categorical.variable.new.name <- paste0(
    predictor,
    "__encoded_rnorm_",
    "sd_width_",
    sd_width
  )

  set.seed(seed)

  #target encoding
  df <- df |>
    dplyr::group_by(.data[[predictor]]) |>
    dplyr::mutate(
      !!categorical.variable.new.name := stats::rnorm(
        n = dplyr::n(),
        mean = mean(
          get(response),
          na.rm = TRUE
        ),
        sd = stats::sd(
          get(response),
          na.rm = TRUE
        ) * sd_width
      )
    ) |>
    dplyr::ungroup()

  if(verbose == TRUE && replace == FALSE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  #replacing original variable with encoded version
  if(replace == TRUE){
    df[[predictor]] <- NULL
    colnames(df)[colnames(df) == categorical.variable.new.name] <- predictor
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
    noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  #new variable name
  if(noise == 0){
    categorical.variable.new.name <- paste0(
      predictor,
      "__encoded_rank"
    )
  } else {
    categorical.variable.new.name <- paste0(
      predictor,
      "__encoded_rank_",
      "noise_",
      noise
    )
  }

  #aggregate by groups
  df.map <- tapply(
    X = df[[response]],
    INDEX = df[[predictor]],
    FUN = mean,
    na.rm = TRUE
  ) |>
    sort()

  df.map <- data.frame(
    names(df.map),
    1:length(df.map)
  )
  names(df.map) <- c(predictor, categorical.variable.new.name)

  #merge
  df <- dplyr::inner_join(
    x = df,
    y = df.map,
    by = predictor
  )

  #add noise if any
  df <- target_encoding_noise(
    df = df,
    predictor = categorical.variable.new.name,
    noise = noise,
    seed = seed
  )

  if(verbose == TRUE && replace == FALSE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  #replacing original variable with encoded version
  if(replace == TRUE){
    df[[predictor]] <- NULL
    colnames(df)[colnames(df) == categorical.variable.new.name] <- predictor
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
    replace = FALSE,
    verbose = TRUE
){

  #new variable name
  categorical.variable.new.name <- paste0(
    predictor,
    "__encoded_loo"
  )

  #leave one out
  #by group, sum all cases of the response, subtract the value of the current row, and divide by n-1
  df <- df |>
    dplyr::group_by(.data[[predictor]]) |>
    dplyr::mutate(
      !!categorical.variable.new.name := (
        sum(
          get(response),
          na.rm = TRUE
        ) -
          get(response)
      ) /
        (dplyr::n() - 1)
    ) |>
    dplyr::ungroup()

  if(verbose == TRUE && replace == FALSE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  #replacing original variable with encoded version
  if(replace == TRUE){
    df[[predictor]] <- NULL
    colnames(df)[colnames(df) == categorical.variable.new.name] <- predictor
  }

  df

}

#' @rdname target_encoding_methods
#' @autoglobal
#' @export
target_encoding_noise <- function(
    df,
    predictor,
    noise = 0,
    seed = 1
){

  #internal function to rescale noise
  rescale_noise <- function(
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

  if(noise < 0){
    noise <- 0
  }

  if(noise == 0){
    return(df)
  }

  if(noise > 1){
    noise <- 1
  }

  #mean difference between groups
  between.group.difference <- df[[predictor]] |>
    sort() |>
    unique() |>
    diff() |>
    mean()

  #minimum noise
  min.noise <- 0
  max.noise <- between.group.difference * noise

  #if noise is too small
  # if(min.noise == max.noise){
  #
  #   #increase noise until min and max noise are different
  #   while(max.noise == min.noise){
  #
  #     noise <- noise + 0.01
  #
  #     max.noise <- between.group.difference * noise
  #
  #   }
  #
  # }

  #reset random seed
  set.seed(seed)

  #add noise to the given variable
  df[[predictor]] <- df[[predictor]] +
    stats::rnorm(
      n = nrow(df)
    ) |>
    abs() |>
    rescale_noise(
      new_min = min.noise,
      new_max = max.noise
      )

  #return df
  df

}
