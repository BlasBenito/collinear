#' Target-encoding methods
#'
#' @description Methods to apply target-encoding to individual categorical variables. The functions implemented are:
#' \itemize{
#'   \item `fe_target_encoding_mean()`: Each group is identified by the mean of the response over the group cases. White noise can be added via the `noise` argument. Columns encoded with this function are identified by the suffix "__encoded_mean". If `noise` is used, then the amount of noise is also added to the suffix.
#'   \item `fe_target_encoding_rank()`: Each group is identified by the rank of the mean of the response variable over the group cases. The group with the lower mean receives the rank 1. White noise can be added via the `noise` argument. Columns encoded with this function are identified by the suffix "__encoded_rank". If `noise` is used, then the amount of noise is also added to the suffix.
#'   \item `fe_target_encoding_rnorm()`: Each case in a group receives a value coming from a normal distribution with the mean and the standard deviation of the response over the cases of the group. The argument `sd.width` multiplies the standard deviation to reduce the spread of the obtained values. Columns encoded with this function are identified by the suffix "__encoded_rnorm_sd.width_X", where X is the amount of `sd.width` used.
#'   \item `fe_target_encoding_loo()`: The suffix "loo" stands for "leave-one-out". Each case in a group is encoded as the average of the response over the other cases of the group. olumns encoded with this function are identified by the suffix "__encoded_loo".
#' }
#'
#' @param df (required; data frame, tibble, or sf) A training data frame. Default: `NULL`
#' @param response (required; character string) Name of the response. Must be a column name of `df`. Default: `NULL`
#' @param categorical.variable.name (required; character) Name of the categorical variable to encode.
#' @param noise (optional; numeric) Numeric with noise values in the range 0-1. Default: `0`.
#' @param sd.width (optional; numeric) Numeric with multiplicator of the standard deviation of each group in the categorical variable, in the range 0.01-1. Default: `0.1`
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
#'   ecoregions,
#'   ecoregions_continuous_response,
#'   ecoregions_all_predictors
#' )
#'
#' #the dataframe ecoregions contains a categorical variable named primary_productivity
#' unique(ecoregions$primary_productivity)
#'
#'
#' #Group mean
#' #########################################################
#'
#' #transforming primary_productivity
#' ecoregions <- fe_target_encoding_mean(
#'   df = ecoregions,
#'   response = ecoregions_continuous_response,
#'   categorical.variable.name = "primary_productivity"
#' )
#'
#' #the encoded variable is named primary_productivity__encoded_mean
#'
#' #plotting it against the response
#' ggplot2::ggplot(data = ecoregions) +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = primary_productivity__encoded_mean,
#'     color = primary_productivity
#'   ) +
#'   ggplot2::geom_point()
#'
#'
#' #Group rank
#' #########################################################
#'
#' #transforming primary_productivity
#' ecoregions <- fe_target_encoding_rank(
#'   df = ecoregions,
#'   response = ecoregions_continuous_response,
#'   categorical.variable.name = "primary_productivity"
#' )
#'
#' #the encoded variable is named primary_productivity__encoded_rank
#'
#' #plotting it against the response
#' ggplot2::ggplot(data = ecoregions) +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = primary_productivity__encoded_rank,
#'     color = primary_productivity
#'   ) +
#'   ggplot2::geom_point()
#'
#'
#' #Leave-one-out
#' #########################################################
#' #transforming primary_productivity
#' ecoregions <- fe_target_encoding_loo(
#'   df = ecoregions,
#'   response = ecoregions_continuous_response,
#'   categorical.variable.name = "primary_productivity"
#' )
#'
#' #the encoded variable is named primary_productivity__encoded_loo
#'
#' #plotting it against the response
#' ggplot2::ggplot(data = ecoregions) +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = primary_productivity__encoded_loo,
#'     color = primary_productivity
#'   ) +
#'   ggplot2::geom_point()
#'
#'
#' #rnorm
#' #########################################################
#'
#' #transforming primary_productivity
#' ecoregions <- fe_target_encoding_rnorm(
#'   df = ecoregions,
#'   response = ecoregions_continuous_response,
#'   categorical.variable.name = "primary_productivity"
#' )
#'
#' #the encoded variable is named primary_productivity__encoded_rnorm_sd.width_0.1
#'
#' #plotting it against the response
#' ggplot2::ggplot(data = ecoregions) +
#'   ggplot2::aes(
#'     x = plant_richness,
#'     y = primary_productivity__encoded_rnorm_sd.width_0.1,
#'     color = primary_productivity
#'   ) +
#'   ggplot2::geom_point()
#'
#'
#' }
#'
#' @export
#' @autoglobal
#' @rdname target_encoding_methods
fe_target_encoding_mean <- function(
    df,
    response,
    categorical.variable.name,
    noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  #new variable name
  if(noise == 0){
    categorical.variable.new.name <- paste0(
      categorical.variable.name,
      "__encoded_mean"
    )
  } else {
    categorical.variable.new.name <- paste0(
      categorical.variable.name,
      "__encoded_mean_",
      "noise_",
      noise
    )
  }

  #aggregate by groups
  df.map <- tapply(
    X = df[[response]],
    INDEX = df[[categorical.variable.name]],
    FUN = mean,
    na.rm = TRUE
  )

  #to data frame
  df.map <- data.frame(
    names(df.map),
    df.map
    )

  #add new name
  names(df.map) <- c(
    categorical.variable.name,
    categorical.variable.new.name
  )

  #join with df
  df <- dplyr::inner_join(
    x = df,
    y = df.map,
    by = categorical.variable.name
  )

  #add noise if any
  df <- fe_target_encoding_noise(
    df = df,
    categorical.variable.name = categorical.variable.new.name,
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
    df[[categorical.variable.name]] <- NULL
    colnames(df)[colnames(df) == categorical.variable.new.name] <- categorical.variable.name
  }

  df

}


#' @rdname target_encoding_methods
#' @autoglobal
#' @export
fe_target_encoding_rnorm <- function(
    df,
    response,
    categorical.variable.name,
    sd.width = 0.1,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  if(sd.width <= 0){
    sd.width <- 0.0001
  }
  if(sd.width > 1){
    sd.width <- 1
  }


  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_rnorm_",
    "sd.width_",
    sd.width
  )

  set.seed(seed)

  #alternative without rlang:::=
  #############################################
  # Calculate the mean and standard deviation
  mean_val <- df |>
    dplyr::group_by(!!rlang::sym(categorical.variable.name)) |>
    dplyr::summarize(mean_val = mean(!!rlang::sym(response), na.rm = TRUE))

  sd_val <- df |>
    dplyr::group_by(!!rlang::sym(categorical.variable.name)) |>
    dplyr::summarize(sd_val = sd(!!rlang::sym(response), na.rm = TRUE))

  # Merge the calculated values back into 'df'
  df <- df |>
    dplyr::left_join(mean_val, by = c(categorical.variable.name = "categorical.variable.name")) |>
    dplyr::left_join(sd_val, by = c(categorical.variable.name = "categorical.variable.name"))

  # Add the new column to 'df'
  df <- df |>
    group_by(!!rlang::sym(categorical.variable.name)) |>
    mutate(!!rlang::sym(categorical.variable.new.name) := rnorm(n(), mean = mean_val, sd = sd_val * sd.width)) |>
    ungroup()
  #############################################

  #alternative with rlang:::=
  #############################################
  df <- df |>
    dplyr::group_by(.data[[categorical.variable.name]]) |>
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
        ) * sd.width
      )
    ) |>
    dplyr::ungroup()
  #############################################

  if(verbose == TRUE && replace == FALSE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  #replacing original variable with encoded version
  if(replace == TRUE){
    df[[categorical.variable.name]] <- NULL
    colnames(df)[colnames(df) == categorical.variable.new.name] <- categorical.variable.name
  }

  df

}


#' @rdname target_encoding_methods
#' @autoglobal
#' @export
fe_target_encoding_rank <- function(
    df,
    response,
    categorical.variable.name,
    noise = 0,
    seed = 1,
    replace = FALSE,
    verbose = TRUE
){

  #new variable name
  if(noise == 0){
    categorical.variable.new.name <- paste0(
      categorical.variable.name,
      "__encoded_rank"
    )
  } else {
    categorical.variable.new.name <- paste0(
      categorical.variable.name,
      "__encoded_rank_",
      "noise_",
      noise
    )
  }

  #aggregate by groups
  df.map <- tapply(
    X = df[[response]],
    INDEX = df[[categorical.variable.name]],
    FUN = mean,
    na.rm = TRUE
  ) |>
    sort()

  df.map <- data.frame(
    names(df.map),
    1:length(df.map)
  )
  names(df.map) <- c(categorical.variable.name, categorical.variable.new.name)

  #merge
  df <- dplyr::inner_join(
    x = df,
    y = df.map,
    by = categorical.variable.name
  )

  #add noise if any
  df <- fe_target_encoding_noise(
    df = df,
    categorical.variable.name = categorical.variable.new.name,
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
    df[[categorical.variable.name]] <- NULL
    colnames(df)[colnames(df) == categorical.variable.new.name] <- categorical.variable.name
  }

  df

}

#' @rdname target_encoding_methods
#' @autoglobal
#' @export
fe_target_encoding_loo <- function(
    df,
    response,
    categorical.variable.name,
    replace = FALSE,
    verbose = TRUE
){

  #new variable name
  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_loo"
  )

  #leave one out
  #by group, sum all cases of the response, subtract the value of the current row, and divide by n-1
  df <- df |>
    dplyr::group_by(.data[[categorical.variable.name]]) |>
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
    df[[categorical.variable.name]] <- NULL
    colnames(df)[colnames(df) == categorical.variable.new.name] <- categorical.variable.name
  }

  df

}

#' @rdname target_encoding_methods
#' @autoglobal
#' @export
fe_target_encoding_noise <- function(
    df,
    categorical.variable.name,
    noise = 0,
    seed = 1
){

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
  between.group.difference <- df[[categorical.variable.name]] |>
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
  df[[categorical.variable.name]] <- df[[categorical.variable.name]] +
    stats::rnorm(
      n = nrow(df)
    ) |>
    abs() |>
    rescale_vector(
      new_min = min.noise,
      new_max = max.noise
      )

  #return df
  df

}
