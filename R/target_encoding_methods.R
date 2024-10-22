#' Target Encoding Methods
#'
#'
#' @inheritParams target_encoding_lab
#' @param predictor (required; string) Name of the categorical predictor to encode. Default: NULL
#' @param encoded_name (required, string) Name of the encoded predictor. Default: NULL
#' @param smoothing (optional; integer) Groups smaller than this number have their means pulled towards the mean of the response across all cases. Ignored by `target_encoding_rank()` and `target_encoding_loo()`. Default: 0
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
#'   response = "vi_numeric",
#'   predictor = "soil_type",
#'   encoded_name = "soil_type_encoded"
#' )
#'
#' plot(
#'   x = df$soil_type_encoded,
#'   y = df$vi_numeric,
#'   xlab = "encoded variable",
#'   ylab = "response"
#' )
#'
#' #group rank
#' #----------
#'
#' df <- target_encoding_rank(
#'   df = vi,
#'   response = "vi_numeric",
#'   predictor = "soil_type",
#'   encoded_name = "soil_type_encoded"
#' )
#'
#' plot(
#'   x = df$soil_type_encoded,
#'   y = df$vi_numeric,
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
#'   response = "vi_numeric",
#'   predictor = "soil_type",
#'   encoded_name = "soil_type_encoded"
#' )
#'
#' plot(
#'   x = df$soil_type_encoded,
#'   y = df$vi_numeric,
#'   xlab = "encoded variable",
#'   ylab = "response"
#' )
#' @export
#' @autoglobal
#' @family target_encoding
#' @rdname target_encoding_methods
target_encoding_mean <- function(
    df = NULL,
    response = NULL,
    predictor = NULL,
    encoded_name = NULL,
    smoothing = 0
){

  if(is.null(encoded_name)){
    stop(
      "collinear::target_encoding_mean() argument 'encoded_name' is required.",
      call. = FALSE
    )
  }

  #mean encoding when smoothing > 0
  if(smoothing == 0){

    df[[encoded_name]] <- stats::ave(
      x = df[[response]],
      df[[predictor]],
      FUN = function(x) mean(x, na.rm = TRUE)
    )

  } else {

    #global response mean
    global_response_mean <- mean(
      x = df[[response]],
      na.rm = TRUE
    )

    #encoding
    df[[encoded_name]] <- stats::ave(
      x = df[[response]],
      df[[predictor]],
      FUN = function(x) {
        n <- length(x)
        mean_x <- mean(x, na.rm = TRUE)
        (n * mean_x + smoothing * global_response_mean) / (n + smoothing)
      }
    )

  }

  df

}

#' @rdname target_encoding_methods
#' @autoglobal
#' @export
target_encoding_rank <- function(
    df = NULL,
    response = NULL,
    predictor = NULL,
    encoded_name = NULL,
    smoothing = 0
){

  if(is.null(encoded_name)){
    stop(
      "collinear::target_encoding_rank() argument 'encoded_name' is required.",
      call. = FALSE
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

  #add rank column
  df.map <- data.frame(
    names(df.map),
    seq_along(df.map) #rank column
  )
  names(df.map) <- c(predictor, encoded_name)

  #column order for merged df
  df.cols <- unique(
    c(
      colnames(df),
      colnames(df.map)
    )
  )

  #merge
  df <- merge(
    x = df,
    y = df.map,
    by = predictor,
    sort = FALSE
  )

  #reorder columns
  df <- df[, df.cols]

  df

}

#' @rdname target_encoding_methods
#' @family target_encoding
#' @autoglobal
#' @export
target_encoding_loo <- function(
    df = NULL,
    response = NULL,
    predictor = NULL,
    encoded_name = NULL,
    smoothing = 0
){

  if(is.null(encoded_name)){
    stop(
      "collinear::target_encoding_loo() argument 'encoded_name' is required.",
      call. = FALSE
    )
  }

  #add id column to facilitate reordering
  df$id.. <- seq_len(nrow(df))

  #order data by predictor levels
  #to facilitate next block
  df <- df[order(df[[predictor]]), ]

  #leave one out
  #by group
  #sum all cases of the response
  #subtract the value of the current row
  #divide by n-1
  df$encoded <- unlist(
    lapply(
      X = split(
        x = df,
        f = df[[predictor]]
      ),
      FUN = function(x) {

        (
          sum(
            x = x[[response]],
            na.rm = TRUE
            ) - x[[response]]
          ) / (nrow(x) - 1)

      }
    )
  )

  #fill groups with NaN or NA with the global mean
  df[is.na(df$encoded), "encoded"] <- mean(
    x = df[[response]],
    na.rm = TRUE
  )

  df[is.nan(df$encoded), "encoded"] <- mean(
    x = df[[response]],
    na.rm = TRUE
  )

  #rename encoded column
  names(df)[names(df) == "encoded"] <- encoded_name

  df <- df[order(df[["id.."]]), ]

  df

}


#' Add White Noise to Encoded Predictor
#'
#' @description
#' Internal function to add white noise to a encoded predictor to reduce the risk of overfitting when used in a model along with the response.
#'
#' @inheritParams target_encoding_lab
#' @param predictor (required, string) Name of a target-encoded predictor. Default: NULL
#' @return data frame
#' @family target_encoding_tools
#' @export
#' @autoglobal
add_white_noise <- function(
    df = NULL,
    response = NULL,
    predictor = NULL,
    white_noise = 0,
    seed = 1
){

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


#' Name of Target-Encoded Predictor
#
#' @inheritParams target_encoding_mean
#' @param encoding_method (required, string) Name of the encoding method. One of: "mean", "rank", or "loo". Default: "mean"
#' @param white_noise (optional; numeric vector) Argument of the methods "mean", "rank", and "loo". Maximum white noise to add, expressed as a fraction of the range of the response variable. Range from 0 to 1. Default: `0`.
#' @param seed (optional; integer vector) Random seed to facilitate reproducibility when `white_noise` is not 0. If NULL, the function selects one at random, and the selected seed does not appear in the encoded variable names. Default: 0
#' @return string: predictor name
#' @export
#' @family target_encoding_tools
#' @autoglobal
encoded_predictor_name <- function(
    predictor = NULL,
    encoding_method = "mean",
    smoothing = 0,
    white_noise = 0,
    seed = 1
){

  #dev args
  # predictor <- "koppen_zone"
  # encoding_method <- "mean"
  # smoothing <- 0
  # white_noise <- 0.2
  # seed <- 2


  #name of smoothing method
  #only for "mean" and smoothing != 0
  name.smoothing <- ifelse(
    test = encoding_method == "mean" && smoothing != 0,
    yes = paste0("__smoothing_", smoothing),
    no = ""
  )

  #name for white noise
  name.noise <- ifelse(
    test = white_noise != 0,
    yes = paste0(
      "__noise_",
      white_noise
    ),
    no = ""
  )

  #name for seed
  if(!is.null(seed) && white_noise != 0){

    name.seed <- ifelse(
      test = seed != 0,
      yes = paste0(
        "__seed_",
        seed
      ),
      no = ""
    )

  } else {

    name.seed <- ""

  }


  #predictor name
  paste0(
    predictor,
    "__encoded_",
    encoding_method,
    name.smoothing,
    name.noise,
    name.seed
  )

}
