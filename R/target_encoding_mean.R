#' Target Encoding Methods
#'
#'
#' @inheritParams target_encoding_lab
#' @param predictor (required; string) Name of the categorical predictor to encode. Default: NULL
#' @param encoded_name (optional, string) Name of the encoded predictor. Default: NULL
#' @param smoothing (optional; integer) Groups smaller than this number have their means pulled towards the mean of the response across all cases. Ignored by [target_encoding_rank()] and [target_encoding_loo()]. Default: 0
#'
#'
#' @inherit target_encoding_lab return
#'
#' @examples
#'
#' #  loading example data
#' data(vi_smol)
#'
#' #mean encoding
#' #-------------
#'
#' df <- target_encoding_mean(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictor = "soil_type", #categorical
#'   encoded_name = "soil_type_encoded"
#' )
#'
#' if(interactive()){
#'
#'   plot(
#'     x = df$soil_type_encoded,
#'     y = df$vi_numeric,
#'     xlab = "encoded variable",
#'     ylab = "response"
#'   )
#'
#' }
#'
#'
#' #rank encoding
#' #----------
#'
#' df <- target_encoding_rank(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictor = "soil_type",
#'   encoded_name = "soil_type_encoded"
#' )
#'
#' if(interactive()){
#'
#'   plot(
#'     x = df$soil_type_encoded,
#'     y = df$vi_numeric,
#'     xlab = "encoded variable",
#'     ylab = "response"
#'   )
#'
#' }
#'
#'
#' #leave-one-out
#' #-------------
#'
#' df <- target_encoding_loo(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictor = "soil_type",
#'   encoded_name = "soil_type_encoded"
#' )
#'
#' if(interactive()){
#'
#'   plot(
#'     x = df$soil_type_encoded,
#'     y = df$vi_numeric,
#'     xlab = "encoded variable",
#'     ylab = "response"
#'   )
#'
#' }
#'
#' @export
#' @autoglobal
#' @family target_encoding
#' @rdname target_encoding_methods
target_encoding_mean <- function(
    df = NULL,
    response = NULL,
    predictor = NULL,
    encoded_name = NULL,
    smoothing = 0,
    ...
){

  if(is.null(encoded_name)){
    encoded_name <- paste0(
      predictor,
      "__encoded"
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
