#' @rdname target_encoding_methods
#' @family target_encoding
#' @autoglobal
#' @export
target_encoding_loo <- function(
  df = NULL,
  response = NULL,
  predictor = NULL,
  encoded_name = NULL,
  smoothing = NULL,
  ...
) {
  if (is.null(encoded_name)) {
    encoded_name <- paste0(
      predictor,
      "__encoded"
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
        (sum(
          x = x[[response]],
          na.rm = TRUE
        ) -
          x[[response]]) /
          (nrow(x) - 1)
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
