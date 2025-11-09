#' @rdname target_encoding_methods
#' @autoglobal
#' @export
target_encoding_rank <- function(
    df = NULL,
    response = NULL,
    predictor = NULL,
    encoded_name = NULL,
    smoothing = NULL,
    ...
){

  if(is.null(encoded_name)){
    encoded_name <- paste0(
      predictor,
      "__encoded"
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
