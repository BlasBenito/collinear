library(collinear)
data(vi)

df = vi
response = "vi_mean"
predictor = "soil_type"
replace = TRUE
m = 30

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
