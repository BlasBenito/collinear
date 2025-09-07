#' @title Automated Multicollinearity Management
#' @description
#' Automated version of [collinear()], with the complete functionality but fewer arguments for fire-and-forget usage.
#'
#' Arguments \code{max_vif} and \code{max_cor} are autoconfigured depending on the average correlation between all pairs of predictors. The autoconfiguration rules are shown below:
#' \preformatted{
#'
#' max_cor <- max(
#'   0.5,
#'   correlation_mean
#' )
#'
#' if (max_cor >= 0.95) {
#'   max_vif <- 10
#' } else if (max_cor >= 0.85) {
#'   max_vif <- 7.5
#' } else if (max_cor >= 0.75) {
#'   max_vif <- 5
#' } else {
#'   max_vif <- 2.5
#' }
#' }

#' @inheritParams collinear
#'
#' @inherit collinear return
#' @autoglobal
#' @family automated_multicollinearity_analysis
#' @export
collinear_auto <- function(
    df = NULL,
    responses = NULL,
    predictors = NULL,
    quiet = FALSE
){

  function_name <- "collinear::collinear_auto()"

  # VALIDATE ARGS ----
  args <- build.collinear_arguments(
    df = df,
    responses = responses,
    predictors = predictors,
    encoding_method = "loo",
    preference_order = NULL,
    f = f_auto,
    f_name = "f_auto",
    max_cor = 0.75,
    max_vif = 5,
    quiet = quiet,
    function_name = function_name
  )

  #compute cor_stats
  if(quiet == FALSE){

    message(
      "\n",
      function_name,
      ": configuring 'max_cor' and 'max_vif'..."
    )

  }

  correlation_stats <- cor_stats(
    df = args$df,
    predictors = args$predictors,
    quiet = TRUE
  )

  correlation_mean <- correlation_stats$stats[
    correlation_stats$stats$statistic == "mean",
    "value"
    ]

  max_cor <- max(
    0.5,
    correlation_mean
  )

  if (max_cor >= 0.95) {
    max_vif <- 10
  } else if (max_cor >= 0.85) {
    max_vif <- 7.5
  } else if (max_cor >= 0.75) {
    max_vif <- 5
  } else {
    max_vif <- 2.5
  }

  args$max_cor <- validate_arg_max_cor(
    max_cor = max_cor,
    function_name = function_name,
    quiet = quiet
  )

  args$max_vif <- validate_arg_max_vif(
    max_vif = max_vif,
    function_name = function_name,
    quiet = quiet
  )

  if(quiet == FALSE){

    message(
      "Autoconfiguration finished:\n",
      "- max_vif = ", args$max_vif,
      "\n- max_cor = ", args$max_cor
    )

  }

  #manage response for loop
  if(
    length(args$responses) == 0 ||
    is.null(args$responses)
  ){
    responses <- list(NULL)
  } else {
    responses <- args$responses
  }


  #if several responses, invalidate predictors
  if(length(responses) > 1){
    attributes(args$predictors)$validated <- NULL
  }


  #ITERATION ----
  out <- list()

  ## start ----
  for(response in responses){

    ### validate response ----
    if(is.list(response)){
      response <- NULL
    }

    response <- validate_arg_response(
      df = args$df,
      response = response,
      quiet = args$quiet,
      function_name = function_name
    )

    if(
      args$quiet == FALSE &&
      length(responses) > 1
    ){

      msg <- paste0(
        function_name,
        ": processing response '",
        response,
        "'"
      )

      msg_length <- nchar(msg)

      message("\n", msg)
      message(rep(x = "-", times = nchar(msg)))

    }

    ## copy df ----
    df.response <- args$df

    ## validate predictors ----
    predictors.response <- validate_arg_predictors(
      df = df.response,
      response = response,
      predictors = args$predictors,
      function_name = function_name
    )


    ## TARGET ENCODING ----
    if(
      !is.null(response) &&
      !is.null(args$encoding_method)
    ){

      df.response <- target_encoding_lab(
        df = df.response,
        response = response,
        predictors = predictors.response,
        encoding_method = args$encoding_method,
        overwrite = TRUE,
        quiet = args$quiet
      )

    }

    ## PREFERENCE ORDER ----
    preference_order.response <- preference_order_wrapper(
      df = df.response,
      response = response,
      predictors = predictors.response,
      preference_order = args$preference_order,
      f = args$f,
      f_name = args$f_name,
      function_name = function_name,
      quiet = args$quiet
    )

    ##MULTICOLLINEARITY ANALYSIS ----

    ### empty selection ----
    selection.response <- predictors.response

    ### correlation ----
    if(!is.null(args$max_cor)){

      selection.response <- cor_select(
        df = df.response,
        predictors = predictors.response,
        preference_order = preference_order.response,
        max_cor = args$max_cor,
        quiet = args$quiet
      )

    }

    ### vif ----
    if(!is.null(args$max_vif)){

      #separate numeric and categorical
      selection.response.type <- identify_predictors(
        df = df.response,
        predictors = selection.response
      )

      selection.vif <- vif_select(
        df = df.response,
        predictors = selection.response.type$numeric,
        preference_order = preference_order.response,
        max_vif = args$max_vif,
        quiet = args$quiet
      )

      selection.response <- c(
        selection.vif,
        selection.response.type$categorical
      )

    }

    ##ORDER SELECTION ----
    if(length(selection.response) > 0){

      selection_in_preference_order <- intersect(
        x = preference_order.response$predictor,
        y = selection.response
      )

      selection_no_in_preference_order <- setdiff(
        x = selection.response,
        y = preference_order.response$predictor
      )

      selection.response <- c(
        selection_in_preference_order,
        selection_no_in_preference_order
      )

      df.response <- df.response[
        ,
        c(response, selection.response),
        drop = FALSE
      ]

      attr(
        x = df.response,
        which = "validated"
      ) <- TRUE


    } else {

      if(args$quiet == FALSE){

        message(
          "\n",
          function_name,
          ": no predictors selected."
        )

      }

      selection.response <- NULL

    }


    ## response list ----
    out.response <- build.collinear_selection(
      df = df.response,
      response = response,
      preference_order = preference_order.response,
      selection = selection.response,
      quiet = args$quiet
    )

    #store in output list
    if(is.null(response)){
      out[["result"]] <- out.response
    } else {
      out[[response]] <- out.response
    }

    # end ----
  } #end of loop

  out_list <- build.collinear_output(
    collinear_selection = out,
    collinear_arguments = args
  )

  out_list

}
