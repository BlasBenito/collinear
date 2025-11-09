#' Validate Argument \code{preference_order}
#'
#' @description
#' Internal function to validate the argument \code{preference_order} in [cor_select()], [vif_select()], [collinear_select()], [collinear_auto()], and [collinear()]. Predictors not in \code{preference_order} are ranked from lower to higher sum of absolute Pearson correlations with all other predictors.
#'
#' @inheritParams collinear
#' @inheritParams validate_arg_quiet
#' @inheritParams target_encoding_lab
#'
#' @return character vector: ranked variable names
#' @export
#' @family argument_validation
#' @autoglobal
#' @examples
#' data(
#'   vi_smol,
#'   vi_predictors_numeric
#'   )
#'
#' #input arguments must be validated first
#' df <- validate_arg_df(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = vi_predictors_numeric,
#'   quiet = TRUE
#' )
#'
#' response <- validate_arg_responses(
#'   df = df,
#'   responses = "vi_numeric"
#' )
#'
#' predictors <- validate_arg_predictors(
#'   df = df,
#'   response = response,
#'   predictors = vi_predictors_numeric[1:10]
#' )
#'
#'
#'
#' #no preference order
#' #no response
#' #ranks predictor from lower to higher multicollinearity
#' y <- validate_arg_preference_order(
#'   df = df,
#'   predictors = predictors,
#'   preference_order = NULL
#' )
#'
#' y
#' attributes(y)$validated
#'
#'
#' #validate character vector
#' y <- validate_arg_preference_order(
#'   df = df,
#'   predictors = predictors,
#'   preference_order = c(
#'     "swi_max",
#'     "swi_min",
#'     "swi_deviance" #does not exist
#'   )
#' )
#'
#' y
#' attributes(y)$validated
#'
#' #validate output of preference order
#' x <- preference_order(
#'   df = df,
#'   responses = response,
#'   predictors = predictors
#' )
#'
#' x
#'
#' y <- validate_arg_preference_order(
#'   df = df,
#'   response = response,
#'   predictors = predictors,
#'   preference_order = x
#' )
#'
#' y
#' attributes(y)$validated
validate_arg_preference_order <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    preference_order = NULL,
    quiet = FALSE,
    function_name = NULL,
    ...
){

  if(
    isTRUE(attr(x = preference_order, which = "validated"))
  ){
    return(preference_order)
  }

  function_name <- validate_arg_function_name(
    default_name = "collinear::validate_arg_preference_order()",
    function_name = function_name
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  if(is.null(predictors)){

    predictors <- validate_arg_predictors(
      df = df,
      responses = response,
      predictors = predictors,
      quiet = quiet,
      function_name = function_name
    )

  }

  response <- validate_arg_responses(
    df = df,
    responses = response,
    max_responses = 1,
    quiet = quiet,
    function_name = function_name
  )

  #character
  if(is.character(preference_order)){

    #remove "auto"
    if("auto" %in% preference_order){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": value 'auto' for the argument 'preference_order' is deprecated, ignoring it."
        )

      }

      preference_order <- setdiff(
        x = preference_order,
        y = "auto"
      )

    }

    preference_order <- intersect(
      x = preference_order,
      y = predictors
    )

    preference_order <- setdiff(
      x = preference_order,
      y = response
    )

    if(length(preference_order) == 0){

      if(quiet == FALSE){

        message(
          "\n",
          function_name,
          ": invalid values in argument 'preference_order', ignoring it."
        )

      }

      preference_order <- NULL

    } else {

      #generate dataframe
      preference_order <- data.frame(
        response = rep(
          x = ifelse(
            test = is.null(response),
            yes = "none",
            no = response
          ),
          times = length(preference_order)
        ),
        predictor = preference_order,
        f = NA,
        metric = "user_preference",
        score = seq(
          from = 1,
          to = 0,
          length.out = length(preference_order)
        ),
        rank = seq_len(length(preference_order))
      )

    }

  }

  #dataframe
  if(is.data.frame(preference_order)){

    #error if not expected structure
    expected_columns <- c(
      "response",
      "predictor",
      "f",
      "metric",
      "score",
      "rank"
    )

    if(!all(expected_columns %in% colnames(preference_order))){

      stop(
        "\n",
        function_name,
        ": dataframe 'preference_order' must have these columns: '",
        paste(expected_columns, collapse = "', '"),
        "'.",
        call. = FALSE
      )

    }

    #subset preference order for the given predictors
    preference_order <- preference_order[preference_order$predictor %in% predictors, ]

    if(nrow(preference_order) == 0){

      stop(
        "\n",
        function_name,
        ": column 'preference_order$predictor' does not contain any 'predictors'.",
        call. = FALSE
      )

    }

    if(is.null(response)){

      unique_df_response <- unique(preference_order$response)

      if(length(unique_df_response) > 1){

        stop(
          "\n",
          function_name,
          ": dataframe 'preference_order' contains more than one response and there is no valid 'response' argument to filter it.",
          call. = FALSE
        )

      }

    } else {


      if(response %in% preference_order$response){

        preference_order <- preference_order[preference_order$response == response, ]

      } else {

        stop(
          "\n",
          function_name,
          ": argument 'response' does not match the column 'response' of the dataframe 'preference_order'",
          call. = FALSE
        )

      }


    }

  }

  if(is.null(preference_order)){

    #generate empty dataframe
    preference_order <- data.frame(
      response = ifelse(
        test = is.null(response),
        yes = "none",
        no = response
      ),
      predictor = NA,
      f = NA,
      metric = NA,
      score = NA,
      rank = NA
    ) |>
      stats::na.omit()

  }


  #order missing predictors by their multicollinearity
  if(nrow(preference_order) < length(predictors)){

    predictors_missing <- setdiff(
      x = predictors,
      y = preference_order$predictor
    )


    if(length(predictors_missing) == 1){

      preference_order_default <- data.frame(
        response = ifelse(
          test = is.null(response),
          yes = "none",
          no = response
        ),
        predictor = predictors_missing,
        f = NA,
        metric = "user_preference",
        score = 0,
        rank = nrow(preference_order) + 1
      )

    } else if(length(predictors_missing) > 1){

      preference_order_default <- preference_order(
        df = df,
        responses = response,
        predictors = predictors_missing,
        f = NULL,
        quiet = quiet,
        function_name = function_name
      )

    }

    preference_order <- rbind(
      preference_order,
      preference_order_default
    )

  }

  rownames(preference_order) <- NULL

  preference_order$rank <- seq_len(length.out = nrow(preference_order))

  attr(
    x = preference_order,
    which = "validated"
  ) <- TRUE

  preference_order

}
