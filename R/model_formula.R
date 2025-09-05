#' Generate Model Formulas
#'
#' @inheritParams collinear
#'
#' @inheritParams f_auto
#'
#' @param term_f (optional; string). Name of function to apply to each term in the formula, such as "s" for [mgcv::s()] or any other smoothing function, "poly" for [stats::poly()]. Default: NULL
#'
#' @param term_args (optional; string). Arguments of the function applied to each term. For example, for "poly" it can be "degree = 2, raw = TRUE". Default: NULL
#'
#' @param random_effects (optional, string or character vector). Names of variables to be used as random effects. Each element is added to the final formula as \code{+(1 | random_effect_name)}. Default: NULL
#'
#' @return list if \code{predictors} is a list or length of \code{response} is higher than one, and character vector otherwise.
#' @export
#' @autoglobal
#' @examples
#'
#' data(vi, vi_predictors_numeric)
#' #using df, response, and predictors
#' #----------------------------------
#' df <- vi[1:1000, ]
#'
#' #additive formulas
#' formulas_additive <- model_formula(
#'   df = df,
#'   response = c(
#'     "vi_numeric",
#'     "vi_categorical"
#'     ),
#'   predictors = vi_predictors_numeric[1:10]
#' )
#'
#' formulas_additive
#'
#' #using a formula in a model
#' #m <- stats::lm(
#' #  formula = formulas_additive[[1]],
#' #  data = df
#' #  )
#'
#' # using output of collinear()
#' #----------------------------------
#' selection <- collinear(
#'   df = df,
#'   response = c(
#'     "vi_numeric",
#'     "vi_binomial"
#'   ),
#'   predictors = vi_predictors_numeric[1:10],
#'   quiet = TRUE
#' )
#'
#' #polynomial formulas
#' formulas_poly <- model_formula(
#'   predictors = selection,
#'   term_f = "poly",
#'   term_args = "degree = 3, raw = TRUE"
#' )
#'
#' formulas_poly
#'
#' #gam formulas
#' formulas_gam <- model_formula(
#'   predictors = selection,
#'   term_f = "s"
#' )
#'
#' formulas_gam
#'
#' #adding a random effect
#' formulas_random_effect <- model_formula(
#'   predictors = selection,
#'   random_effects = "country_name"
#' )
#'
#' formulas_random_effect
#' @family modelling_tools
model_formula <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    term_f = NULL,
    term_args = NULL,
    random_effects = NULL,
    quiet = FALSE
){

  function_name <- "collinear::model_formula()"

  quiet <- validate_arg_quiet(
    function_name = function_name,
    quiet = quiet
  )

  df <- validate_arg_df(
    df = df,
    responses = response,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  if(is.null(response)){
    stop(
      function_name,
      ": argument 'response' cannot be NULL.",
      call. = FALSE
    )
  }

  response <- validate_arg_response(
    df = df,
    response = response,
    function_name = function_name,
    quiet = quiet
  )

  if(is.null(predictors)){
    stop(
      function_name,
      ": argument 'predictors' cannot be NULL.",
      call. = FALSE
    )
  }

  predictors <- validate_arg_predictors(
    df = df,
    response = response,
    predictors = predictors,
    function_name = function_name,
    quiet = quiet
  )

  predictors_types <- identify_predictors(
    df = df,
    predictors = predictors
  )

  #prepare terms formula
  term_comma <- ","
  if(!is.null(term_f)){

    #remove (
    term_f <- gsub(
      pattern = "\\(",
      replacement = "",
      x = term_f
    )

    term_f <- paste0(term_f, "(")

    if(is.null(term_args)){

      term_args <- ")"
      term_comma <- NULL

    } else {

      term_args <- gsub(
        pattern = "\\)",
        replacement = "",
        x = term_args
      )

      term_args <- gsub(
        pattern = "^,",
        replacement = "",
        x = term_args
      )

      term_args <- paste0(term_args, ")")

    }

  } else {
    term_args <- NULL
    term_comma <- NULL
  }

  #random effects

  if(!is.null(random_effects)){

    if(!is.character(random_effects)){
      stop(
        function_name,
        ": argument 'random_effects' must be a character string or vector.",
        call. = FALSE
      )
    }

    random_effects <- setdiff(
      x = random_effects,
      y = unique(unlist(predictors))
    )

    if(length(random_effects) == 0){
      stop(
        function_name,
        ": argument 'random_effects' must name variables not in argument 'predictors'.",
        call. = FALSE
      )
    }

    random_effects <- gsub(
      pattern = "\\)",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "\\(",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "\\+",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "\\|",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "1",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "\\s",
      replacement = "",
      x = random_effects
    )

    random_effects <- paste(
      "+",
      paste(
        "(1 |",
        random_effects,
        ")",
        collapse = " + "
      )
    )

  }

  #out list
  terms <- vector()

  #iterate over responses
  for(predictor.i in predictors){

    if(predictor.i %in% predictors_types$numeric){

      predictor.i <- paste0(
        term_f,
        predictor.i,
        term_comma,
        term_args
      )

    }

    terms <- c(terms, predictor.i)

  }

  out <- paste0(
    response,
    " ~ ",
    paste0(
      terms,
      collapse = " + "
    ),
    random_effects
  ) |>
    stats::as.formula()

  environment(out) <- parent.frame()


  out

}



